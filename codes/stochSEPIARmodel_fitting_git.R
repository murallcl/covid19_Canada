#
# COVID-19 MODELLING :  stochastic SEPIAR MODEL with Canadian data (FITTING)

# w/ pre- and asymptomatic cases

# Carmen Lia Murall
# using: Forecasting Ebola case study: pomp codes (AAKing)
# example codes: https://kingaa.github.io/sbied/pfilter/pf-in-Nelder-Mead.html

#-------------------- parameter estimation/fitting (trajectory matching) with pomp -------------

rm(list = ls())

#libraries
library(magrittr)
library(pomp)
library(tidyverse)
library(tictoc)
library(deSolve)
library(gridExtra)
theme_set(theme_bw())
options(stringsAsFactors=FALSE)


setwd("C:/Users/carme/Dropbox/Work_Files/R/COVID-19/shared")

#canadian nationwide data
dat1<-read.csv(file="covid_CDN_Mar25_PHAC.csv",header=T)
colnames(dat1)<-c("date","cases","deaths")
dat1$date<-as.Date(dat1$date)
dat1$day<-seq(1,length(dat1$date))
#View(dat1)

#populations sizes
populations <- c(Canada=37894799,Montreal=4350817,Toronto=6522989)

bl <- 5 #five day backlog (last 5 days less certain/accurate)
end <-length(dat1$date)-bl

dat2<-slice(dat1, 1:end) #remove last dates that are less reliable

#plot data
dat2 %>%
  ggplot()+  geom_point(aes(x=date, y=cases, color="cases"),size=2)+  
  geom_point(aes(x=date, y=deaths, color="deaths"),size=2)+
  scale_color_manual(values=c("cases"="black","deaths"="grey60"))+
  ylab("incidence") +  theme(legend.title = element_blank())


# SEIR model with pre-symptomatic and asymptomatics
#-----------

#S -> E -> P -> I -> R or D
#and
#S -> E -> A -> R 

#deterministic skeleton
skel <- Csnippet("
  double lambdai, lambdap, lambdaa, betai, betap, betaa;
  betai = R0i * gammai; // Transmission rate for I
  betap = R0p * gammai; // Transmission rate for P
  betaa = R0a * gammaa; // Transmission rate for A
  lambdai = betai * I / N; // Force of infection
  lambdap = betap * P / N; // Force of infection
  lambdaa = betaa * A / N; // Force of infection
  

  // Balance the equations
  DS = - lambdai * S - lambdap * S- lambdaa * S;
  DE = (lambdai + lambdap + lambdaa) * S - sigma * E;
  DP = nu * sigma * E - zeta * P;
  DI = zeta * P - gammai * I;
  DA = (1-nu) * sigma * E - gammaa * A;
  DR = (1-f) * gammai * I + gammaa * A;
  DD = f * gammai * I;
  DN_I = zeta * P + (1-nu) * sigma * E;
  DN_R = (1-f) * gammai * I + gammaa * A;
")



# process model:
rSim <- Csnippet("
  double lambdai, lambdap, lambdaa, betai, betap, betaa;
  betai = R0i * gammai; // Transmission rate for I
  betap = R0p * gammai; // Transmission rate for P
  betaa = R0a * gammaa; // Transmission rate for A
  lambdai = betai * I / N; // Force of infection
  lambdap = betap * P / N; // Force of infection
  lambdaa = betaa * A / N; // Force of infection

  // Transitions
  double dN_SE  = rbinom(S, 1.0 - exp(- (lambdai + lambdap + lambdaa) * dt)); // No of infections
  double dN_Eo  = rbinom(E, 1.0 - exp(- sigma * dt)); // No of transitions out of E 
  double dN_EP  = rbinom(E, 1.0 - exp(- nu * sigma * dt)); // No of transitions E->P
  double dN_EA  = rbinom(E, 1.0 - exp(- (1-nu) * sigma * dt)); // No of transitions E->A
  double dN_PI  = rbinom(P, 1.0 - exp(- zeta * dt)); // No of transitions P->I
  double dN_Io  = rbinom(I, 1.0 - exp(- gammai * dt)); // No of transitions out of I
  double dN_IR  = rbinom(I, 1.0 - exp(- (1-f) * gammai * dt)); // No of transitions I->R
  double dN_AR  = rbinom(A, 1.0 - exp(- gammaa * dt)); // No of transitions A->R
  double dN_ID  = rbinom(I, 1.0 - exp(- f * gammai * dt)); // No of transitions I->D

  
  // Balance the equations
  S -= - dN_SE;   
  E += dN_SE - dN_Eo;
  P += dN_EP - dN_PI;
  I += dN_PI - dN_Io;
  A += dN_EA - dN_AR;
  R += dN_IR + dN_AR;
  D += dN_ID;
  N_I += dN_EP + dN_EA + dN_PI; // No of transitions from P to I and E to A - use for number of infected
  N_R += dN_IR + dN_AR; // No of transitions from I and A to R - use for number of recovered
  N_D += dN_ID; // No of transitions from I to D
")

#initial contions 
rInit <- Csnippet("
  S = 37894799;
  E = 0;
  P = 0;
  I = 1;
  A = 0;
  R = 0;
  D = 0;
  N_I = 0;
  N_R = 0;
  N_D =0;
")




#if missing data
dObs_miss <- Csnippet("
  if (ISNA(B)) {
    lik = (give_log) ? 0 : 1;
  } else {
    dbinom(nearbyint(cases), N_I, rho, give_log),
    dbinom(nearbyint(deaths),N_D, rho, give_log);
  }")


#assume overdispersion:
dObs <- Csnippet("
                 lik = dbinom(nearbyint(cases), N_I, rho, give_log),
                       dbinom(nearbyint(deaths),N_D, rho, give_log);
                 ")

rObs <- Csnippet("
                 cases = rbinom(N_I, rho),
                 deaths = rbinom(N_D, rho); ")

#not overdispersed:
dObs_pois <- Csnippet(" double ff;
                 ff = dpois(nearbyint(cases),rho*N_I,1),
                       dpois(nearbyint(deaths),rho*N_D,1);
                 lik = (give_log) ? ff : exp(ff);
                 ")

rObs_pois <- Csnippet("
                 cases = rpois(rho*N_I),
                 deaths = rpois(rho*N_D); ")



# pomp 
## Create the pomp object
dat2 %>%
  #select(day,cases) %>%
  pomp(
    times="day",
    t0=min(dat2$day)-1,
    accumvars=c("N_I","N_R","N_D"),
    statenames=c("S", "E", "P" ,"I", "A","R", "D","N_I","N_R","N_D"),
    paramnames=c("N","R0i","R0p","R0a","sigma","zeta","f", "nu", 
                 "gammai", "gammaa","rho"),
    dmeasure=dObs_pois,  
    rmeasure=rObs_pois,
    rprocess=discrete_time(step.fun=rSim, delta.t=1),
    skeleton=vectorfield(skel),
    partrans=parameter_trans(
      log=c("R0i","R0p","R0a"),logit=c("nu","rho","gammaa")),
    rinit=rInit,
    params=c(N=populations[["Canada"]], R0p=1.8, R0i=2.2, R0a=2.0,
             sigma=1/4.1, zeta=1/1, gammai=1/14, gammaa=1/2.9,
             nu=0.5, f=0.035, rho=0.5)
  ) -> po



#parameter estimates in the pomp
coef(po)

#simulate model with specific parameters (not fit to data)
po %>%
  simulate(nsim=30, format="data.frame", include.data=TRUE) -> sims

simssim<-filter(sims, .id!="data")#sim only
simsdat<-filter(sims, .id=="data")#dat only

plt1<-list()

ggplot()+
  geom_line(data=simssim, aes(x=day, y=cases, group=.id, color=.id=="data"))+
  geom_line(data=simsdat, aes(x=day, y=cases, group=.id, color=.id=="data"))+
  guides(color=FALSE)+
  geom_line(alpha = .1, size= 2)+
  scale_color_manual(values=c("grey80","black"))->plt1[[1]]

ggplot()+
  geom_line(data=simssim, aes(x=day, y=deaths, group=.id, color=.id=="data"))+
  geom_line(data=simsdat, aes(x=day, y=deaths, group=.id, color=.id=="data"))+
  guides(color=FALSE)+
  geom_line(alpha = .1, size= 2)+
  scale_color_manual(values=c("grey80","black"))->plt1[[2]]

grid.arrange(grobs=plt1, top="Stochastic SEPIAR model NOT fit to Canadian national data, Mar 25, 2020")



#single particle filter
require(pomp)
pf <- pfilter(po, Np=1000)
logLik(pf)
plot(pf)

#function that returns neg loglik of data at a given point in parameter space
#est = parameters to be estimated
#freeze - sets a seed

neg.ll <- function (par, est) {
  try(
    freeze({
      allpars <- coef(po,transform=TRUE)
      allpars[est] <- par
      theta <- partrans(po,allpars,dir="fromEst")
      pfilter(po,params=theta,Np=2000)
    },
    seed=915909831
    )
  ) -> pf
  if (inherits(pf,"try-error")) 1e10 else -logLik(pf)
}

estpars <- c("R0p","R0i","R0a","nu","rho","gammaa") #params to estimate

#optim minimizes neg. loglik function:
optim(
  par=coef(po,estpars,transform=TRUE),
  est=estpars,
  fn=neg.ll,
  method="Nelder-Mead",
  control=list(maxit=400,trace=0)
) -> fit

mle <- po
coef(mle,estpars,transform=TRUE) <- fit$par #change po to have fitted parameter estimates
coef(mle) #new parameter values

fit$val 
lls <- replicate(n=10,logLik(pfilter(mle,Np=20000)))
ll <- logmeanexp(lls,se=TRUE); ll



#calculate R0 (from next-gen) based on the chosen parameters:
pars <-coef(mle)
betai <- pars[["R0i"]] * pars[["gammai"]] # Transmission rate for I
betap <- pars[["R0p"]] * pars[["gammai"]] # Transmission rate for P
betaa <- pars[["R0a"]] * pars[["gammaa"]] # Transmission rate for A
gammaa <- pars[["gammaa"]]
gammai <- pars[["gammai"]]
zeta <- pars[["zeta"]]
nu <- pars[["nu"]]
S0 <- populations[["Canada"]]
R0 <- (betap*gammaa*gammai*nu + betaa*gammai*zeta + betai*gammaa*nu*zeta - betaa*gammai*nu*zeta)/(gammaa*gammai*zeta) ;R0

R0lab<-paste0("R0 = ", round(R0,2)) #create label

#simulate some runs using new parameters:
mle %>% simulate(nsim=30,format="data.frame",include.data=TRUE) -> sims2

sim2sim<-filter(sims2, .id!="data")#sim only
sim2dat<-filter(sims2, .id=="data")#dat only

plt<-list()
ggplot()+
  geom_line(data=sim2sim, aes(x=day, y=cases, group=.id, color=.id=="data"))+
  geom_line(data=sim2dat, aes(x=day, y=cases, group=.id, color=.id=="data"))+
  guides(color=FALSE)+
  geom_line(alpha = .2, size= 3)+
  scale_color_manual(values=c("grey75","black"))+ annotate("text",x=5, y=250, label=R0lab)+
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45,vjust=0.9, hjust=0.9))+
  scale_x_continuous(breaks=seq(0,60,20),
                   labels=c(as.character(dat2$date[[1]]),as.character(dat2$date[[20]]),
                            as.character(dat2$date[[40]]),as.character(dat2$date[[60]])))->plt[[1]]

ggplot()+
  geom_line(data=sim2sim, aes(x=day, y=deaths, group=.id, color=.id=="data"))+
  geom_line(data=sim2dat, aes(x=day, y=deaths, group=.id, color=.id=="data"))+
  guides(color=FALSE)+
  geom_line(alpha = .1, size= 3)+
  scale_color_manual(values=c("grey75","black"))+
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45,vjust=0.9, hjust=0.9))+
  scale_x_continuous(breaks=seq(0,60,20),
                     labels=c(as.character(dat2$date[[1]]),as.character(dat2$date[[20]]),
                              as.character(dat2$date[[40]]),as.character(dat2$date[[60]])))->plt[[2]]

grid.arrange(grobs=plt, top="Stochastic SEPIAR model fit to Canadian national data, Mar 25, 2020")





#-------------------------

#expand search to do GLOBAL MAXIMIZATION:

#https://kingaa.github.io/sbied/pfilter/pfilter.html (see likelihood surface section)

# 1. slices of likelihood per fitted parameter:
int<-20
rep<-3
sliceDesign(
  center=coef(po),
  R0i=rep(seq(from=0.01,to=6,length=int),each=rep),
  R0a=rep(seq(from=0.01,to=5,length=int),each=rep),
  R0p=rep(seq(from=0.01,to=6,length=int),each=rep),
  gammaa=rep(seq(from=0,to=1,length=int),each=rep),
  nu=rep(seq(from=0,to=1,length=int),each=rep)
) -> p

library(foreach)
library(doParallel)
library(doRNG)

registerDoParallel()
registerDoRNG(473910809)

foreach (theta=iter(p,"row"),
         .combine=rbind,.inorder=FALSE) %dopar% {
           library(pomp)
           
           po %>% pfilter(params=theta,Np=5000) -> pf
           
           theta$loglik <- logLik(pf)
           theta
         } -> p

plot(p) #gives all slices


#plot specific slices 
library(tidyverse)

p %>% 
  gather(variable,value,R0p,R0i,R0a,gammaa,nu) %>%
  filter(variable==slice) %>%
  ggplot(aes(x=value,y=loglik,color=variable))+
  geom_point()+
  facet_grid(~variable,scales="free_x")+
  guides(color=FALSE)+
  labs(x="parameter value",color="")+
  theme_bw()
