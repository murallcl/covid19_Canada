# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#     R0 estimation for Canadian data, COVID-19 2020 - using serial interval
#
# Carmen Lia Murall, Universtite de Montreal, Mar 29, 2020
# following lead by ETE team, CNRS, Montpellier http://alizon.ouvaton.org./Report1_R0_France.html

# ------- Datasets

#populations sizes
populations <- c(Canada=37894799,Montreal=4350817,Toronto=6522989)

setwd("C:/Users/carme/Dropbox/Work_Files/R/COVID-19/shared")

# National
datCA<-read.csv(file="covid_CDN_Mar25_PHAC.csv",header=T)
colnames(datCA)<-c("date","cases","deaths")
datCA$date<-as.Date(datCA$date)
datCA$day<-seq(1,length(datCA$date))
#View(datCA)

# Montreal
dataMTL<-read.csv(file="covid_CDN_Mar29_MTL_trim.csv",header=T)
#cases
casesbyDate<-data.frame( tapply(dataMTL$Case, dataMTL$Date, FUN=sum)) #sum cases by date
colnames(casesbyDate)<-c("Cases")
casesbyDate$Date <- row.names(casesbyDate)
rownames(casesbyDate) <- NULL
casesbyDate$Date<-paste0("2020-", casesbyDate$Date)
casesbyDate$Date<-as.Date(casesbyDate$Date, format="%Y-%d-%b")
df<-na.omit(arrange(casesbyDate, Date)) #arrange by date
#deaths
df2<-data.frame(tapply(dataMTL$Death, dataMTL$Date, FUN=sum))
colnames(df2)<-c("Deaths")
df2$Date <- row.names(df2)
rownames(df2) <- NULL
df2$Date<-paste0("2020-", df2$Date)
df2$Date<-as.Date(df2$Date, format="%Y-%d-%b")
df2<-na.omit(arrange(df2, Date)) #arrange by date
df$Deaths<-df2$Deaths #add deaths to cases df
datMTL <-df

# Toronto
df<-data.frame()
df2<-data.frame()
dataTO<-read.csv(file="covid_CDN_Mar27_TO_trim.csv",header=T)

casesbyDate<-data.frame( tapply(dataTO$Case, dataTO$Date, FUN=sum)) #sum cases by date
colnames(casesbyDate)<-c("Cases")
casesbyDate$Date <- row.names(casesbyDate)
rownames(casesbyDate) <- NULL
casesbyDate$Date<-paste0("2020-", casesbyDate$Date)
casesbyDate$Date<-as.Date(casesbyDate$Date, format="%Y-%d-%b")
df<-na.omit(arrange(casesbyDate, Date)) #arrange by date

df2<-data.frame(tapply(dataTO$Death, dataTO$Date, FUN=sum))
colnames(df2)<-c("Deaths")
df2$Date <- row.names(df2)
rownames(df2) <- NULL
df2$Date<-paste0("2020-", df2$Date)
df2$Date<-as.Date(df2$Date, format="%Y-%d-%b")
df2<-na.omit(arrange(df2, Date)) #arrange by date
df$Deaths<-df2$Deaths #add deaths to cases df
datTO <-df

# ------- Libraries
library(R0)
library(tidyverse)
library(gridExtra)

#-------------- generation time distribution used for estimation - COVID-19 specific
mGT<-generation.time("lognormal", c(4.7, 2.9), truncate = 20) 
#use serial interval: most certain 4.7 days [3.5, 5.9] , SD: 2.9 days [1.9,4.9] Nishiura et al 2020
#truncation from code, xmax=20


#-------------- Canada estimate - various methods
end<-as.numeric(dim(datCA)[[1]]) #no of rows
est_R0CA<-estimate.R(as.numeric(datCA$cases[8:end]), mGT, #start at initial case
          methods=c("EG", "ML", "TD", "SB"), 
          pop.size=populations[["Canada"]], nsim=100)
est_R0CA

# time varying
TDCA<-est.R0.TD(as.numeric(datCA$cases[8:end]), mGT, t=as.Date(datCA$date[8:end]), nsim=10000)
RtCA<-TDCA$R 
RtCA<-TDCA$R 
CA_CI_low <-TDCA$conf.int$lower
CA_CI_upp <-TDCA$conf.int$upper
RtCA<-data.frame(TDCA$R)
RtCA$date <- row.names(RtCA)
rownames(RtCA) <- NULL
RtCA$CIlow<-CA_CI_low
RtCA$CIupp<-CA_CI_upp



#-------------- Montreal estimate - various methods
#end2<-as.numeric(dim(datMTL)[[1]]) #no of rows
est_R0MTL<-estimate.R(as.numeric(datMTL$Cases), mGT, #start at initial case
                   methods=c("EG", "ML", "TD", "SB"), 
                   pop.size=populations[["Montreal"]], nsim=100)
est_R0MTL

# time varying <- given the spike in cases (due to testing increase) this makes more sense the the overall R0
TDMTL<-est.R0.TD(as.numeric(datMTL$Cases), mGT, t=as.Date(datMTL$Date), nsim=500)
RtMTL<-TDMTL$R 
MTL_CI_low <-TDMTL$conf.int$lower
MTL_CI_upp <-TDMTL$conf.int$upper
RtMTL<-data.frame(TDMTL$R)
RtMTL$date <- row.names(RtMTL)
rownames(RtMTL) <- NULL
RtMTL$CIlow<-MTL_CI_low
RtMTL$CIupp<-MTL_CI_upp


#-------------- Toronto estimate - various methods
#end3<-as.numeric(dim(datTO)[[1]]) #no of rows
mGT1<-generation.time("lognormal", c(4.7, 2.9),truncate=30) #data requires higher truncation

est_R0TO<-estimate.R(as.numeric(datTO$Cases), mGT1, #start at initial case
                      methods=c("EG", "ML", "TD", "SB"), 
                      pop.size=populations[["Toronto"]], nsim=100)
est_R0TO

# time varying 
TDTO<-est.R0.TD(as.numeric(datTO$Cases), mGT1, t=as.Date(datTO$Date), nsim=500)
RtTO<-TDTO$R 
TO_CI_low <-TDTO$conf.int$lower
TO_CI_upp <-TDTO$conf.int$upper
RtTO<-data.frame(TDTO$R)
RtTO$date <- row.names(RtTO)
rownames(RtTO) <- NULL
RtTO$CIlow<-TO_CI_low
RtTO$CIupp<-TO_CI_upp


#----- -------------------------plots

# 1. Overall R0 - point plot (Exp, ML estimates)
location<-c("CA","CA", "MTL","MTL","TO","TO")
method<-c("Exp","ML","Exp","ML","Exp","ML")
R0est<-c(est_R0CA$estimates$EG$R, est_R0CA$estimates$ML$R,est_R0MTL$estimates$EG$R, est_R0MTL$estimates$ML$R,est_R0TO$estimates$EG$R, est_R0TO$estimates$ML$R)
low<-c(est_R0CA$estimates$EG$conf.int[1],est_R0CA$estimates$ML$conf.int[1],est_R0MTL$estimates$EG$conf.int[1],est_R0MTL$estimates$ML$conf.int[1],est_R0TO$estimates$EG$conf.int[1],est_R0TO$estimates$ML$conf.int[1])
upp<-c(est_R0CA$estimates$EG$conf.int[2],est_R0CA$estimates$ML$conf.int[2],est_R0MTL$estimates$EG$conf.int[2],est_R0MTL$estimates$ML$conf.int[2],est_R0TO$estimates$EG$conf.int[2],est_R0TO$estimates$ML$conf.int[2])
datR0 <-data.frame(location,method,R0est,low,upp)
pp<-ggplot(datR0, aes(x=location, y=R0est, color=method))+geom_point(size=4)+
  geom_hline(yintercept=1, linetype= "dashed")+ ylim(0,4)+
  ylab("R0")+theme_classic()
pp + geom_pointrange(aes(x=location, ymin=low, ymax=upp))



# 2. R(t) by dates:
p<-list()
#CA:
pCA<-ggplot(data=RtCA, aes(x=as.Date(date), y=TDCA.R))+geom_line()+
  ylab("R(t)")+ xlab("date")+ 
  ggtitle("Canada 2020")+
  geom_hline(yintercept=1, linetype= "dashed")+
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d")
pCA  +  geom_ribbon(aes(x=as.Date(date), ymin=CIlow, ymax=CIupp), alpha = 0.3) ->p[[1]]

# MTL line plot
pMTL<-ggplot(data=RtMTL, aes(x=as.Date(date), y=TDMTL.R))+geom_line()+
  ylab("R(t)")+ xlab("date")+ ylim(0,20)+
  ggtitle("Montreal 2020")+
  geom_hline(yintercept=1, linetype= "dashed")+
  scale_x_date(date_breaks = "2 day", date_labels = "%m-%d")
pMTL  +  geom_ribbon(aes(x=as.Date(date), ymin=CIlow, ymax=CIupp), alpha = 0.3) ->p[[2]]

# TO line plot
pTO<-ggplot(data=RtTO, aes(x=as.Date(date), y=TDTO.R))+geom_line()+
  ylab("R(t)")+ xlab("date")+ 
  ggtitle("Toronto 2020")+
  geom_hline(yintercept=1, linetype= "dashed")+
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d")
pTO  + geom_ribbon(aes(x=as.Date(date), ymin=CIlow, ymax=CIupp), alpha = 0.3) ->p[[3]]


grid.arrange(grobs=p, top="COVID-19: Effective reproductive number estimation, temporal variation")
  
