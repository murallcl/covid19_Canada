#                         COVID-19 - Canadian data - Provinces - data plots

# Carmen Lia Murall, Apr 2020 

#libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)

#load data
setwd("C:/Users/carme/Dropbox/Work_Files/R/COVID-19/shared")


bl<-3 #blacklogged days, uncertainty of numbers
bl2<-5 #days 


# ------------------ DATA ---------------------------------------

# NATIONAL

dataCA<-read.csv(file="covid19_PHAC_Apr4.csv",header=T) #data from PHAC website

dataCA<-filter(dataCA, dataCA$prname=="Canada")
dataCA$date<-as.Date(dataCA$date, format="%d-%m-%Y")

# QUEBEC

#cases
dataQC<-read.csv(file="scrapped_data_QC_Apr2_cases.csv",header=T) #data from git of Prof. Bhatnagar (McGill biostats)
colnames(dataQC)<-c("id","date","type","cum.cases")
dataQC<-filter(dataQC, type=="Nombre cumulatif de cas") #select only cum cases
#calculate incidence cases
dataQC$inc.cases <-c(dataQC$cum.cases[1],diff(dataQC$cum.cases))
dataQC$date<-as.Date(dataQC$date, format="%Y-%m-%d") #transform into date format

#deaths
deathsQC<-read.csv(file="scrapped_data_QC_Apr2_deaths.csv",header=T) #data from git Prof. Bhatnagar (McGill biostats)
colnames(deathsQC)<-c("id","date","type","cum.deaths")
deathsQC<-filter(deathsQC, type=="Nombre cumulatif de deces") #select only cum cases
#calculate incidence deaths
deathsQC$inc.deaths <-c(deathsQC$cum.deaths[1],diff(deathsQC$cum.deaths))
deathsQC$date<-as.Date(deathsQC$date, format="%Y-%m-%d") #transform into date format

# ONTARIO

dataON<-read.csv(file="ON_covid_cases_Apr1_trim.csv",header=T) #data from ON gov website
#cases
dataON$case<-rep(1,length(dataON$date))
dfON<-data.frame( tapply(dataON$case, dataON$date, FUN=sum)) #sum cases by date
colnames(dfON)<-c("cases")
dfON$date <- as.Date(row.names(dfON),format="%Y-%m-%d")
rownames(dfON) <- NULL
#deaths
dON<-filter(dataON, outcome=="FATAL")
deathsON<-data.frame( tapply(dON$case, dON$date, FUN=sum)) 
colnames(deathsON)<-c("deaths")
deathsON$date <- as.Date(row.names(deathsON),format="%Y-%m-%d")
rownames(deathsON) <- NULL
deathsON$deaths[is.na(deathsON$deaths)]<-0 #make NAs into 0 (needed for cumsum)

#recovered
rON<-filter(dataON, outcome=="RECOVERED")
recON<-data.frame( tapply(rON$case, rON$date, FUN=sum)) 
colnames(recON)<-c("recovered")
recON$date <- as.Date(row.names(recON),format="%Y-%m-%d")
rownames(recON) <- NULL
#travel acquired
trON<-filter(dataON, case.acquisition=="Travel-Related")
travON<-data.frame( tapply(trON$case, trON$date, FUN=sum)) 
colnames(travON)<-c("travel.acq")
travON$date <- as.Date(row.names(travON),format="%Y-%m-%d")
rownames(travON) <- NULL

#merge all three into one dataset
dfON %>% select(date, everything())->dfON #reorder columns
left_join(dfON,deathsON, by=c("date")) ->dfON #merge deaths
left_join(dfON,recON, by=c("date")) ->dfON #merge recovered
left_join(dfON,travON, by=c("date")) ->dfON #merge travel



# ------------------ PLOTS ---------------------------------------

# ------------------ Nation wide ---------------------------------------

pl<-list()

#plot incidence cases
pl[[1]]<-ggplot() +
  geom_point(data=dataCA, aes(x=date, y=numconf, color="cases"), size=3)+
  geom_point(data=dataCA, aes(x=date, y=numdeaths, color="deaths"), size=3)+ 
  geom_line(data=dataCA, aes(x=date, y=numconf, color="cases"))+
  geom_line(data=dataCA, aes(x=date, y=numdeaths, color="deaths"))+
  ylab("Incidence")+
  theme_classic()+
  scale_colour_manual(breaks=c("cases","deaths"),values = c(cases="firebrick", deaths="black"))+
  theme(legend.title = element_blank(),
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal')+
  geom_rect(aes(xmin = dataCA$date[length(dataCA$date)-bl], xmax = dataCA$date[length(dataCA$date)], 
                ymin = -Inf, ymax = Inf), alpha = 0.2) 

#logplot cumm cases
pl[[2]]<-ggplot(dataCA, aes(x=date, y=cumsum(numconf)) ) +
  geom_point(size=3)+
  scale_y_continuous(trans='log2')+
  theme_classic()+
  ylab("Cummulative cases")+
  geom_smooth(method=lm, se=FALSE, color="firebrick")+
  geom_vline(xintercept =as.Date("2020-02-27"), linetype="dashed")+
  annotate(geom="text",x=as.Date("2020-02-27"), y=2000,label="community spread begins", angle=90)

#plot cummulative deaths - histogram
end<-which(dataCA$date=="2020-03-31")
pl[[3]]<-ggplot()+
  geom_histogram(aes(x=dataCA$date[1:end], y=dataCA$numdeaths[1:end],color="CA"), fill="white", stat = "identity" ) +
  geom_histogram(data=dfON, aes(x=date, y=cumsum(deaths),color="ON"), fill="white",alpha=0.1, stat = "identity") +
  geom_histogram(data=deathsQC, aes(x=date, y=cum.deaths,color="QC"), fill="white",alpha=0.1, stat = "identity") +
  ylab("Cummulative deaths")+ xlab("date")+
  theme_classic()+
  scale_x_date(date_breaks = "7 day", date_labels = "%d-%m")+
  theme(legend.title = element_blank())+
  scale_colour_manual(breaks=c("CA", "ON" ,"QC"),values = c(CA="black", ON="darkblue", QC = "firebrick"))#+
#scale_fill_manual(values =c("lightgrey", "lightblue", "firebrick1"))


grid.arrange(grobs=pl, nrow=1, top="Canada COVID-19, Apr 4, 2020")



#------------------- QUEBEC -----------------------------------------------------------


p<-list() 

#plot incidence cases
p[[1]]<-ggplot() +
  geom_point(data=dataQC, aes(x=date, y=inc.cases, color="cases"), size=3)+
  geom_point(data=deathsQC, aes(x=date, y=inc.deaths, color="deaths"), size=3)+ 
  geom_line(data=dataQC, aes(x=date, y=inc.cases, color="cases"))+
  geom_line(data=deathsQC, aes(x=date, y=inc.deaths, color="deaths"))+ 
  ylab("Incidence")+
  theme_classic()+
  scale_colour_manual(breaks=c("cases","deaths"),
                      values = c(cases="firebrick", deaths="black"))+
  theme(legend.title = element_blank(),
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal')+
  geom_rect(data= dataQC, aes(xmin = date[length(date)-bl], xmax = date[length(date)], 
                ymin = -Inf, ymax = Inf), alpha = 0.002) 

  

#logplot 
p[[2]]<-ggplot(dataQC, aes(x=date, y=cum.cases) ) +
  geom_point(size=3)+
  scale_y_continuous(trans='log2')+
  theme_classic()+
  ylab("Cummulative cases")+
  geom_smooth(method=lm, se=FALSE, color="firebrick")


#plot cummulative cases
p[[3]]<-ggplot( ) +
  geom_point(data=dataQC, aes(x=date, y=cum.cases),size=3, color="firebrick")+
  geom_point(data=deathsQC, aes(x=date, y=cum.deaths),size=3, color="black")+
  theme_classic()+
  ylab("Cummulative cases")+
  geom_point(size=3)+
  scale_x_date()+ 
  geom_vline(xintercept =as.Date("2020-03-16"), linetype="dashed")+
  geom_vline(xintercept =as.Date("2020-03-13"), linetype="dashed")+
  geom_vline(xintercept =as.Date("2020-03-23"), linetype="dashed")+
  annotate(geom="text",x=as.Date("2020-03-23"), y=3000,label="increased testing", angle=90)+
  annotate(geom="text",x=as.Date("2020-03-16"), y=3000,label="school closures", angle=90)+
  annotate(geom="text",x=as.Date("2020-03-13"), y=3000,label="health emergency", angle=90)+
  geom_rect(data =dataQC, aes(xmin = date[length(date)-bl], xmax = date[length(date)], 
                ymin = -Inf, ymax = Inf), alpha = 0.002) 



grid.arrange(grobs=p, nrow=1, top="Quebec COVID-19, Apr 1 2020")




# ------------------ ONTARIO ------------------------------------------


g<-list()
# plot incidence cases, deaths, recovered

dfON %>% ggplot() +
  geom_point( aes(x=date, y=cases, color="cases"), size=3)+
  geom_line( aes(x=date, y=cases, color="cases"))+
  geom_point( aes(x=date, y=deaths, color="deaths"), size=3)+ 
  geom_line( aes(x=date, y=deaths, color="deaths"))+
  geom_point( aes(x=date, y=recovered, color="recovered"), size=3)+ 
  geom_line( aes(x=date, y=recovered, color="recovered"))+ 
  ylab("Incidence")+
  theme_classic()+
  scale_colour_manual(breaks=c("cases","deaths", "recovered"),
                      values = c(cases="firebrick", deaths="black", recovered="blue"))+
  theme(legend.title = element_blank(),
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal')+
  geom_rect(aes(xmin = dfON$date[length(dfON$date)-bl2], xmax = dfON$date[length(dfON$date)], 
                ymin = -Inf, ymax = Inf), alpha = 0.002) -> g[[1]]



# plot cumulative cases logged
g[[2]]<-ggplot(dfON, aes(x=date, y=cumsum(cases)) ) +
  geom_point(size=3)+
  scale_y_continuous(trans='log2')+
  theme_classic()+
  ylab("Cummulative cases")+
  geom_smooth(method=lm, se=FALSE, color="firebrick")

# plot travel acquisitions and dates of travel-related controls implimented
g[[3]]<-ggplot(travON, aes(x=date, y=travel.acq) ) +
  theme_classic()+
  ylab("Incidence of travel-related cases")+
  geom_point(size=3)+
  geom_vline(xintercept =as.Date("2020-03-16"), linetype="dashed")+
  geom_vline(xintercept =as.Date("2020-03-18"), linetype="dashed")+
  annotate(geom="text",x=as.Date("2020-03-16"), y=10,label="close border", angle=90)+
  annotate(geom="text",x=as.Date("2020-03-18"), y=10,label="close US-CA border", angle=90)+
  geom_rect(data=travON, aes(xmin = date[length(date)-bl2], xmax = date[length(date)], 
                             ymin = -Inf, ymax = Inf), alpha = 0.002) 

# together
grid.arrange(grobs=g, nrow=1, top="Ontraio COVID-19, Apr 1 2020")


