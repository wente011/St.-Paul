#Access a json:
library(dygraphs)
library(lubridate)
library(ggplot2)
library(reshape2)
library(jsonlite)
library(tidyr)
library(zoo)
library(curl)
library(RSocrata)
library(magrittr)
Sys.getenv("SOCRATA_EMAIL", "wente011@gmail.com")
Sys.getenv("SOCRATA_PASSWORD", "2018Data23!")
token="lxfC2o86PruIlgfIimrzGb8Q5"


crime <- read.socrata("https://information.stpaul.gov/resource/xytm-mbqv.csv",app_token =token)
w<-read.csv("1413419.csv",header=TRUE)
w$DATE %<>% as.character() 
w$DATE %<>% ymd()

w$TMAX<-na.locf(w$TMAX)
w$TMIN<-na.locf(w$TMIN)
w$TMEAN<-(w$TMAX+w$TMIN)/2

crime$DATE<-date(crime$date)

crime2<-merge(crime,w)
crime2$incident_type %<>% as.factor()
crime2$incident %<>% as.factor()

#crime$TIME<-hms(crime$TIME)
#crime$DATE<-mdy(crime$DATE)

#ft<-crime[crime$POLICE.GRID.NUMBER==88,]

ft2<-ft2[year(ft2$DATE) %in% c(2017,2018),]

ft2[ft2$INCIDENT.TYPE=="Att. Burglary, Forced Entry, Night, Residence",]

summary(ft2$BLOCK[grepl("WESTERN",ft2$BLOCK)])

ml<-aggregate(ft$Count,by=list(month(ft$DATE),year(ft$DATE),ft$INCIDENT),FUN=sum)
              

ml$dt<-my(paste0(ml$Group.1, "/",ml$Group.2))
library(zoo)

ml$dt<-as.Date(as.yearmon(ml$dt, "%m/%Y"))

dygraph(ml$x,)

m<-aggregate(ft$Count,by=list(month(ft$DATE),ft$INCIDENT),FUN=sum)

qplot(y=ml$x,x=ml$dt,color=ml$Group.3)




ml$datetime<-paste0(ml$x)


ml2<-melt(ml,id=c("Group.1","Group.2"))

ft2$BLOCK<-as.character(ft2$BLOCK)
