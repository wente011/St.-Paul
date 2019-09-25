library(rnoaa)
library(lubridate)
library(tidyverse)


wd<-"https://www.ncdc.noaa.gov/cdo-web/api/v2/{endpoint}"
options(noaakey = tok)  #Need to define your token here. 

ncdc_datasets(stationid = "GHCND:USW00014922",token=tok)  #view datasets for Minneapolis-St. Paul airport.

#unique data set id = gov.noaa.ncdc:C00861

wd<-meteo_tidy_ghcnd(stationid = "USW00014922")  #excellent function for grabbing tidy meteorlogical data. #blessed 
wd$tmean<-((wd$tmax/10+wd$tmin/10)/2)*(9/5) + 32 #need to convert units here. 
wd$tmean


wd$HDD<-ifelse(wd$tmean<65,abs(wd$tmean),0)
wd$CDD<-ifelse(wd$tmean>65,abs(wd$tmean),0)


wd$week<-week(wd$date)
wd$year<-year(wd$date)

wd.wk<-wd %>% group_by(year,week) %>% summarize_at(c("prcp","HDD","CDD","snow"),sum,na.rm=T)
wd.wk.tmean<-wd %>% group_by(year,week) %>% summarize_at(c("tmean"),mean,na.rm=T)

wd.wk<-left_join(wd.wk,wd.wk.tmean)

#units are a litte funny and worth double checking. prcp is in tenths of millimeters.
#units can be checked here. https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

wd.wk$prcp<-wd.wk$prcp/10*0.0393701 #convert to basic milimeters then to inches. 
wd.wk$snow<-wd.wk$snow*0.0393701 #snow is in milimeters. 
