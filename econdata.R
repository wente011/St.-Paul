#load packages
library(XML)
#library(RCurl)
library(rlist)
library(tidyverse)
library(imputeTS)
library(magrittr)
library(lubridate)

#Get the unemployment statistics from DEED for St. Paul using XML. The URL is for monthly data from 2010 to 2019.

sue<-"https://apps.deed.state.mn.us/lmi/laus/Results.aspx?geog=2705123095&adjusted=0&periodtype=03&resultset=3&startyear=2010&endyear=2019"

theurl <- getURL(sue,.opts = list(ssl.verifypeer = FALSE))
tables <- readHTMLTable(theurl) %>% list.clean(fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
sue<-tables[[which.max(n.rows)]] %>% as_tibble()
names(sue)<-c("ym","unemploy_rate")
sue$ym %<>% paste0("-01") 
sue$ym %<>% ymd()

sue$unemploy_rate %<>% as.character() %>% as.numeric() 
sue[,2]<-sue[,2]/100

sue$unemploy_ratex<-xts(sue$unemploy_rate,order.by=sue$ym)
sue<-sue[order(sue$ym),]


url2<-"https://apps.deed.state.mn.us/lmi/laus/Results.aspx?geog=2705123095&adjusted=0&periodtype=03&resultset=2&startyear=2010&endyear=2019"
theurl <- getURL(url2,.opts = list(ssl.verifypeer = FALSE))
tables <- readHTMLTable(theurl) %>% list.clean(fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
su<-tables[[which.max(n.rows)]] %>% as_tibble()
names(su)<-c("ym","unemploy")
su$ym %<>% paste0("-01") 
su$ym %<>% ymd()

su$unemploy<-gsub(",","",as.character(su$unemploy)) %>% as.numeric()

sue<-left_join(sue,su)

tsm<-seq.Date(from=min(sue$ym),to=max(sue$ym),by="week") %>% enframe()
names(tsm)<-c("idx","ym")
sue2<-full_join(tsm,sue[,-3])
sue2<-sue2[order(sue2$ym),]

sue2 %<>% mutate_at(c("unemploy_rate","unemploy"),function(x) na_interpolation(x,option="spline"))

sue2$week<-week(sue2$ym)
sue2$year<-year(sue2$ym)
sue2$idx<-NULL

