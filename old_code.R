

```{r  load the data}
#Ok, so only 1096 fail to parse!! So close!

#fix the time series so it is continuous now.
#crime2$time %>% hms

crime2$DATE %<>% as.character()
crime2$time %<>% as.character()

test<-strsplit(as.character(crime2$date[112679:113774])," ")

for (i in 1:length(test)){
  crime2$DATE[112679+i-1]<-test[[i]][[1]]
  crime2$time[112679+i-1]<-test[[i]][2]
}

crime2$datetime %<>% as.character()

crime2$datetime[112679:113774]<-paste0(crime2$DATE[112679:113774]," ",crime2$time[112679:113774])
crime2$datetime<-parse_date_time(crime2$datetime,c("%Y-%m-%d %H:%M","%Y-%m-%d %H:%M:%S"))
############# TIME SERIES IS GOOD TO GO#####





crime2<-crime2[crime2$incident != "Proactive Police Visit",]
crime2<-crime2[crime2$incident != "Other",]
crime2<-crime2[crime2$incident != "Community Engagement Event",]
crime2$incident<-droplevels(crime2$incident)

ag1<-aggregate(crime2$count,by=list(crime2$TMEAN,crime2$incident),FUN=sum)
names(ag1)<-c("TMEAN","incident","count")

gg.ls<-list()
for ( i in levels(ag1$incident)){
  gg.ls[[i]]<-ggplot(data=ag1[ag1$incident==i,],aes(x=TMEAN,y=count)) + geom_point() + ggtitle(label=i)
}
gg.ls


gg1<-ggplot(data=ag1,aes(x=TMEAN,y=count,color=incident)) + geom_point()
gg1

#Try poisson regression

fit<-glm(data=ag1,count~incident*TMEAN,family=poisson(link="log"))
summary(fit)

plot(predictorEffects(fit, ~ TMEAN))

fit<-multinom(incident ~ TMEAN, data = crime2)
z<-summary(fit)$coefficients/summary(fit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(fit))
head(pp <- fitted(fit))

gg.ls
```
```{r Minneapolis data}
minn<-fromJSON("https://services.arcgis.com/afSMGVsC7QlRK1kZ/arcgis/rest/services/PoliceOffense2013/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

test<-fromJSON("https://information.stpaul.gov/resource/v5dr-nvug.json")



```


Here, I will create at time series object for each crime type. Practice: apply functions and functional programming. Look at crime as a function of risk, hazard, a key variable being time elapsed until the next event. 

Base time scale. 15 min? 30 min? Let's start with one hour.

```{r time series analysis}
tseq<-seq(as.POSIXct("2014-08-14 00:00:00",tz="UTC"),to=as.POSIXct("2018-07-19 00:00:00",tz="UTC"),by="hour",tz="UTC")



c.ts<-list()
library(xts)

for (i in levels(crime2$incident)){
c.ts[[i]]<-xts(crime2$count[crime2$incident == i],order.by=crime2$datetime[crime2$incident == i])
c.ts[[i]]<-period.apply(c.ts[[i]],endpoints(c.ts[[i]],"hours"),sum)
#c.ts[[i]]<-elseif()

}

#aggregate(X2, format(time(X2),"%y-%m-%d %H"), mean)

#c.ts<-lapply(c.ts,function(x) aggregate(x, format(time(x),"%y-%m-%d %H"), sum))

#period.apply(X2, endpoints(X2, "hours"), mean)

kurver<-read.dbf("kurver_10km_617_70.dbf")


c.ts<-list()






```{r}

sub<-crime[grepl(c("WESTERN"),crime$block) | grepl(c("BLAIR"),crime$block) ,]
sub2<-crime[grepl(c("WESTERN"),crime$block) | grepl(c("BLAIR"),crime$block) ,]

crime2$neighborhood_name %<>% as.factor()

sub2<-crime[crime$grid == 89 & crime$incident_type != "Proactive Police Visit" & month(crime$DATE) > 2 & year(crime$DATE) > 2018 ,]
  
#sub2<-crime[,]
tab1<-aggregate(sub2$count,by=list(sub2$incident_type),sum)
tab1<-tab1[order(tab1$x),]



```




```