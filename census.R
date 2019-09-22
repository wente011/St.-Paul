library(tidycensus)
library(rgdal)
library(sf)
library(raster)

ckey="47b95b7affd600590d66898d665397c137bf310e"  #TOKEN (CENSUS HERE)

census_api_key(ckey, install = TRUE)

url<-"https://www2.census.gov/programs-surveys/popest/geographies/2016/all-geocodes-v2016.xlsx"

tmp<-tempfile()
download.file(url,destfile = tmp,method="curl",mode="wb") 
fips<-read_excel(tmp,range="A5:G43939")

fips<-fips[fips$`Area Name (including legal/statistical area description)`=="St. Paul city",]
#Ramsey county is 123, MN is 27

cens<-get_acs(geography = "tract", variables = "B19013_001",state = "MN", county = "Ramsey", geometry = TRUE) #These functions are fantastic!
#geometries are multipolygon sfc_multipolygon 
st_crs(cens)


tmp <- tempfile()
download.file("https://information.stpaul.gov/api/geospatial/ykwt-ie3e?method=export&format=Original",tmp,method="curl",mode="wb")
pg2.2<- read_sf(unzip(tmp,"PGrid/PoliceGrid.shp"))   #read the original shapefile here, no API token needed. 
p<-readOGR(unzip(tmp,"PGrid/PoliceGrid.shp")) #need to identify what is the CRS here. 

p<-shapefile(unzip(tmp,"PGrid/PoliceGrid.shp"))


unlink(tmp)
st_crs(pg2.2)


#Police grid map is in  WGS84 and I need to convert it to NAD83. CRS = 4326
pg.nad<-st_transform(x = pg2.2, crs = 32610)

#Going to test a map really quick
map.df<-crime2[crime2$incident=="Robbery",] %>% group_by(year,grid) %>% summarize_at("count",sum,na.rm=T)
names(pg2.2)<-c("DIST","grid","geometry")

pg2.3<-left_join(pg2.2,map.df) 
pg2.3<-pg2.3[pg2.3$year==2018,c(2:5)]


tmap_mode("view")
  tm_shape(pg2.3) + tm_polygons(col="count",n=7,group="grid",
                                alpha=0.45, title="Robbery Incidents by Grid 2018"
                                )
 

  
  ##leaflet(cens) %>%
#  addPolygons(group="GEOID") %>% 
#   addTiles(group = "OSM") %>%
#  addProviderTiles("CartoDB.Voyager")


