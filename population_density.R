require(readxl)


tmp<-tempfile()
download.file("http://www.bycensus2016.gov.hk/Page/Maintables/source/eng/A201a/Result.xlsx",tmp)
pp<-read_xlsx(tmp)

options(stringsAsFactors = F)
pp<-cbind.data.frame(district=pp$X__3,population=pp$X__12)
pp<-subset(pp, !is.na(district) & !is.na(population) & !grepl("otal",district) & district != "Marine")
pp$district<-gsub("\\(1\\)","",gsub(" and "," & ", pp$district))

all(pp$district %in% levels(hkdc2015_lands$DISTRICT_E)) == TRUE

pp<-cbind(pp, avg_p=as.numeric(pp$population)/sapply(pp$district,function(d){
  sum(hkdc2015_lands$DISTRICT_E == d)
}))

hkdc2015_lands@data<-cbind(hkdc2015_lands@data, density= sapply(1:length(hkdc2015_lands$area),function(i){
  aa<-hkdc2015_lands@data[i,]$area
  subset(pp, district==hkdc2015_lands@data[i,]$DISTRICT_E)$avg_p/aa
}))

hkden<-cbind.data.frame(lng=hkdc2015_lands$E00_CENTRO,lat=hkdc2015_lands$E00_CENT_1,density=hkdc2015_lands$density)
coordinates(hkden) <- ~lng+lat
proj4string(hkden) <- CRS("+init=epsg:2326")
hkden<-spTransform(hkden,CRS("+init=epsg:4326"))


hkden@data<-cbind(x=as.vector(hkden@coords[,1]),y=as.vector(hkden@coords[,2]),hkden@data)

require(leaflet.extras)
data(quakes)

head(quakes)

mm<-m %>%
  addHeatmap(data=hkden@data, lng=~x,lat=~y, intensity=~density,max=max(hkden$density)) %>%
  addPolygons(data=hkdc2015_lands,fill=F,weight=1)











