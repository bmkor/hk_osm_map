hkdc2015<-rgdal::readOGR("DC_2015_poly Shapefile/",layer="GIH3_DC_2015_POLY")

plot(hkdc2015)
proj4string(hkdc2015)

hkdc2015<-sp::spTransform(hkdc2015,CRS("+init=epsg:2326"))
hkdc2015<-sp::spTransform(hkdc2015,CRS("+init=epsg:4326"))

require(leaflet) 

m <- leaflet() %>% 
  addProviderTiles(providers$HikeBike) 

spdf<-do.call(rbind,lapply(levels(hkdc2015$DISTRICT_E),function(d){
  cw<-subset(hkdc2015, DISTRICT_E == d) 
  spg<-rgeos::gUnaryUnion(cw)
  df<-data.frame(tc=unique(cw$DISTRICT_T),en=unique(cw$DISTRICT_E),
                 row.names = unique(cw$DISTRICT_E))
  SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(spg@polygons[[1]]@Polygons[[1]]),ID=d)),
                  proj4string = CRS(proj4string((hkdc2015)))),
                  data=df)
}))

m %>% addPolygons(data=spdf,popup=~tc)

# geojson::to_geobuf(geojsonio::geojson_json(spdf,geometry="polygon"),
#                    file="HK18Districts.geobuf") 


hkcoastlines <- geojsonio::geojson_sp(geojson::from_geobuf("Hong_Kong.geobuf"))

m %>% addPolygons(data=hkcoastlines, group="HK Coastlines") %>%
  addPolygons(data=hkdc2015, color = "red", popup=~CNAME, group = "HKDC 2015") %>%
  addLayersControl(overlayGroups = c("HK Coastlines","HKDC 2015"))

#m %>% addPolygons(data=hkdc2015, popup=~CNAME)

hkcoastlines<-spTransform(hkcoastlines,CRS("+init=epsg:4326"))

hkdc2015_lands<-rgeos::gIntersection(hkdc2015,hkcoastlines)

hkdc2015_lands<-do.call(rbind,lapply(1:length(hkdc2015@polygons),function(i){
  spg<-SpatialPolygons(list(hkdc2015@polygons[[i]]),proj4string = CRS(proj4string(hkdc2015)))  
  ipg<-rgeos::gIntersection(spg,hkcoastlines)
  SpatialPolygonsDataFrame(ipg,hkdc2015@data[i,],match.ID = F)
}))

require(geosphere)



hkdc2015_lands@data<-cbind(hkdc2015_lands@data,area=areaPolygon(hkdc2015_lands))

m %>% addPolygons(data=hkdc2015_lands,weight=1,popup=~CACODE) %>%
  addPolygons(data=dsp,fill=F,color="black",weight=2) %>%
  addPolygons(data=hkdc2015,weight=1,fill=F,color="red")

# geojson::to_geobuf(geojsonio::geojson_json(hkdc2015_lands,
#                                            geometry="polygon"),
#                    file="HKDC2015_coastlines.geobuf")

