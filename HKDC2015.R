hk<-geojsonio::geojson_sp(geojson::from_geobuf("Hong_Kong.geobuf"))
hk<-spTransform(hk,CRS("+init=epsg:4326"))
m %>% addPolygons(data=hk)

hkdc<-do.call(rbind,lapply(levels(dc$DISTRICT_E),function(d){
  tmp<-subset(dc, DISTRICT_E==d)
  pg<-rgeos::gIntersection(tmp,hk)
  pg@polygons[[1]]@ID <- d
  pg
}))

hkdc<-SpatialPolygonsDataFrame(hkdc,data.frame(DISTRICT_E=levels(dc$DISTRICT_E),
                                               DISTRICT_T=levels(dc$DISTRICT_T),
                                               row.names = levels(dc$DISTRICT_E)))

m %>% addPolygons(data=hkdc,popup=~DISTRICT_E)

geojson::to_geobuf(geojsonio::geojson_json(hkdc),file="Hong_Kong_18Districts.geobuf")


require(rgdal)
dc<-readOGR("DC_2015_poly Shapefile/",layer="GIH3_DC_2015_POLY")
proj4string(dc)<-CRS("+init=epsg:2326")
dc<-spTransform(dc,CRS("+init=epsg:4326"))


hkdc2015<-do.call(rbind,lapply(levels(dc$CACODE),function(d){
  tmp<-subset(dc, CACODE==d)
  pg<-rgeos::gIntersection(tmp,hk)
  pg@polygons[[1]]@ID <- d
  pg
}))
dc$CACODE
hkdc2015<-rgeos::gIntersection(dc,hk)
m %>% addPolygons(data=hkdc2015)

geojson::to_geobuf(geojsonio::geojson_json(hkdc2015),file="Hong_Kong_2015DC.geobuf")