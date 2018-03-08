# 2015 District Council Constituency Area Boundaries (https://accessinfo.hk/zh_HK/request/shapefile_for_2015_district_coun)

require(rgdal)
dc<-readOGR("DC_2015_poly Shapefile/",layer="GIH3_DC_2015_POLY")
proj4string(dc)<-CRS("+init=epsg:2326")
dc<-spTransform(dc,CRS("+init=epsg:4326"))
geojson::to_geobuf(geojsonio::geojson_json(dc),file="HKDC2015.geobuf")

require(leaflet)
require(leaflet.minicharts)

m <- leaflet() %>% addTiles()

m %>% addPolygons(data=dc,opacity=0.4)

dsp<-do.call(rbind,lapply(levels(dc$DISTRICT_E),function(d){
  tmp<-subset(dc,DISTRICT_E == d)
  tmp<-rgeos::gUnaryUnion(tmp)
  tmp@polygons[[1]]@ID <- d
  tmp
}))

m %>% addPolygons(data=dsp)
dsp<-SpatialPolygonsDataFrame(dsp,data.frame(DISTRICT_E=levels(dc$DISTRICT_E),
                                             DISTRICT_T=levels(dc$DISTRICT_T),
                                             row.names = levels(dc$DISTRICT_E)))

geojson::to_geobuf(geojsonio::geojson_json(dsp),file="HK18Districts.geobuf")

