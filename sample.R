hk<-geojson::from_geobuf("Hong_Kong.geobuf") #read from geobuf, output geojson
hk<-geojsonio::geojson_sp(hk) #convert to sp polygons
require(leaflet)
leaflet() %>% addTiles() %>% addPolygons(data=hk) #draw it
