require(tidyverse)
require(sf)
require(mapview)


# 2015 District Council Constituency Area Boundaries (https://accessinfo.hk/zh_HK/request/shapefile_for_2015_district_coun)
# "https://accessinfo.hk/en/request/8/response/55/attach/4/DC%202015%20poly%20Shapefile.zip?cookie_passthrough=1" %>%
#   download.file(destfile = "DC2015.zip") 
# unzip(zipfile = "DC2015.zip")

hk<-st_read("DC_2015_poly Shapefile/") %>% 
  st_union() %>% 
  st_coordinates() %>%
  as_tibble() %>%
  select(-L2) %>%
  filter(L1 == 1) %>%
  select(-L1) %>%
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs=2326) 

hk %>%  mapview()

# tmp<-hk18DC %>%
#   sf::st_transform(4326) %>%
#   sf::st_bbox() %>%
#   osmdata::opq() %>%
#   osmdata::add_osm_feature("natural","coastline") %>%
#   osmdata::osmdata_sf()
# 
# tmp$osm_lines %>% mapview()
# tmp$osm_polygons %>% mapview()
# tmp$osm_points %>% mapview()


# hkg<-sf::st_read("data/hong-kong_china.geojson")
# 
# hkg %>% mapview()
# 
# hk %>% st_geometry() %>% 
#   st_intersection((tmp$osm_polygons %>% st_transform(2326) %>% st_geometry())) %>%
#   mapview() + hk
# 
# hk %>% st_geometry() %>% 
#   st_intersection((tmp$osm_lines %>% st_transform(2326) %>% st_geometry())) %>%
#   mapview() + tmp$osm_polygons
# 




#proj4string(dc)<-CRS("+init=epsg:2326")
# dc<-spTransform(dc,CRS("+init=epsg:2326"))
# dc<-spTransform(dc,CRS("+init=epsg:4326"))
# geojson::to_geobuf(geojsonio::geojson_json(dc),file="HKDC2015.geobuf")
# 
# require(leaflet)
# require(leaflet.minicharts)
# install.packages("osmar")
# require(osmar)
require(osmdata)

### download latest osm
# "https://download.geofabrik.de/asia/china-latest.osm.pbf" %>%
#   download.file(destfile="china-latest.osm.pbf")

### cut within the bbox=113.75,22.1,114.6,22.7 
# require(osmar)
# bx<-corner_bbox(113.75,22.1,114.6,22.7)
# src<-osmsource_osmosis(file="china-latest.osm")
# rg<-get_osm(bx,source=src)
# 
# osmar::osmsource_file("china-latest.osm")

### make query
require(osmdata)

#### custom region ####
bbx=c(113.75,22.1,114.6,22.7)
rg<-st_bbox(c(xmin=113.75,ymin=22.1,xmax=114.6,ymax=22.7)) %>%
  st_as_sfc() %>% st_set_crs(4326) %>% st_transform(2326)

#### get coastlines ####
q<-opq(bbox = bbx) %>%
  add_osm_feature("natural","coastline") 
oshk<-osmdata::osmdata_sf(q)

#### get lakes, reservoirs, etc... ####
qw<-opq(bbox = bbx) %>%
  add_osm_feature("natural","water") 
oshkw<-osmdata::osmdata_sf(qw)

#### get coastlines with the region, rg ####
w<-oshk$osm_lines %>% st_transform(2326) %>% st_geometry() %>% 
  st_crop(rg %>% st_geometry())

w %>% mapview()

#### can we make the coastalines as polygons? ####
w %>% 
  st_as_sf(data.frame(n=1:length(.))) %>% 
  mapview() ### linestring not in proper order

i<-w %>% st_geometry_type() %>% magrittr::equals("LINESTRING") %>% magrittr::not(.) %>% which() ## 9 in multilinestring

w[i] %>% mapview() ## disconnected linestring

#### make the disconnected linestring to two linestrings #####
w<-w[i] %>% 
  st_coordinates() %>%
  as_tibble() %>%
  group_by(L1) %>%
  group_map(~{
    .x %>%
      .[,1:2] %>%
      as.matrix() %>%
      st_linestring() %>%
      st_sfc(crs=w %>% st_crs())
  }) %>%
  do.call(c,.) %>%
  c(w[-i],.)

w %>% mapview()
rm(i)

#### rearrange the linestring to proper order #####

require(tidygraph)
require(lwgeom)
wg<-w %>%
  st_endpoint() %>%
  st_equals(w %>% st_startpoint(),sparse = F) %>%
  igraph::graph_from_adjacency_matrix(mode="directed") %>%
  as_tbl_graph() %>%
  mutate(n=1:length(w),
         startpt=(w %>%
                    st_startpoint() %>%
                    st_equals(w %>% st_endpoint(),sparse = F) %>%
                    apply(1,which) %>% sapply(function(x){length(x) == 0})),
         geometry=w) %>%
  morph(to_components) %>%
  lapply(function(g){
    gg<-g %>%
      mutate(d=centrality_degree())
    if(all(gg %>% pull(d) == 1)){
      # cycle -> polygon
      ig<-gg %>%
        as.igraph()
      ngs<-ig %>%
        igraph::neighborhood(nodes=1) %>% unlist()
      e<-ngs %>%
        lapply(function(v){
          ig %>%
            igraph::all_simple_paths(from=1,to=v) %>%
            unlist()
        })
      wh<-e %>% sapply(length) %>% magrittr::equals(ig %>% igraph::V(.) %>% length()) %>% which()
      idx<-e[[wh]] %>% unlist() %>% as.vector()
      gg %>%
        as_tibble() %>%
        st_as_sf() %>%
        .[idx,] %>% st_coordinates() %>%
        .[,1:2] %>% list() %>% 
        st_polygon() %>% st_sfc(crs=w %>% st_crs())
    } else{
      # linestring
      s<-gg %>% pull(startpt) %>% which()
      t<-gg %>% pull(d) %>% magrittr::equals(0) %>% which()
      idx<-(gg %>%
              as.igraph() %>%
              igraph::all_shortest_paths(from=s,to=t) %>%
              .$res %>% unlist())
      gg %>%
        as_tibble() %>%
        st_as_sf() %>%
        .[idx,] %>% st_coordinates() %>%
        .[,1:2] %>% st_linestring() %>% st_sfc(crs=w %>% st_crs())
    }
  })

wg %>% sapply(st_geometry_type) %>%
  magrittr::equals("LINESTRING") %>% which()

wg[[2]]<-wg[[2]] %>%
  st_coordinates() %>%
  .[,1:2] %>%
  rbind(.,wg[[15]] %>% st_coordinates() %>% .[,1:2]) %>%
  rbind(., wg[[2]] %>% st_coordinates() %>% .[1,1:2]) %>%
  list() %>%st_polygon() %>% st_sfc(crs=w %>% st_crs()) 

wg[[20]]<-wg[[20]] %>%
  st_coordinates() %>%
  .[,1:2] %>%
  rbind(.,wg[[20]] %>% st_startpoint() %>% st_coordinates()) %>%
  list() %>% st_polygon() %>% st_sfc(crs=w %>% st_crs()) 

wg<-wg[-15] %>% 
  do.call(c,.)

rgc<-rg
for(i in 1:length(wg)){
  rgc<-rgc %>%
    st_difference(wg[i])
}

rgc %>% mapview()
# 
# 
# ww2 %>%
#   activate(nodes) %>%
#   filter(m==2) %>%
#   mutate(d=centrality_degree()) %>%
#   pull(d) 
# 
# ww2 %>%
#   activate(nodes) %>%
#   filter(m==1) %>%
#   pull(startpt) %>% which()
# 
# ww2 %>%
#   activate(nodes) %>%
#   filter(m==1) %>%
#   mutate(d=centrality_degree()) %>%
#   pull(d) %>% magrittr::equals(1) %>% which()
# 
# ww2 %>%
#   activate(nodes) %>%
#   filter(m==1) %>%
#   as_tibble() %>%
#   st_as_sf() %>%
#   .[(ww2 %>%
#        activate(nodes) %>%
#        filter(m==1) %>%
#        as.igraph() %>%
#        igraph::all_shortest_paths(from=3,to=10) %>%
#        .$res %>% unlist()),] %>% st_coordinates() %>%
#   .[,1:2] %>% st_linestring() %>% st_sfc(crs=w %>% st_crs()) %>% mapview()
# 
# 
# ww2 %>%
#   activate(nodes) %>%
#   filter(m==2) %>%
#   as.igraph() %>%
#   igraph::neighborhood(nodes=1)
# 
# ww2 %>%
#   activate(nodes) %>%
#   filter(m==2) %>%
#   as.igraph() %>%
#   igraph::all_simple_paths(from=1,to=32)
# 
#   igraph::all_shortest_paths(from=1,to=1) %>%
#   .$res %>% unlist()
# 
# ww2 %>%
#   activate(nodes) %>%
#   filter(m==2) %>%
#   as_tibble() %>%
#   st_as_sf() %>%
#   .[(ww2 %>%
#        activate(nodes) %>%
#        filter(m==1) %>%
#        as.igraph() %>%
#        igraph::all_shortest_paths(from=1,to=1) %>%
#        .$res %>% unlist()),] %>% st_coordinates() %>%
#   .[,1:2] %>% st_linestring() %>% st_sfc(crs=w %>% st_crs()) %>% mapview()
# 
# 
# 
# tmp<-ww %>%
#   activate(nodes) %>%
#   filter(m==1) %>%
#   morph(to_shortest_path,10,3) %>%
#   mutate(selected_node = TRUE) %>%
#   unmorph()
# 
# tmp[[1]] %>%
#   pull(.tidygraph_node_index)
# 
# 
# 
# ww %>%
#   activate(nodes) %>%
#   filter(m==1) %>% 
#   activate(edges) %>%
#   as_tibble() %>%
#   st_as_sf() %>% mapview()
# 
# 
# 
# ww %>%
#   as_tibble() %>%
#   st_as_sf() %>% mapview()
# 
# ww %>%
#   activate(nodes) %>%
#   # group_by(m) %>%
#   # as_tbl_graph()
#   filter(m == 1) %>%
#   mutate(d=centrality_degree()) %>%
#   pull(d)
# 
# ww %>%
#   activate(nodes) %>%
#   group_by(m) %>%
#   group_map(~{
#     .x
#   })
# 
# 
# ww %>%
#   morph(to_components) %>%
#   .[[2]] %>%
#   mutate(d=centrality_degree(mode="all",loops = F)) %>%
#   pull(d)
# 
# ww %>%
#   morph(to_components) %>%
#   mutate(m=names(.)) %>%
#   unmorph()
# 
# ww %>% class()
# 
# summary(ww)
# ww[2]
# 
# ww<-ww %>%
#   morph(to_components) %>%
#   mutate(m=1:nrow()) %>%
#   unmorph()
# 
# for (i in 1:length(ww)){
#   ww[[i]]<-ww[[i]] %>%
#     mutate(idx=i)
# }
# ?quo
# ww %>% unmorph() %>%
#   pull(idx)
# 
# ww %>%
#   lapply(function(x){x})
# 
# ww %>%
#   activate(nodes) %>%
#   pull(m)
# 
# ?to_components
# 
# 
# require(lwgeom)
# w %>% st_startpoint()
# w %>% st_endpoint()
# 
# 
# ohkl<-rgb %>%
#   st_intersection(oshk$osm_lines %>% st_transform(2326)) 
# 
# ohkl<-ohkl %>% st_sf(data.frame(n=1:length(.)),geometry=.)
# 
# oshk$osm_lines %>% mapview()
# 
# ohkl  %>%
#   mapview()
# 
# ohkl[-c(7,25:27),] %>% mapview()
# 
# ohkl<-ohkl[-c(7,25:27),] %>%
#   st_geometry()
# 
# 
# hk %>% mapview() + ohkl
# require(lwgeom)
# 
# hkl<-hk %>% st_cast('LINESTRING')
# pts<-ohkl %>%
#   st_intersection(hk)
# 
# ohkl %>% st_intersection(hkl)  %>% mapview() + hkl
# hkl %>% st_distance(pts)
# 
# st_snap(hkl,ohkl,tolerance = 1e-9) %>% mapview()
# 
# st_split(hkl,oshk$osm_lines) %>% 
#   st_collection_extract("LINESTRING") %>% mapview()
# 
# st_split(hk,oshk$osm_lines %>% st_transform(2326)) %>%
#   st_collection_extract("POLYGON") %>% mapview()
# 
# rg %>% mapview()
# 
# rg %>% st_transform(2326) %>% st_difference(oshk$osm_lines %>% st_transform(2326)) %>% mapview()
# 
# 
# w %>%
#   st_crop(hkl) %>% mapview()
# 
# require(concaveman)
# w %>% st_as_sf() %>% mapview()
# w<-w %>% st_sf(data.frame(n=1:length(.)),geometry=.) 
# 
# w %>% st_intersects(w)
# require(tidygraph)
# 
# ww<-w[w %>% st_geometry_type() == "MULTILINESTRING",] %>%
#   st_geometry() %>% st_coordinates() %>%
#   as_tibble() %>% group_by(L1) %>%
#   group_map(~{
#     .x %>% .[,1:2] %>%
#       as.matrix() %>%
#       st_linestring() %>%
#       st_sfc(crs=2326)
#   }) %>% do.call(c,.) %>%
#   c(.,w[w %>% st_geometry_type() == "LINESTRING",] %>% st_geometry()) %>%
#   st_sf(data.frame(n=1:length(.)),geometry=.)
# 
# wg<-ww %>% st_intersects(ww,sparse=F) %>%
#   magrittr::and(.,upper.tri(.)) %>%
#   igraph::graph_from_adjacency_matrix("undirected") %>%
#   as_tbl_graph(directed=FALSE)
# 
# wgi<-ww %>% st_intersects(ww,sparse=F) %>%
#   magrittr::and(.,upper.tri(.)) %>%
#   igraph::graph_from_adjacency_matrix("undirected")
# 
# ww<-ww %>%
#   mutate(m=wgi %>% igraph::components(mode="strong") %>%
#            .["membership"] %>% unlist() %>% as.vector()) %>%
#   select(-geometry,geometry)
# 
# 
# 
# ww %>% filter(m==1)   %>%
#   st_geometry() %>% st_cast('POINT') %>% st_as_sf() %>%
#   concaveman(concavity = 1) %>% mapview() + (ww %>% filter(m==1)   %>%
#                                   st_geometry())
# 
# 
# wwg<-ww %>%
#   group_by(m) %>%
#   group_map(~{
#     s<-.x %>% st_startpoint() %>% st_equals(.x %>% st_endpoint(),sparse=F) %>%
#       apply(1,any) %>% magrittr::equals(FALSE) %>% which()
#     s<-ifelse(length(s) > 0, s, 1)
#     
#     l<-.x %>% st_endpoint() %>% st_equals(.x %>% st_startpoint(),sparse=F) %>%
#       apply(1,which)
#     
#     ll<-c()
#     while(length(s) && !any(ll == s)){
#       ll<-c(ll,s)
#       s<-l[[s]]
#     }
#     
#     if(l %>% unlist() %>% length() == l %>% length()){
#       .x[ll,] %>% st_coordinates() %>%
#         .[,1:2] %>% list() %>% st_polygon() %>%
#         st_sfc(crs=2326) 
#     } else{
#       .x[ll,] %>% st_coordinates() %>%
#         .[,1:2] %>% st_linestring() %>%
#         st_sfc(crs=2326)
#     }
#   },.keep=T) 
# 
# wwg %>%
#   mapview()
# 
# wwg %>% sapply(st_geometry_type)
# 
# wwg[[4]] %>% mapview() + wwg[[1]] + wwg[[2]]
#   st_coordinates()
# 
# p1<-wwg[[2]] %>%
#   st_coordinates() %>%
#   .[nrow(.):1,] %>%
#   rbind(.,wwg[[2]] %>%
#           st_coordinates() %>%
#           .[nrow(.),]) %>%
#   .[,1:2] %>%
#   list() %>%
#   st_polygon() %>%
#   st_sfc(crs=2326) 
#         
#         
# p2<-wwg[[1]] %>% st_coordinates() %>% .[nrow(.):1,] %>%
#   rbind(., wwg[[4]] %>% st_coordinates() %>% .[nrow(.):1,]) %>%
#   rbind(., wwg[[1]] %>% st_coordinates() %>% .[nrow(.),]) %>%
#   .[,1:2] %>%
#   list() %>%
#   st_polygon() %>%
#   st_sfc(crs=2326) 
# 
# wwg<-wwg[-c(1,2,4)] %>%
#   do.call(c,.) %>%
#   c(.,p1,p2) 
# 
# hk %>% mapview()
# 
# wwg1<-wwg %>%
#   st_difference(hk)
# 
# wwg2<-wwg %>%
#   st_intersection(hk)
# 
# wwg3<-oshk$osm_polygons %>% st_transform(2326) %>%
#   st_geometry() %>%
#   st_intersection(hk) 
# 
# rg %>% st_transform(2326) %>%
#  st_sym_difference(c(wwg1,wwg2,wwg3)) %>%
#   mapview()
# 
# rg %>% st_transform(2326) %>%
#   st_difference(wwg2) %>% mapview()
# ?st_difference
# 
# wwg2 %>%
#   st_difference(rg %>% st_transform(2326))
# 
# rg %>% st_transform(2326) %>%
#   st_difference(wwg2[2]) %>% mapview()
# 
# r<-rg %>% st_transform(2326)
# for(i in 1:length(wwg2)){
#   r<<-r %>%
#     st_difference(wwg2[i])
# }
# 
# for(i in 1:length(wwg3)){
#   r<<-r %>%
#     st_difference(wwg3[i])
# }
# r %>% 
#   st_difference(wwg3) %>%
#   mapview()
# 
# rg %>% st_transform(2326) %>%
#   st_difference(wwg2[2])
# 
# 
# 
# wg<-c(wwg1,wwg2,wwg3) %>%
#   st_intersection(rg %>% st_transform(2326)) 
# 
# 
# 
# wwg3 %>% mapview()
# 
# c(wwg1,wwg2,wwg3) %>% 
#   st_difference() %>%
#   mapview()
# 
# e<-ww %>% filter(m==7) 
# 
# 
# e %>% st_geometry_type()  == "MULTLINESTRING"
# 
# e %>%
#   st_cast('LINESTRING')
# 
# st_startpoint(e[1,])
# st_endpoint(e[1,])
# st_startpoint(e[2,])
# st_endpoint(e[2,])
# 
# e %>% st_coordinates() %>%
#   .[,1:2] %>% list() %>% st_polygon() %>% st_sfc(crs=2326) %>% mapview()
# 
# e %>% st_coordinates() %>%
#   .[,1:2] %>% st_linestring() %>% st_sfc(crs=2326) %>% mapview()
# e %>% mapview()
# 
# s<-e %>% st_startpoint() %>% st_equals(e %>% st_endpoint(),sparse=F) %>%
#   apply(1,any) %>% magrittr::equals(FALSE) %>% which()
# s<-ifelse(length(s) > 0, s, 1)
# 
# l<-e %>% st_endpoint() %>% st_equals(e %>% st_startpoint(),sparse=F) %>%
#   apply(1,which)
# 
# ll<-c()
# while(length(s) && !any(ll == s)){
#   ll<-c(ll,s)
#   s<-l[[s]]
# }
# 
# if(l %>% unlist() %>% length() == l %>% length()){
#   e[ll,] %>% st_coordinates() %>%
#     .[,1:2] %>% list() %>% st_polygon() %>%
#     st_sfc(crs=2326) 
# } else{
#   e[ll,] %>% st_coordinates() %>%
#     .[,1:2] %>% st_linestring() %>%
#     st_sfc(crs=2326)
# }
# 
# wg  %>%
#   morph(to_components) %>%
#   mutate(comp=) %>%
#   unmorph()
# 
# 
# w[c(2,197,200),] %>% mapview()
# 
# w %>% 
# concaveman(w %>% st_as_sf()) %>% mapview()
# 
# w %>%
#   st_crop(rg %>% st_transform(2326)) %>% mapview()
# 
# w %>% mapview()
# 
# w %>%
#   st_difference(hk) %>% mapview()
# 
# 
# 
# 
# 
# 
# oshk$osm_lines %>% st_transform(2326) %>% mapview()
# 
# st_startpoint(hkl) %>% mapview() + hkl
# 
# 
# ohkl[25:26] %>% mapview()
# 
# 
# 
# oshkw$osm_lines %>% mapview() + oshkw$osm_polygons + 
#   oshkw$osm_multipolygons %>% mapview()
# 
# plot(oshkw$osm_multipolygons,max.plot=27)
# 
# oshkw$osm_multipolygons %>%
#   st_geometry() %>% mapview(map.types="OpenStreetMap.DE")
# 
# 
# 
# 
# hkb %>%
#   st_intersection(oshk$osm_lines) %>% mapview()
# 
# q<-opq(bbox = c(113.75,22.1,114.6,22.7)) %>%
#   add_osm_feature("natural","coastline")
# oshk<-osmdata::osmdata_sf(q)
# 
# 
# 
# oshk<-osmdata::osmdata_sp(q,doc="china-latest.osm",quiet=FALSE)
# 
# oshk$osm_lines %>% mapview()
# 
# oshk$osm_polygons %>% mapview()
# 
# m <- leaflet() %>% addTiles()
# 
# m %>% addPolygons(data=dc,opacity=0.4)
# 
# dsp<-do.call(rbind,lapply(levels(dc$DISTRICT_E),function(d){
#   tmp<-subset(dc,DISTRICT_E == d)
#   tmp<-rgeos::gUnaryUnion(tmp)
#   tmp@polygons[[1]]@ID <- d
#   tmp
# }))
# 
# m %>% addPolygons(data=dsp)
# dsp<-SpatialPolygonsDataFrame(dsp,data.frame(DISTRICT_E=levels(dc$DISTRICT_E),
#                                              DISTRICT_T=levels(dc$DISTRICT_T),
#                                              row.names = levels(dc$DISTRICT_E)))
# 
# geojson::to_geobuf(geojsonio::geojson_json(dsp),file="HK18Districts.geobuf")
# 

#### ggmap #####
# install.packages("ggmap")
# require(ggmap)
# 
