cpt<-read.csv("https://www.wastereduction.gov.hk/sites/default/files/wasteless.csv")
coordinates(cpt) <- ~lgt + lat
mm %>% addCircleMarkers(data=cpt,radius=2,opacity=0.3)