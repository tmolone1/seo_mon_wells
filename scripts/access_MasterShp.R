library(sf)
library(sp)
shp<-st_read("O:/Monitor Wells/Mapping/MasterShp/MonWells.shp")
tbl<-as_tibble(shp@data)
