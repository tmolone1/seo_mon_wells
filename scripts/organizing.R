library(tidyverse)
library(janitor)
library(readxl)

sheets<-excel_sheets("./data/MOnWellNetwork Worksheet.xlsx")
for (sheet in sheets) {
  assign(paste0(sheet,"_data"), read_excel("./data/MOnWellNetwork Worksheet.xlsx", sheet=sheet) |> clean_names())
}

# Gillette
colnames(Gillette_data) <- as.character(unlist(Gillette_data[2,]))
Gillette_data <- Gillette_data |> clean_names()
gillette_filtered<- Gillette_data |> select(1:11,27:34) |> filter(!is.na(well_name) & well_name != "Well Name" & !is.na(t))
gillette_filtered <- gillette_filtered |> mutate(county="campbell", monitor_category="gillette")

# Huntoon
huntoon_selected<- Huntoon_data |> select(1:11,18:25) 
huntoon_selected <- huntoon_selected |> mutate(permit_number=substr(permit_no, 6, 10), county="albany", monitor_category="albany", comments_2=NA)
huntoon_selected <- huntoon_selected[,names(gillette_filtered)]
huntoon_selected$lat <- str_replace(huntoon_selected$lat, "-",".")

#LaGrange
lagrange_selected<- LaGrange_data |> select(1:10,26:33) 
lagrange_selected <- lagrange_selected |> mutate(permit_number=NA, county="goshen", monitor_category="lagrange", comments_2=comments_29, well_name=well_name_1)
lagrange_selected <- lagrange_selected[,names(gillette_filtered)]

#Laramie County
larco_selected<- `Laramie Co._data` |> select(1:11,23:31) |> filter(!is.na(well_name))
larco_selected <- larco_selected |> mutate(permit_number=substr(permit_no, 6, 11), county="laramie", monitor_category="laramie_co", comments_2=comments_26)
larco_selected <- larco_selected[,names(gillette_filtered)]

#Madison
madison_selected<- Madison_data |> select(1:11,18:25) 
madison_selected <- madison_selected |> mutate(permit_number=parse_number(str_replace(permit_no, "U.W. ", "")), county=c(rep("crook",9),"goshen",rep("crook",2),rep("weston",2),rep("niobrara",3)), monitor_category="madison", comments_2=comments)
madison_selected <- madison_selected[,names(gillette_filtered)]

#Platte County
platteco_selected<- `Platte Co._data` |> select(1:11,26:34) 
platteco_selected <- platteco_selected |> mutate(permit_number=NA, county="platte", monitor_category="platte_co", comments_2=comments_29, well_name=well_name_1)
platteco_selected <- platteco_selected[,names(gillette_filtered)]

#Prairie Center
prairie_center_selected<- `Prairie Center CA_data` |> select(1:11,18:25) 
prairie_center_selected <- prairie_center_selected |> mutate(permit_number=parse_number(str_replace(permit_no, "U.W. ", "")), county="goshen", monitor_category="prairie_center", comments_2=comments)
prairie_center_selected <- prairie_center_selected[,names(gillette_filtered)]

#Split Rock
splitrock_selected<- `Split Rock_data` |> select(1:10,20:27) |> filter(!is.na(well_name))
splitrock_selected <- splitrock_selected |> mutate(permit_number=NA, county="carbon", monitor_category="split_rock", comments_2=comments_23)
splitrock_selected <- splitrock_selected[,names(gillette_filtered)]

# combine to single data frame   
dattbl<-rbind(gillette_filtered,huntoon_selected,lagrange_selected,larco_selected, madison_selected, platteco_selected, prairie_center_selected, splitrock_selected)
  

# convert degrees minutes seconds to decimal degress            
degminsec<-dattbl |> filter(is.na(as.numeric(dattbl$lat))) |> select(c("well_name","lat", "long"))
degminsec$lat <- gsub("[^0-9.-]", " ", degminsec$lat)
degminsec$long <- gsub("[^0-9.-]", " ", degminsec$long)

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

degminsec$lat<- angle2dec(degminsec$lat) 
degminsec$long<- -angle2dec(degminsec$long) 

# merge together the locations that have DD and DMS coordinates and replace in the data frame
merged<-merge(dattbl[,names(degminsec)],degminsec, by="well_name", all.x=TRUE)
names(merged)<-c("well_name","orig_lat","orig_long","converted_lat","converted_long")

merged$lat_2 <- merged$orig_lat |> as.numeric()
merged$lat_3 <- merged$converted_lat |> as.numeric() 
merged$lat_2[is.na(merged$lat_2)] <- 0
merged$lat_3[is.na(merged$lat_3)] <- 0
merged<-merged |> mutate(lat=lat_2+lat_3)

merged$long_2 <- merged$orig_long |> as.numeric()
merged$long_3 <- merged$converted_long |> as.numeric()
merged$long_2[is.na(merged$long_2)] <- 0
merged$long_3[is.na(merged$long_3)] <- 0
merged<-merged |> mutate(long=long_2+long_3)


merged<-merged[,c("well_name","orig_lat","orig_long","converted_lat","converted_long","lat","long")]
dattbl$lat<-merged$lat
dattbl$long<-merged$long
dattbl<-merge(dattbl,merged[,1:5],by="well_name")

dattbl<-dattbl[,1:21] # removes extra lat/long coordinates


save(dattbl,file="./data/seo_mon_wells.Rda")  
write_csv(dattbl,"./outputs/seo_mon_wells.csv")

