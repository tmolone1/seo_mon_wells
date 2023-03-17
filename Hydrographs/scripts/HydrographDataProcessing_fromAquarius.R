library(janitor)
library(dplyr)
library(readr)

longitude<-read_csv("https://seoflow.wyo.gov/Data/ExportList?interval=Latest&parameters[0]=24&value=Location_9&type=Location&subValue=null&subValueType=null&refPeriod=&legend=null&utcOffset=420&date=2023-02-16&endDate=null&calendar=1&wkid=4326&exportType=csv&filter=~&sort=Sequence-desc", 
                   skip=1) |> clean_names() |> dplyr::select("location","value") |> rename(longitude=value) |> unique() |> tibble()
latitude<-read_csv("https://seoflow.wyo.gov/Data/ExportList?interval=Latest&parameters[0]=24&value=Location_8&type=Location&subValue=null&subValueType=&refPeriod=&legend=null&utcOffset=420&date=2023-02-16&endDate=null&calendar=1&wkid=4326&exportType=csv&filter=~&sort=Sequence-desc", 
                    skip=1) |> clean_names() |> dplyr::select("location","value") |> rename(latitude=value) |> unique() |> tibble()
locations<-read_csv("https://seoflow.wyo.gov/Data/Export_Folder/?folder=61&utcoffset=420&exportType=csv&filter=~&sort=Identifier-asc", 
                    skip=1) |> clean_names() |> tibble()
data_sets<-read_csv("https://seoflow.wyo.gov/Data/ExportList?interval=Latest&parameters[0]=24&value=Location_1&type=Location&subValue=null&subValueType=null&refPeriod=&legend=null&utcOffset=420&date=2023-02-16&endDate=null&calendar=1&wkid=4326&exportType=csv&filter=~&sort=Sequence-desc", 
                    skip=1) |> clean_names() |> tibble()
record_dates<-data_sets |> group_by(location) |> summarize(start_rec=min(start_of_record), end_rec=max(end_of_record))

locations<-left_join(locations,longitude, by="location") 
locations<-left_join(locations,latitude, by="location")
locations<-left_join(locations,record_dates, by="location") |> tibble()

data_sets <- data_sets |> mutate(meas_type="NA")

for (loc in locations$location) {
this_location<-locations |> filter(location==loc)
data_set_ids <- data_sets |> filter(location  == loc)

# this set of statements "guesses" which dataset is the field measurements and which is the transducer data
data_set_ids$meas_type[grep("well",data_set_ids$data_set_id)] <- "recorded"
data_set_ids$meas_type[grep("pth.Well",data_set_ids$data_set_id)] <- "recorded"
data_set_ids$meas_type[grep("epth.Depth",data_set_ids$data_set_id)] <- "recorded"
data_set_ids$meas_type[grep("depth below LSD",data_set_ids$data_set_id)] <- "recorded"
data_set_ids$meas_type[grep("isits",data_set_ids$data_set_id)] <- "field"

data_query_strings <- data_set_ids |> filter(meas_type %in% c("recorded", "field")) |> dplyr::select(data_set_id) |> as.vector()
data_query_strings <- data_query_strings[["data_set_id"]] |> URLencode()
query_order<-data_set_ids |> filter(meas_type %in% c("recorded", "field")) |> dplyr::select(data_set_id, meas_type)

recorded<-read_csv(paste0("https://seoflow.wyo.gov/Export/DataSet?DataSet=",
                          data_query_strings[which(query_order$meas_type=="recorded")],
                          "&ExportFormat=csv&Compressed=false&RoundData=False&Unit=&Timezone=0"),
                                skip=4, col_types = c("T","d")) |> 
                                clean_names() |> 
                                tibble()
field<-read_csv(paste0("https://seoflow.wyo.gov/Export/DataSet?DataSet=",
                       data_query_strings[which(query_order$meas_type=="field")],
                       "&ExportFormat=csv&Compressed=false&RoundData=False&Unit=&Timezone=0"),
                                skip=4, col_types = c("T","d")) |> 
                                clean_names() |>
                                tibble()

field$measurement_type <- "field"
recorded$measurement_type<-"recorded"
names(recorded)<-names(field)

dat <- as_tibble(rbind(recorded,field))
## dat<-dat %>% filter(time_date_4>"2010-01-01")  # optional filter dates

# remove NA measurements
dat <- dat %>% mutate(timestamp_utc=as.Date(timestamp_utc),measured_water_level_ft=as.numeric(value_feet)) %>% filter(!is.na(measured_water_level_ft))
#dat <- dat %>% mutate(colornames=ifelse(dat$measurement_type=="recorded","royalblue3","black"),
#                      shapes=ifelse(dat$measurement_type=="recorded",16,0),
#                      sizes=ifelse(dat$measurement_type=="recorded",1.2,3.3))
cols<-c("black","royalblue3")
shapes<-c(0,16)
sized<-c(3.3,1.2)

#  plot
p <- ggplot(dat, aes(x = timestamp_utc, y = measured_water_level_ft)) + 
  geom_point(aes(color=measurement_type, shape=measurement_type, size=measurement_type)) + 
  xlab("") +
  ylab("Depth below ground surface (ft)") +
  labs(title=loc, 
      subtitle = paste0("Station number: ",this_location$identifier,"     Latitude: ",this_location$latitude,"     Longitude: ",this_location$longitude," \n Period of record: ",format(this_location$start_rec,"%m/%d/%Y")," - ", format(this_location$end_rec,"%m/%d/%Y")),
       caption = paste0("Date prepared:     ",format(Sys.time(),'%B %d, %Y %H:%M')),
       color = "Measurement type:", 
       shape = "Measurement type:",
       size = "Measurement type:") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust=0, color = "ivory4"),
    axis.text.x=element_text(angle=60, hjust=1),
    plot.margin = margin(1,1,1,1,unit = "in"),
    legend.position="bottom",
    legend.justification = "left",
    panel.grid.major = element_line(color = "ivory4",
                                    size = 0.4,
                                    linetype = 3),
    panel.border = element_rect(color="azure4", fill=NA)
  ) + 
  scale_y_reverse() + 
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_shape_manual(values=shapes, labels=c("Field measurement", "Recorded water level")) + 
  scale_color_manual(values=cols, labels=c("Field measurement", "Recorded water level")) + 
  scale_size_manual(values=sized, labels=c("Field measurement", "Recorded water level"))

p

draw<-ggdraw() +
  draw_plot(p) + 
  draw_image("./Hydrographs/data/SEO_logo_BW.png", x = 0.37, y= -0.36, scale =.11, vjust=0,hjust=0)

#draw

pdf(paste0("./Hydrographs/outputs/",name,".pdf"), family = "sans",width=11,height=8.5,pagecentre = TRUE)
print(draw)
dev.off()

}
