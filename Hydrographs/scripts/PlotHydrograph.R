library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(zoo)
library(readxl)
library(stringr)
library(png)
library(raster)
library(cowplot)
library(magick)
library(googledrive)
library(googlesheets4)


options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "./.secrets"
)

googledrive::drive_auth() 
gs4_auth(token = drive_token())

construction<-googlesheets4::read_sheet(drive_get(paste0("~/Projects/database/construction"))$id,col_types = 'icdcDcccddc') %>% tibble()
location<-googlesheets4::read_sheet(drive_get(paste0("~/Projects/database/location"))$id,col_types = 'c') %>% tibble()
info<-googlesheets4::read_sheet(drive_get(paste0("~/Projects/database/info"))$id,col_types = 'c') %>% tibble()
id_matching<-read.csv("./data/names.csv")
path<-"O:/Monitor Wells/Website Data/Hydrographs/Excel Hydrographs"

files<-list.files(path, recursive = TRUE)
files<-files[!str_detect(files,"~")] # remove tildes ~ ghost files
files<-paste0(path,"/",files)

files<-files[36:44] # Lagrange, or specify other file or files of interest
limits<-NULL # use NULL to use default scale #as.Date(c("2013-10-01","2023-04-01"))


for (file in files) {
  name<-str_remove(file,".xlsx")
  j<-str_locate_all(name,"/")[[1]][str_count(name,"/"),1]
  name<-str_remove(str_trunc(name,nchar(name)-j+3,"left"),"...")
  key<-id_matching %>% filter(value==name) %>% dplyr::select(id_key)
  # deal with sheets misnamed
  sheets <- excel_sheets(file)
  if ("data" %in% sheets) {
    read <- read_excel(file, sheet = "data", skip=1)
  } else {
    read <- read_excel(file, sheet = "Data", skip=1)
  }
  
  # go to the next file if the data doesn't look like the template
  read<-read %>% clean_names()
  if (ncol(read)>5) {next}
  
  if (class(read$time_date_1)[1]!=c("POSIXct")) {
    read$time_date_1<-ymd_hms(read$time_date_1)
  }
  read$time_date_1<-as.Date(read$time_date_1)
  if (class(read$time_date_4)[1]!=c("POSIXct")) {
    read$time_date_4<-ymd_hms(read$time_date_4)
  }
  read$time_date_4<-as.Date(read$time_date_4)
  
  field<-read %>% filter(!is.na(time_date_4)) %>% dplyr::select(c(4,5))
  field$measurement_type <- "field"
  recorded<-read %>% dplyr::select(c(1,2)) 
  recorded$measurement_type<-"recorded"
  names(recorded)<-names(field)
  
  dat <- as_tibble(rbind(recorded,field))
  ## dat<-dat %>% filter(time_date_4>"2010-01-01")  # optional filter dates
  
  # remove NA measurements
  dat <- dat %>% mutate(measured_water_level_ft=as.numeric(measured_water_level_ft)) %>% filter(!is.na(measured_water_level_ft))
  #dat <- dat %>% mutate(colornames=ifelse(dat$measurement_type=="recorded","royalblue3","black"),
  #                      shapes=ifelse(dat$measurement_type=="recorded",16,0),
  #                      sizes=ifelse(dat$measurement_type=="recorded",1.2,3.3))
  cols<-c("black","royalblue3")
  shapes<-c(0,16)
  sized<-c(3.3,1.2)
  selected_location<-location %>% filter(id_key==key$id_key)
  
  #  plot
  p <- ggplot(dat, aes(x = time_date_4, y = measured_water_level_ft)) + 
    geom_point(aes(color=measurement_type, shape=measurement_type, size=measurement_type)) + 
    xlab("") +
    ylab("Depth below ground surface (ft)") +
    labs(title=name, 
         subtitle = paste0("Station number: ",selected_location$well_number,"     Latitude: ",selected_location$lat,"     Longitude: ",selected_location$long," \n Period of record: ",format(min(dat$time_date_4),"%m/%d/%Y")," - ", format(max(dat$time_date_4),"%m/%d/%Y")),
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
    scale_x_date(date_breaks = "1 years", date_labels = "%Y", limits = limits) + 
    scale_shape_manual(values=shapes, labels=c("Field measurement", "Recorded water level")) + 
    scale_color_manual(values=cols, labels=c("Field measurement", "Recorded water level")) + 
    scale_size_manual(values=sized, labels=c("Field measurement", "Recorded water level"))
  
  p
  
  draw<-ggdraw() +
    draw_plot(p) + 
    draw_image("./Hydrographs/data/SEO_logo_BW.png", x = 0.37, y= -0.36, scale =.11, vjust=0,hjust=0)
  
  #draw
  
  jpeg(paste0("./Hydrographs/outputs/",name,".jpg"), family = "sans", quality = 85,width=11,height=8.5,units="in", res=72)
  print(draw)
  dev.off()
  
}


