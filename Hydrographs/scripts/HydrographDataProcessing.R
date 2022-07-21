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

path<-"O:/Monitor Wells/Website_data/Raw Data and Hydrographs"

files<-list.files(path, recursive = TRUE)
files<-files[!str_detect(files,"~")] # remove tildes ~ ghost files
files<-paste0(path,"/",files)
for (file in files) {
name<-str_remove(file,".xlsx")
j<-str_locate_all(name,"/")[[1]][str_count(name,"/"),1]
name<-str_remove(str_trunc(name,nchar(name)-j+3,"left"),"...")

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


read$time_date_1<-ymd_hms(read$time_date_1)
read$time_date_1<-as.Date(read$time_date_1)
read$time_date_4<-ymd_hms(read$time_date_4)
read$time_date_4<-as.Date(read$time_date_4)
field<-read %>% filter(!is.na(time_date_4)) %>% dplyr::select(c(4,5))
field$measurement_type <- "field"
recorded<-read %>% dplyr::select(c(1,2)) 
recorded$measurement_type<-"recorded"
names(recorded)<-names(field)


dat <- as_tibble(rbind(recorded,field))

# remove NA measurements
dat <- dat %>% mutate(measured_water_level_ft=as.numeric(measured_water_level_ft)) %>% filter(!is.na(measured_water_level_ft))
#dat <- dat %>% mutate(colornames=ifelse(dat$measurement_type=="recorded","royalblue3","black"),
#                      shapes=ifelse(dat$measurement_type=="recorded",16,0),
#                      sizes=ifelse(dat$measurement_type=="recorded",1.2,3.3))
cols<-c("black","royalblue3")
shapes<-c(0,16)
sized<-c(3.3,1.2)

#  plot
p <- ggplot(dat, aes(x = time_date_4, y = measured_water_level_ft)) + 
  geom_point(aes(color=measurement_type, shape=measurement_type, size=measurement_type)) + 
  xlab("") +
  ylab("Depth below measuring point (ft)") +
  labs(title=name, 
       subtitle = paste0("Station number: 15-73-01dba     Latitude: 41.2975     Longitude: -105.52417 \n Period of record: ",format(min(dat$time_date_4),"%m/%d/%Y")," - ", format(max(dat$time_date_4),"%m/%d/%Y")),
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
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") + 
  scale_shape_manual(values=shapes, labels=c("Field measurement", "Recorded water level")) + 
  scale_color_manual(values=cols, labels=c("Field measurement", "Recorded water level")) + 
  scale_size_manual(values=sized, labels=c("Field measurement", "Recorded water level"))

#p

draw<-ggdraw() +
  draw_plot(p) + 
  draw_image("./Hydrographs/data/SEO_logo_BW.png", x = 0.37, y= -0.36, scale =.11, vjust=0,hjust=0)

#draw

pdf(paste0("./Hydrographs/outputs/",name,".pdf"), family = "sans",width=11,height=8.5,pagecentre = TRUE)
print(draw)
dev.off()


}


