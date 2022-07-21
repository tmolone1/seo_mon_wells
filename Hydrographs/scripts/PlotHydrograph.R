library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(zoo)
library(readxl)

read<-read_excel("Huntoon #1.xlsx", skip=1)
read<-read %>% clean_names()
read$time_date_1<-ymd_hms(read$time_date_1)
read$time_date_1<-as.Date(read$time_date_1)
read$time_date_4<-ymd_hms(read$time_date_4)
read$time_date_4<-as.Date(read$time_date_4)
field<-read %>% filter(!is.na(time_date_4)) %>% select(c(4,5))
field$measurement_type <- "field"
recorded<-read %>% select(c(1,2)) 
recorded$measurement_type<-"recorded"
names(recorded)<-names(field)
dat<-as_tibble(rbind(recorded,field))
head(dat)


#  plot
p <- ggplot(dat, aes(x = time_date_4, y = measured_water_level_ft, color=measurement_type, shape=measurement_type)) + 
  geom_point() + 
  xlab("") +
  ylab("Depth Below Measuring Point (ft)") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_y_reverse() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

p

