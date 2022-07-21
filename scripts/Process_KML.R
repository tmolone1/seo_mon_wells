library(plotKML)
library(rgdal)
library(sf)
layers = st_layers("./data/SEO Active Monitor Wells.kml")
varnames<-str_split(layers[["name"]]," ", simplify=TRUE)[,1]
i=1
for (name in layers[["name"]]) {
read<-st_read("./data/SEO Active Monitor Wells.kml", layer=name)
read$group<-varnames[i]
df<-rbind(df,read)
assign(varnames[i],read)
i=i+1

}
df$status<-"active"
save(df,file="./data/seo_mon_wells_active.Rda")  
write.csv(df,"./outputs/seo_mon_wells_active.csv")
