names<-character(length(files))
i=1
for (file in files) {
  name<-str_remove(file,".xlsx")
  j<-str_locate_all(name,"/")[[1]][str_count(name,"/"),1]
  name<-str_remove(str_trunc(name,nchar(name)-j+3,"left"),"...")
  names[i]<-name
  i=i+1
}
i<-1
df<-as_tibble(names)
df$match<-character(length(df))
for(name in names) {
  df$match[i]<-agrep(name,location$well_name,max.distance = 3,value = TRUE)[1]
  i=i+1
}
  
  
  
write.csv(df,"names.csv")
