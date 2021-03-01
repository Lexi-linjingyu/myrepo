library(plyr)
library(ggmap)
library(sp)
library(mapview)

#data cleaning
list <- list.files("E:/Lexi/data_paper3/data for SWAT/data1", pattern="\\.csv$",full.names = TRUE)
data <- lapply(list, read_csv)
lalo <- lapply(data, function(x) x%>% select(Longitude, Latitude))
lalo_un <- lapply(lalo,unique)
lalo_df <- ldply (lalo_un, data.frame)
write_delim(lalo_df,file = "lalo_df1.xls")
#mapping
coordinates(lalo_df) <- ~ Longitude + Latitude
proj4string(lalo_df) <- CRS("+init=epsg:4326")
mapview(lalo_df)
