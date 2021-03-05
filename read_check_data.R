##read data##
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


##check climate data##
library(data.table)
list <- list.files(pattern = '^p')
data_list <- lapply(list,fread)
df_data <- data.frame(matrix(unlist(data_list),nrow = length(data_list),byrow = TRUE))
dfdata <- data.frame(t(df_data))
data <- dfdata[-1,]
plot(data$X790,ylim=c(0,100))


##merge climate data of global weather data##
library(data.table)
setwd("D:/Jinzhu/linjingy/")
path <- "D:/Jinzhu/linjingy/"
list <- list.files(path, pattern = "^p", full.names = TRUE)
tmp <- lapply(list, fread) 
tmp_1 <- dplyr::bind_rows(tmp)
first_col <- as.numeric(as.factor(tmp_1[['ID']]))
tmp_all <- transform(tmp_1,'ID'=c(1:4064))
write.table(tmp_all,file = "pcp_all.txt",sep = ",", quote = FALSE,row.names = FALSE)

##read MySQL database##


