##read data##
library(plyr)
library(ggmap)
library(sp)
library(mapview)

##data cleaning##
list <- list.files("E:/Lexi/data_paper3/data for SWAT/data1", pattern="\\.csv$",full.names = TRUE)
data <- lapply(list, read_csv)
lalo <- lapply(data, function(x) x%>% select(Longitude, Latitude))
lalo_un <- lapply(lalo,unique)
lalo_df <- ldply (lalo_un, data.frame)
write_delim(lalo_df,file = "lalo_df1.xls")
coordinates(lalo_df) <- ~ Longitude + Latitude
proj4string(lalo_df) <- CRS("+init=epsg:4326")
mapview(lalo_df)


##read NC data##
library(ncdf4)
library(raster)
library(rasterVis)
library(snow)

files <- list.files(pattern=".nc",full.names = TRUE)#read nc.file
china_coordinate <- read.csv("E:/Lexi/data_paper3/china_shp/china_point.csv",header = TRUE, sep = ",",stringsAsFactors = FALSE)
nc <- stack(files)
brick1 <- lapply(files,brick,varname ="runoff")#extract subsurface and surface runoff
runf <- stack(brick1)
brick2 <- lapply(files,brick,varname ="Snowf_tavg")
snowf <- stack(brick2)
coordinates(china_coordinate) <- ~lon +lat # extract value from point coordinated
cn_runf <- extract(runf, china_coordinate)
cn_snowf <- extract(snowf, china_coordinate)
combine_cn_runf <- cbind(china_coordinate,cn_runf)# China_runoff <- China_sur + China_sub
c_cn_runf <- as.data.frame(combine_cn_runf)
df_cn_runf <- data.frame(t(c_cn_runf[,-1]))
colnames(df_cn_runf) <- df_cn_runf[, 1]
write.csv(df_runoff, file = "china_runoff_2015.csv")


##check climate data##
library(data.table)
list <- list.files(pattern = '^p')
data_list <- lapply(list,fread)
df_data <- data.frame(matrix(unlist(data_list),nrow = length(data_list),byrow = TRUE))
dfdata <- data.frame(t(df_data))
data <- dfdata[-1,]
plot(data$X790,ylim=c(0,100))


##merge climate data of global weather data including pcp and tmp##
library(data.table)
setwd("D:/Jinzhu/linjingy/")
path <- "D:/Jinzhu/linjingy/"
list <- list.files(path, pattern = "^t", full.names = TRUE)
tmp <- lapply(list, fread) 
tmp_1 <- dplyr::bind_rows(tmp)
first_col <- as.numeric(as.factor(tmp_1[['ID']]))
tmp_all <- transform(tmp_1,'ID'=c(1:4896))
write.table(tmp_all,file = "tmp_all.txt",sep = ",", quote = FALSE,row.names = FALSE)

##select streamflow data##
library(dplyr)
path <- "H:/Documents/paper3_water_china/runoff_china/database/data_table.xlsx"
streamflow <- readxl::read_xlsx(path)
stfl_35 <- streamflow %>% 
  filter(station_number == 35) %>%
  mutate(date = as.Date(paste(year,month,day,sep="-"))) %>%
  select(date,discharge)
write.csv(stfl_35,"q_obs_35.csv",row.names = FALSE)

##
path <- "C:/Users/linjingy/Downloads/precipitation_0019/precipitation_0019/county_daily.csv"
prec <- readr::read_csv(path)
  
  