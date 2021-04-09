##read data##
library(plyr)
library(ggmap)
library(sp)
library(mapview)

##data cleaning##================================================================================================================
list <- list.files("E:/Lexi/data_paper3/data for SWAT/data1", pattern="\\.csv$",full.names = TRUE)
data <- lapply(list, read_csv)
lalo <- lapply(data, function(x) x%>% select(Longitude, Latitude))
lalo_un <- lapply(lalo,unique)
lalo_df <- ldply (lalo_un, data.frame)
write_delim(lalo_df,file = "lalo_df1.xls")
coordinates(lalo_df) <- ~ Longitude + Latitude
proj4string(lalo_df) <- CRS("+init=epsg:4326")
mapview(lalo_df)


##read NC data##================================================================================================================
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


##check climate data##=====================================================================================================
library(data.table)
list <- list.files(pattern = '^p')
data_list <- lapply(list,fread)
df_data <- data.frame(matrix(unlist(data_list),nrow = length(data_list),byrow = TRUE))
dfdata <- data.frame(t(df_data))
data <- dfdata[-1,]
plot(data$X790,ylim=c(0,100))


##merge climate data of global weather data including pcp and tmp##========================================================
library(data.table)
setwd("D:/Jinzhu/linjingy/")
path <- "D:/Jinzhu/linjingy/"
list <- list.files(path, pattern = "^t", full.names = TRUE)
tmp <- lapply(list, fread) 
tmp_1 <- dplyr::bind_rows(tmp)
first_col <- as.numeric(as.factor(tmp_1[['ID']]))
tmp_all <- transform(tmp_1,'ID'=c(1:4896))
write.table(tmp_all,file = "tmp_all.txt",sep = ",", quote = FALSE,row.names = FALSE)

##select streamflow data##=================================================================================================
library(dplyr)
path <- "H:/Documents/paper3_water_china/runoff_china/database/data_table.xlsx"
streamflow <- readxl::read_xlsx(path)
stfl_35 <- streamflow %>% 
  filter(station_number == 35) %>%
  mutate(date = as.Date(paste(year,month,day,sep="-"))) %>%
  select(date,discharge)
write.csv(stfl_35,"q_obs_35.csv",row.names = FALSE)

##===================================================================================================================
library(dplyr)
path <- "E:/Lexi/data_paper3/prec_0019/county_daily.csv"
path_2 <- "E:/Lexi/data_paper3/prec_0019/lon_lat.txt"
prec <- readr::read_csv(path)
colnames(prec)[6] <- 'FIRST_2'
lon <- readr::read_delim(path_2,delim = ",")  
study <- lon %>% 
  filter(26<lat & lat< 35) %>%
  filter(96<lon & lon< 103) 
study_yalong <- inner_join(prec,study,by = "FIRST_2")
ID <- data.frame(ID = c(1:86))
colnames(study_yalong)[5] <- 'NAME'
colnames(study_yalong)[7] <- 'DATE'
colnames(study_yalong)[8] <- 'PRECI'
study_yalong$PREC <- formattable::digits(study_yalong$PREC,3)
lat <- data.frame(LAT = c(unique(study_yalong[15])))
long <- data.frame(LON = c(unique(study_yalong[16])))
name <- data.frame(NAME = c(unique(study_yalong[5])))
pcp <- cbind(ID,name,lat,long)

group <- study_yalong %>% 
  group_by(NAME)%>%
  mutate(PREC=PRECI*10)%>%
  select(NAME,DATE,PREC)
list_ya = split(group,group$NAME)

filename <- unique(group$NAME)
txtfile = paste(filename,".txt",sep = "") 

nam <- list()
for (i in 1:86) {
  nam[[i]] <- paste("E:/Lexi/data_paper3/prec_0019/prec/",txtfile[i],sep="")
  write.table(list_ya[[i]][[3]],nam[[i]],row.names =FALSE,col.names = 20000101,quote = FALSE)
}

write.table(pcp_0009,"pcp_0009.txt",row.names = FALSE,quote=FALSE,sep=",")

##===================================================================##
library(dplyr)
xl_prec <- read.csv("H:/SWAT/SWAT_China/yalong_river/xinlong.csv") %>%
  select(date,prec,tempmax,tempmin) %>%
  filter(!is.na(prec),!is.na(tempmax),!is.na(tempmin))
write.csv(xl_prec,"H:/SWAT/SWAT_China/yalong_river/xl_prec.csv")


##=========read gauge-based data================================================================
library(data.table)
library(readr)
library(dplyr)
library(lubridate)
path <- "E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/data"
setwd(path)
mypath <- list.dirs(path, full.names = TRUE, recursive = TRUE)
files <- sapply(mypath, list.files, full.names = TRUE)
new_dir <- "data_elements"
dir.create(new_dir, recursive = TRUE)
for(file in files) {
  file.copy(file, new_dir)
}
num <- c('13011','12030','12001','13003','11002')
path1 <- "E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/data/data_elements"
setwd(path1)
mylist <- list.files(path1,pattern='13011')
pre_list <- lapply(mylist,fread)
all_pre_list <- bind_rows(pre_list)
colnames(all_pre_list)[c(1,5,6,7,10)] <- c('st_id','year','month','day','pre')
pre <- all_pre_list %>% 
  select('st_id','year','month','day','pre') %>%
  mutate(pcp=pre) %>%
  mutate(pcp=ifelse(pre>30000 & pre < 34000 & pre != 32700 & pre !=32766, pre%%1000/10,pcp)) %>%
  mutate(pcp=ifelse(pre==32700,pre*0,pcp))%>%
  mutate(pcp=ifelse(pre==32766,pre*0-99,pcp))%>%
  mutate(pcp=ifelse(pre < 30000,pre/10,pcp))%>%
  mutate(pcp=round(pcp,3))%>%
  mutate(date= make_date(year,month,day))%>%
  select('st_id','date','pcp')

list_pre <- split(pre,pre$st_id)
pre_filename <- unique(pre$st_id)
txtfile <- paste('pcp',pre_filename,".txt",sep = "") 
nam <- list()
for (i in 1:length(unique(pre$st_id))) {
    nam[[i]] <- paste("E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/swat_weather/pcp/",txtfile[i],sep="")
    write.table(list_pre[[i]][[3]],nam[[i]],row.names =FALSE,col.names = list_pre[[i]][[2]][1],quote = FALSE)
}

###============================================================

