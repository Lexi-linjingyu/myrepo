####  1.Longitude/lat       ####
list <- list.files("E:/Lexi/data_paper3/data for SWAT/data1", pattern="\\.csv$",full.names = TRUE)
data <- lapply(list, read_csv)
lalo <- lapply(data, function(x) x%>% select(Longitude, Latitude))
lalo_un <- lapply(lalo,unique)
lalo_df <- ldply (lalo_un, data.frame)
write_delim(lalo_df,file = "lalo_df1.xls")
coordinates(lalo_df) <- ~ Longitude + Latitude
proj4string(lalo_df) <- CRS("+init=epsg:4326")
mapview(lalo_df)

####  2.Merge climate data of global weather data including pcp and tmp##========================================================
library(data.table)
setwd("D:/Jinzhu/linjingy/")
path <- "D:/Jinzhu/linjingy/"
list <- list.files(path, pattern = "^t", full.names = TRUE)
tmp <- lapply(list, fread) 
tmp_1 <- dplyr::bind_rows(tmp)
first_col <- as.numeric(as.factor(tmp_1[['ID']]))
tmp_all <- transform(tmp_1,'ID'=c(1:4896))
write.table(tmp_all,file = "tmp_all.txt",sep = ",", quote = FALSE,row.names = FALSE)


####  3.Select streamflow data ####
library(dplyr)
path <- "H:/Documents/paper3_water_china/runoff_china/database/data_table.xlsx"
streamflow <- readxl::read_xlsx(path)
stfl_35 <- streamflow %>% 
  filter(station_number == 35) %>%
  mutate(date = as.Date(paste(year,month,day,sep="-"))) %>%
  select(date,discharge)
write.csv(stfl_35,"q_obs_35.csv",row.names = FALSE)

####  4.Read weather gauge-based data   ####
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
##=========precipitation
num <- c('13011','12030','12001','13003','11002')
path1 <- "E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/data/data_elements"
setwd(path1)
mylist <- list.files(path1,pattern='13011')
pre_list <- lapply(mylist,function(x) {
  read.table(x,fill = TRUE)
})
all_pre_list <- bind_rows(pre_list)
#remove some errors values
all_pre_list_f <- all_pre_list %>%
  filter(V10>40000)%>%
  mutate(pcp=V10)%>%
  mutate(pcp=ifelse(V10<200000000,trunc(V10/100000),pcp))%>%
  mutate(pcp=ifelse(V10>200000000,trunc(V10/10000000000),pcp))%>%
  mutate(V10=pcp)%>%
  select(-pcp)
all_pre_list <- all_pre_list[!(all_pre_list$V10>40000),]
all_pre_list <- rbind(all_pre_list,all_pre_list_f)
#data clean
colnames(all_pre_list)[c(1,2,3,4,5,6,7,10)] <- c('st_id','LAT','LONG','ELEVATION','year','month','day','pre')
info <- all_pre_list %>% 
  select('st_id','LAT','LONG','ELEVATION')%>%
  rename(NAME=st_id)%>%
  mutate(LAT=LAT/100)%>%
  mutate(LONG=LONG/100)%>%
  mutate(ELEVATION=ELEVATION/10,ELEVATION)%>%
  mutate(ELEVATION=ifelse(ELEVATION > 10000,ELEVATION-10000,ELEVATION))
info <- info[!duplicated(info$NAME),] %>%
  mutate(ID = 1:840) %>%
  relocate(ID,NAME,LAT,LONG,ELEVATION)
info$NAME <- sub("^","pcp",info$NAME)
write.table(info,"info_all_station.txt",quote=FALSE,sep = ",",row.names = FALSE)
pre <- all_pre_list %>% 
  select('st_id','year','month','day','pre') %>%
  filter(year >= 1960) %>%
  mutate(pcp=pre) %>%
  mutate(pcp=ifelse(pre < 30000,pre/10,pcp))%>%
  mutate(pcp=ifelse(pre>30000 & pre < 34000 & pre != 32700 & pre !=32766, pre%%1000/10,pcp)) %>%
  mutate(pcp=ifelse(pre==32700,pre*0,pcp))%>%
  mutate(pcp=ifelse(pre==32766,pre*0-99,pcp))%>%
  mutate(pcp=sprintf("%0.3f", pcp))%>%
  mutate(date=make_date(year,month,day))%>%
  select('st_id','date','pcp')

pre$date <- gsub('-','',pre$date,perl = TRUE)
date_start<-ymd(pre$date[1])
date_end<-ymd("2019-12-31")
complete_date<-date_start+days(0:as.numeric(date_end-date_start))

df2<-data.frame(date=complete_date,pcp_1=-99)
df2 <- df2%>%
  mutate(pcp_1=sprintf("%0.3f", pcp_1))
df2$date <- gsub('-','',df2$date, perl = TRUE)
n <- 840
df2_all <- do.call("rbind", replicate(n, df2, simplify = FALSE))
df2_all <- df2_all %>% 
  group_by(date)%>%
  mutate (st_id = pre_filename)
pre_df <- full_join(pre,df2_all)
pre_df <- pre_df %>%
  mutate(pcp_2=ifelse(is.na(pcp),pcp_1,pcp))%>%
  mutate(pcp=pcp_2)%>%
  select(-pcp_1)%>%
  select(-pcp_2)%>%
  arrange(st_id,date)
pre_df <- pre_df[!duplicated(pre_df[,1:2]),] 
# names(list_df)<- paste0(unique(pre$st_id))
list_pre <- split(pre_df,pre_df$st_id)
pre_filename <- sort(unique(pre$st_id))
txtfile <- paste('pcp',pre_filename,".txt",sep = "") 
nam <- list()
for (i in 1:length(unique(pre$st_id))) {
  nam[[i]] <- paste("E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/swat_weather/pcp/",txtfile[i],sep="")
  write.table(list_pre[[i]][[3]],nam[[i]],row.names =FALSE,col.names = list_pre[[i]][[2]][1],quote = FALSE)
}
##=========temperature
mylist <- list.files(path1,pattern='12001')
tem_list <- lapply(mylist,function(x) {
  read.table(x,fill = TRUE)
})
all_tem_list <- bind_rows(tem_list)
all_tem_list_f <- all_tem_list %>%
  filter(V10>40000)%>%
  mutate(tmp=V10)%>%
  mutate(tmp=ifelse(V10>=32766327663276632766,trunc(V10/1000000000000000),tmp))%>%
  mutate(V10=tmp)%>%
  select(-tmp)
all_tem_list <- all_tem_list[!(all_tem_list$V10>40000),]
all_tem_list <- rbind(all_tem_list,all_tem_list_f)
colnames(all_tem_list)[c(1,2,3,4,5,6,7,9,10)] <- c('st_id','LAT','LONG','ELEVATION','year','month','day','max','min')
info <- all_tem_list %>% 
  select('st_id','LAT','LONG','ELEVATION')%>%
  rename(NAME=st_id)%>%
  mutate(LAT=LAT/100)%>%
  mutate(LONG=LONG/100)%>%
  mutate(ELEVATION=ELEVATION/10,ELEVATION)%>%
  mutate(ELEVATION=ifelse(ELEVATION > 10000,ELEVATION-10000,ELEVATION))
info <- info[!duplicated(info$NAME),]%>%
  mutate(ID = 1:840) %>%
  relocate(ID,NAME,LAT,LONG,ELEVATION)
info$NAME <- sub("^","tmp",info$NAME)
write.table(info,"info_all_tem.txt",quote=FALSE,sep = ",",row.names = FALSE)
tem <- all_tem_list %>% 
  select('st_id','year','month','day','max','min') %>%
  filter(year >= 1960) %>%
  mutate(max=ifelse(max < 30000,max/10,max))%>%
  mutate(min=ifelse(min < 30000,min/10,min))%>%
  mutate(max=ifelse(max>30000 & max < 34000 & max != 32700 & max !=32766, max%%1000/10,max)) %>%
  mutate(min=ifelse(min>30000 & min < 34000 & min != 32700 & min !=32766, min%%1000/10,min)) %>%
  mutate(max=ifelse(max==32700,max*0,max))%>%
  mutate(min=ifelse(min==32700,min*0,min))%>%
  mutate(max=ifelse(max==32766,max*0-99,max))%>%
  mutate(min=ifelse(min==32766,min*0-99,min))%>%
  mutate(date=make_date(year,month,day))%>%
  mutate(max = sprintf("%0.3f", max))%>%
  mutate(min = sprintf("%0.3f", min))%>% 
  mutate(tem = paste(max,min,sep = ","))%>% 
  select('st_id','date','tem')
tem$date <- gsub('-','',tem$date,perl = TRUE)
df_tem<-data.frame(date=complete_date,tem_1=-99)
df_tem <- df_tem %>%
  mutate(tem_1=sprintf("%0.3f", tem_1))
df_tem <- df_tem %>%
  mutate(tem2=paste(tem_1,tem_1,sep = ","))%>%
  select(-tem_1)
df_tem$date <- gsub('-','',df_tem$date, perl = TRUE)
n <- 840
df_tem_all <- do.call("rbind", replicate(n, df_tem, simplify = FALSE))
df_tem_all <- df_tem_all %>% 
  group_by(date)%>%
  mutate (st_id = pre_filename)
tem_df <- full_join(tem,df_tem_all)
tem_df <- tem_df %>%
  mutate(tem_3=ifelse(is.na(tem),tem2,tem))%>%
  mutate(tem=tem_3)%>%
  select(-tem2)%>%
  select(-tem_3)%>%
  arrange(st_id,date)
tem_df <- tem_df[!duplicated(tem_df[,c('st_id','date')]),]
list_tem <- split(tem_df,tem_df$st_id)
tem_filename <- sort(unique(tem$st_id))
txtfile <- paste('tem',tem_filename,".txt",sep = "") 
nam <- list()
for (i in 1:length(unique(tem$st_id))) {
  nam[[i]] <- paste("E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/swat_weather/tem/",txtfile[i],sep="")
  write.table(list_tem[[i]][[3]],nam[[i]],row.names =FALSE,col.names = list_tem[[i]][[2]][1],quote = FALSE)
}
##==========wind
num <- c('13011','12030','12001','13003','11002')
path1 <- "E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/data/data_elements"
setwd(path1)
mylist <- list.files(path1,pattern='11002')
win_list <- lapply(mylist,function(x) {
  read.table(x,fill = TRUE)
})
all_win_list <- bind_rows(win_list)
#data clean
colnames(all_win_list)[c(1,2,3,4,5,6,7,8)] <- c('st_id','LAT','LONG','ELEVATION','year','month','day','win')
info <- all_win_list %>% 
  select('st_id','LAT','LONG','ELEVATION')%>%
  rename(NAME=st_id)%>%
  mutate(LAT=LAT/100)%>%
  mutate(LONG=LONG/100)%>%
  mutate(ELEVATION=ELEVATION/10,ELEVATION)%>%
  mutate(ELEVATION=ifelse(ELEVATION > 10000,ELEVATION-10000,ELEVATION))
info <- info[!duplicated(info$NAME),] %>%
  mutate(ID = 1:840) %>%
  relocate(ID,NAME,LAT,LONG,ELEVATION)
info$NAME <- sub("^","pcp",info$NAME)
write.table(info,"info_all_station.txt",quote=FALSE,sep = ",",row.names = FALSE)

win <- all_win_list %>% 
  select('st_id','year','month','day','win') %>%
  filter(year >= 1960) %>%
  mutate(win=ifelse(win < 30000,win/10,win))%>%
  mutate(win=ifelse(win==32766,win*0-99,win))%>%
  mutate(win_2=sprintf("%0.3f", win))%>%
  mutate(win=win_2)%>%
  mutate(date=make_date(year,month,day))%>%
  select('st_id','date','win')

win$date <- gsub('-','',win$date,perl = TRUE)
win_df <- full_join(win,df2_all)
win_df <- win_df %>%
  mutate(win_2=ifelse(is.na(win),pcp_1,win))%>%
  mutate(win=win_2)%>%
  select(-pcp_1)%>%
  select(-win_2)%>%
  arrange(st_id,date)
win_df <- win_df[!duplicated(win_df[,c('st_id','date')]),] 
# names(list_df)<- paste0(unique(win$st_id))
list_win <- split(win_df,win_df$st_id)
win_filename <- sort(unique(win$st_id))
txtfile <- paste('win',win_filename,".txt",sep = "") 
nam <- list()
for (i in 1:length(unique(win$st_id))) {
  nam[[i]] <- paste("E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/swat_weather/win/",txtfile[i],sep="")
  write.table(list_win[[i]][[3]],nam[[i]],row.names =FALSE,col.names = list_win[[i]][[2]][1],quote = FALSE)
}
##====================rh
mylist <- list.files(path1,pattern='13003')
rh_list <- lapply(mylist,function(x) {
  read.table(x,fill = TRUE)
})
all_rh_list <- bind_rows(rh_list)
#data clean
colnames(all_rh_list)[c(1,2,3,4,5,6,7,8)] <- c('st_id','LAT','LONG','ELEVATION','year','month','day','rh')
##make the station file
info <- all_rh_list %>% 
  select('st_id','LAT','LONG','ELEVATION')%>%
  rename(NAME=st_id)%>%
  mutate(LAT=LAT/100)%>%
  mutate(LONG=LONG/100)%>%
  mutate(ELEVATION=ELEVATION/10,ELEVATION)%>%
  mutate(ELEVATION=ifelse(ELEVATION > 10000,ELEVATION-10000,ELEVATION))
info <- info[!duplicated(info$NAME),] %>%
  mutate(ID = 1:840) %>%
  relocate(ID,NAME,LAT,LONG,ELEVATION)
info$NAME <- sub("^","pcp",info$NAME)
write.table(info,"info_all_station.txt",quote=FALSE,sep = ",",row.names = FALSE)
##data clean
rh <- all_rh_list %>% 
  select('st_id','year','month','day','rh') %>%
  filter(year >= 1960) %>%
  mutate(rh=ifelse(rh==32766,rh*0-99,rh))%>%
  mutate(rh=sprintf("%0.3f", rh))%>%
  mutate(date=make_date(year,month,day))%>%
  select('st_id','date','rh')
rh$date <- gsub('-','',rh$date,perl = TRUE)
rh_df <- full_join(rh,df2_all)
rh_df <- rh_df %>%
  mutate(win_2=ifelse(is.na(rh),pcp_1,rh))%>%
  mutate(rh=win_2)%>%
  select(-pcp_1)%>%
  select(-win_2)%>%
  arrange(st_id,date)
rh_df <- rh_df[!duplicated(rh_df[,c('st_id','date')]),] 
# names(list_df)<- paste0(unique(rh$st_id))
list_rh <- split(rh_df,rh_df$st_id)
rh_filename <- sort(unique(rh$st_id))
txtfile <- paste('rh',rh_filename,".txt",sep = "") 
nam <- list()
for (i in 1:length(unique(rh$st_id))) {
  nam[[i]] <- paste("E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/swat_weather/rh/",txtfile[i],sep="")
  write.table(list_rh[[i]][[3]],nam[[i]],row.names =FALSE,col.names = list_rh[[i]][[2]][1],quote = FALSE)
}
##============evaporation
path1 <- "E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/data/data_elements"
setwd(path1)
mylist1 <- list.files(path1,pattern='13240')
evp_list <- lapply(mylist1,function(x) {
  read.table(x,fill = TRUE,row.names=NULL)
})
all_evp_list <- bind_rows(evp_list)
#data clean
colnames(all_evp_list)[c(1,2,3,4,5,6,7,8)] <- c('st_id','LAT','LONG','ELEVATION','year','month','day','evp')
info <- all_evp_list %>% 
  select('st_id','LAT','LONG','ELEVATION')%>%
  rename(NAME=st_id)%>%
  mutate(LAT=LAT/100)%>%
  mutate(LONG=LONG/100)%>%
  mutate(ELEVATION=ELEVATION/10,ELEVATION)%>%
  mutate(ELEVATION=ifelse(ELEVATION > 10000,ELEVATION-10000,ELEVATION))
info <- info[!duplicated(info$NAME),] %>%
  mutate(ID = 1:840) %>%
  relocate(ID,NAME,LAT,LONG,ELEVATION)
info$NAME <- sub("^","pcp",info$NAME)
write.table(info,"info_all_station.txt",quote=FALSE,sep = ",",row.names = FALSE)

evp <- all_evp_list %>% 
  select('st_id','LAT','LONG','year','month','day','evp') %>%
  filter(year >= 1960) %>%
  mutate(LAT=LAT/100) %>%
  mutate(LONG=LONG/100) %>%
  mutate(evp=ifelse(evp < 30000,evp/10,evp))%>%
  mutate(evp=ifelse(evp==32766,evp*0-99,evp))%>%
  mutate(evp_2=sprintf("%0.3f", evp))%>%
  mutate(evp=evp_2)%>%
  mutate(date=make_date(year,month,day))%>%
  select('st_id','LAT','LONG','date','evp')
evp_zy <- evp %>%
  filter(date >= '2018-01-01') %>%
  filter(LONG >= 117.00 & LONG <= 117.73)%>%
  filter(LAT >= 24.17& LAT <= 25.03)
  
win$date <- gsub('-','',win$date,perl = TRUE)
win_df <- full_join(win,df2_all)
win_df <- win_df %>%
  mutate(win_2=ifelse(is.na(win),pcp_1,win))%>%
  mutate(win=win_2)%>%
  select(-pcp_1)%>%
  select(-win_2)%>%
  arrange(st_id,date)
win_df <- win_df[!duplicated(win_df[,c('st_id','date')]),] 
# names(list_df)<- paste0(unique(win$st_id))
list_win <- split(win_df,win_df$st_id)
win_filename <- sort(unique(win$st_id))
txtfile <- paste('win',win_filename,".txt",sep = "") 
nam <- list()
for (i in 1:length(unique(win$st_id))) {
  nam[[i]] <- paste("E:/Lexi/data_paper3/weather/gauge-based weather data China (1960-2020)/swat_weather/win/",txtfile[i],sep="")
  write.table(list_win[[i]][[3]],nam[[i]],row.names =FALSE,col.names = list_win[[i]][[2]][1],quote = FALSE)
}
####  5.Foreign data  ####
# matlab data
library(R.matlab)
library(dplyr)
data_20 <- readMat("Data20.mat")
info <- data_20[["Data20"]][c(1,5,6,7,11,12,13,17,18,19,23,24,25,29,30,31,35,36,37,41,42,43,47,48,49,53,54,55,59,60,61,65,66,67,71,72,73,77,78,79,83,84,85,89,90,91,95,96,97,101,102,103,107,108,109,113,114,115,119,120)]
rf <- data_20[["Data20"]][c(3,9,15,21,27,33,39,45,51,57,63,69,75,81,87,93,99,105,111,117)]
df_info <- matrix(unlist(info),byrow=TRUE,ncol=3)
df_2 <- as.data.frame(rf[[2]])
colnames(df_2) <- c("year","mon","day","discharge","V5")
df_2$date <- as.Date(with(df_2,paste(year,mon,day,sep="-")),"%Y-%m-%d")
df_2 <- df_2 %>% 
  select(date,discharge)
write.csv(df_info,"Data2_info.csv")
write.csv(df_2,"wu_river.csv",row.names = FALSE)

## reading .mdb file
library(RODBC)
con2 = odbcConnectAccess('D:/jingyVM/linjingy/min_river/SWAT2012.mdb')
sqltable = sqlTables(con2)
soil = sqlFetch(con2,"usersoil",as.is=TRUE)
hru_90 = hru[,2:7]
write.table(hru_90,"hru_lu_1990.txt",row.names = FALSE)
odbcClose(con2)

## reading .nc data
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

## read.dbf data
other = foreign::read.dbf("D:/jingyVM/linjingy/others/1.dbf",as.is = TRUE)
####  6.Web scraping  ######
##/// download weekly report
library(downloader)
library(rvest)
library(RSelenium)
library(stringr)
library(stringi)
library(purrr)
library(XML)
url = c('http://www.cnemc.cn/sssj/szzdjczb/index_8.shtml')
allsourcecode <- url %>% 
  read_html(encoding = "UTF-8")%>% 
  html_node("div[class=textcon_list]")
IMUFE_link <- allsourcecode %>% 
  html_nodes("a")%>%
  html_attr("href")
IMUFE_link <- str_replace(IMUFE_link,"./","/")
site<-rep('http://www.cnemc.cn/sssj/szzdjczb',20)
websites2<-paste(site,IMUFE_link,sep="")
out <- vector("character", length = length(websites2))
for(i in seq_along(websites2)){
  derby <- read_html(websites2[i])
  out[i] <- derby %>%
    html_node("div[class=text]") %>%
    html_node("div") %>%
    html_node("a") %>%
    html_attr("href")
}
##scraping website2
date <- vector("character", length = length(websites2))
for(i in seq_along(websites2)){
  derby <- read_html(websites2[i])
  date[i] <- derby %>%
    html_node("div[class=text]") %>%
    html_node("h5") %>%
    html_text()
}
date2 <- stri_extract_first_regex(IMUFE_link, "\\d{6}")
out <- str_replace(out,"./","/")
site<-rep('http://www.cnemc.cn/sssj/szzdjczb/',20)
out2<-paste(site,date2,out,sep="")

##scraping name
name <- vector("character", length = length(websites2))
for(i in seq_along(websites2)){
  derby <- read_html(websites2[i])
  name[i] <- derby %>%
    html_node("div[class=text]") %>%
    html_node("h1") %>%
    html_text()
}
##download files
for (i in 1:length(out2)){
  download.file(out2[[i]], destfile = paste(name[[i]],".doc"), method="auto")
}
##///scraping web water quality
library(RCurl)
library(XML)
library(stringr)
library(dplyr)
library(Rwebdriver)
start_session(root = 'http://localhost/wd/hub/',browser ="chrome")# ???????????????4444,??????????????????chorme,????????????????????????firefox
list_url <-  "http://106.37.208.243:8068/GJZ/Business/Publish/Main.html?nsukey=izcwxfBd9wvE6xZJmS9DC%2BOo7fv7ybS8nnsrmgK4cyMrp54jCB0NEXQBVjHOqwFgPrUPw2B8TL6b2mQbPLe4HWhy0yPx80Vj1mmfXMC4nBUa3naLxIrNOvCXjHO4a25yILUSi%2Fu7K%2F2QrmjBVN1DaQ9k1056DUZQRdPPYtpWzEWXHPpeZoHNvcoDS0EtEC6HodxX7A34wVTWuAOGl5kh9w%3D%3D"
post.url(url = list_url)
pageSource <- page_source()

####  7.Water quality data ####
#/// read .docx data ///
library(docxtractr)
library(dplyr)
path = "D:/jingyVM/linjingy/water quality"
mypath = list.dirs(path, full.names = TRUE, recursive = TRUE)
files = sort(unique(list.files(mypath,pattern = "\\.docx$", full.names = TRUE, recursive = TRUE)))
name = sub(pattern = "(.*)\\...*$",replacement = "\\1",basename(files))
name_fi <- gsub('_','',name, perl = TRUE)
doc <- lapply(files,read_docx)
doc1 <- "C:/desktop/doc1.docx"
table1 = lapply(doc1,function(x){
  docx_extract_all_tbls(x, guess_header = FALSE)
})
names(table) = paste0(name_fi)
list.table = lapply(table, function(x){
  dplyr::bind_rows(x,.id = "id")
})
df.table = bind_rows(list.table,.id="column_name")
name_river = matrix(data=NA,nrow=20008,ncol=1)
df2.table = cbind(df.table[1:20008,1:4],name_river,df.table[1:20008,5:ncol(df.table)])
df2.table$V12 = NULL
df.table = df.table[20008:60804,]
nm = names(df2.table)
names(df.table) = nm
df3.table = rbind(df2.table,df.table)
df3.table[df3.table== c("","-","-")] <- NA
df3.table$V1[df3.table$V1==""]<-NA
df4.table <- df3.table %>%
  filter(!is.na(V1))
df4.table[,9:11] <- lapply(
    df4.table[,9:11], 
    function(x)as.numeric(gsub("???", NA, x)))

dftable <- df5.table%>%
  mutate(s = V6+V7+V8-6)
readr::write_excel_csv(df4.table,"water_quality.csv")
water_quality <- read.csv("water_quality.csv",encoding = "UTF-8")
d <- sort(unique(water_quality$V1))
d <- d[10:158]
d <- d[-3]
water <- water_quality%>%
  filter(V1 %in% d)
colnames(water)[c(2,7,8,9,10,11,12)]<- c("date","river_name","basin_name","ph","do","cod","nh3")
## clean water quality
quality = read.csv("~/Files/2020/paper 4/water quality/changed_row.csv")
min_river_sc1 = quality %>%
  filter(river_name == "min_river_sc")%>%
  select(2,13)%>%
  distinct()
min_river_sc2 = quality %>%
  filter(river_name == "min_river_sc2")%>%
  select(2,13)%>%
  distinct()
min_river = left_join(min_river_sc1,min_river_sc2,by="column_name")
min_river = tidyr::gather(min_river,"id","nh4",c(2,3))
ggplot(min_river,aes(column_name,nh4,color=id))+
  geom_point()
readr::write_excel_csv(quality,"changed_row.csv",)

####  8.Geocoding  #### 
library(recharts)
lat = map_location$lat
lon = map_location$lon
address = map_location$address
df = data.frame(lat=lat,lon=lon)
df2 <- convBD2WGS(df)
df3 = cbind(df2,address)
readr::write_excel_csv(df3,"wwtps.xls")


#### 9.HRU Identify/threhold combination ####
library(topHRU)
hru_table = extract_hru("D:/jingyVM/linjingy/min_river/min_river.mdb")
cols = c("LANDUSE","SOIL","SLP","UNIQUECOMB")
hru_table[cols]=lapply(hru_table[cols],factor)
hru_eval = evaluate_hru(hru_table=hru_table,luse_thrs = c(0,20,1),soil_thrs = c(0,20,1),weight = c(2, 1, 1))
hru_eval$result_nondominated
plot_pareto(hru_eval, area_thrs = 0.05, hru_thrs = 2000,
            interactive = TRUE)

####  10.Write point source daily ####
library(sf)
point = foreign::read.dbf("point_select.dbf",as.is = TRUE)
point_list = sort(point$POINTID[duplicated(point$POINTID)])
point_list_r = unique(point[duplicated(point$POINTID),])
point_inf = point_list_r[,c(13,23)]
q_point = readxl::read_xlsx("H:/Documents/paper3_water_china/nitrogen sources/point sources.xlsx")
q_point_info = q_point[,c(1,2,13)]
colnames(q_point_info)[1:3] = c("NAME_2","YEAR","NH3")
join = merge(point_inf,q_point_info,by="NAME_2")
join2 = join[,2:4]

module = readr::read_table2("3p.dat",col_names = FALSE,skip = 1)
colnames(module)=module[1,]
module = as.data.frame(module[-1,])

join2[,2] = as.character(join2$YEAR)
join2$NH3= format(as.numeric(join2$NH3),scientific = TRUE,digits = 11)
join2$NH3 = gsub("e","E",join2$NH3)
join_modu = left_join(module,join2,by="YEAR")
join_modu2 = join_modu %>%
  select(-NH3CNST)%>%
  rename(NH3CNST=NH3)%>%
  relocate(NH3CNST,.after = NO3CNST)
list_modu = split(join_modu2[-which(names(join_modu2)=="Subbasin")],f=join_modu2$Subbasin)

#write.table(module,quote=FALSE,row.names=FALSE)
time = as.character(x= lubridate::now(),format='%m/%d/%Y %H:%M:%S')
title = paste(time,"AM .dat file Daily Record Subbasin  10 ArcSWAT 2012.10_4.21 interface")
max.print <- getOption('max.print')
max.width <- getOption('width')
options(width = 1000L)
options(max.print=nrow(module)*ncol(module))
point_name = sort(unique(join_modu2$Subbasin))
txtname = paste(point_name,'p','.dat',sep="")
subbasin = list()
for (i in 1:length(list_modu)){
  sink(txtname[i])
  writeLines(title)
  cat("\n\n\n\n")
  print(list_modu[[i]],row.names = FALSE)
  sink()
  options(max.print = max.print)
  options(width = 1000)
}

#### 11.Write non-point source data####
library(readr)
library(rlist)
library(dplyr)
library(data.table)
library(lubridate)
library(RODBC)
path = "D:/jingyVM/linjingy/min_river/Scenarios/Default/TxtInOut"
list = list.files(path, pattern="\\.mgt$")
file = lapply(list,function(x){
  read_table2(x,col_names = FALSE)
  }
  )
name = gsub(".mgt","",list)
names(file) = paste(name)#rename list elements
select_file = list()
for (i in name){
  if (file[[i]][["X7"]][1]== "Luse:AGRC"){
    select_file[[i]] = print(i)
  }
}
selection = paste(as.vector(unlist(select_file)),".mgt",sep = "")#select.mgt files which contains agriculture
hrus <- read_table("D:/jingyVM/linjingy/min_river/Watershed/text/HRULandUseSoilsReport.txt",col_names = FALSE, skip = 180)
hrus_sele = hrus[grep("(\\d+)",hrus$X1),]
hrus_sele = hrus_sele[grep("(\\d+)",hrus_sele$X6),]
hrus_agri = hrus_sele %>%
  filter(X2 == "Agricultural")%>%
  mutate(X1= as.integer(X1))%>%
  select(X1:X7)
con = odbcConnectAccess('D:/jingyVM/linjingy/min_river/min_river.mdb')
sqltable = sqlTables(con)
other = sqlFetch(con,"hrus",as.is =TRUE)
odbcClose(con)
join_agri = right_join(other,hrus_agri,by=c("HRU_ID"="X1"))
fina_agri = join_agri[,c(12,2,6,8,13,19)]
fina_agri = fina_agri%>%
  mutate(HRU_ALL = HRU_GIS)
fina_agri$HRU_GIS = as.numeric(stringr::str_sub(fina_agri$HRU_GIS, -2))
fina_agri$HRU_ID = paste("HRU:",fina_agri$HRU_ID,sep = "")
fina_agri$SUBBASIN = paste("Subbasin:",fina_agri$SUBBASIN,sep = "")
fina_agri$SOIL = paste("Soil:",fina_agri$SOIL,sep = "")
fina_agri$SLP = paste("Slope:",fina_agri$SLP,sep="")
fina_agri$HRU_GIS = paste("HRU:",fina_agri$HRU_GIS,sep = "")
start_time = seq(as.Date("2007/01/01"),by = "day",length.out = 4383)
time = as.data.frame(start_time,ncol=1)
join_time = merge(time,fina_agri)
ferti = readxl::read_xlsx("H:/Documents/paper3_water_china/nitrogen sources/fertilizer_yearly_city.xlsx",range = "K1:L13")
livestock = readxl::read_xlsx("H:/Documents/paper3_water_china/nitrogen sources/livestock_manure.xlsx",range = c("N602:O614"))
names(ferti)[2] = paste("coef")
names(livestock)[1:2] = paste(c("year","birds"))
agri_time = join_time %>%
  mutate(year = year(start_time),
         month = month(start_time),
         day = day(start_time))%>%
  select(HRU_ID,X7,year,month,day,HRU_ALL)%>%
  merge(ferti) %>%
  mutate(fertilizer = coef * X7)%>%
  merge(livestock) %>%
  mutate(live_manure = birds * X7)%>%
  filter(month == 1 & day == 1)
agri_time$fertilizer = format(agri_time$fertilizer,digits = 8,justify = "right")
agri_time$live_manure = as.integer(agri_time$live_manure,justify = "right")
list_agri_time = split(agri_time,agri_time$HRU_ALL)
k = readLines("000030005.mgt")
middle = k[1:30]
last_1 = list()
last_2 = list()
for (i in 1:length(list_agri_time)){
  for (j in list_agri_time[[i]]["fertilizer"]){
    last_1[i] = paste("  1  1           3    4",j,"  0",collapse ="\n",sep = " ")
  }
  header[i] = paste(" .mgt file Watershed",fina_agri$HRU_ID[i],fina_agri$SUBBASIN[i],fina_agri$HRU_GIS[i],"Luse:AGRC",fina_agri$SOIL[i],fina_agri$SLP[i],"7/29/2021 12:00:00 AM ArcSWAT 2012.10_4.21")
  sink(selection[i])
  cat(header[i])
  writeLines(middle)
  cat(last_1[[i]])
  sink() 
}

for (k in list_agri_time[[i]]["live_manure"]){
  last_2[i] = paste("  1  1          14",k,"47  0       0.0000",collapse = "\n",sep = " ")
}
writeLines(last_2[[i]])
