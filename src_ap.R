#src_ap.R 
#This script is used to process the air pollutant related data and generate data to visualization 
#This script has three sub-scripts including:
#1. Removing outlines of raw air pollutant data using anomalize packages;
#2. Calculate summer MDA8 O3 in from 2014 to 2022 ;
#3. Calculate mean surface O3 and NO2 concentration at 13:00 and 14:00 LT

#----sub-script 1: Removing outlines and extract site-specific air pollutants----
library(lubridate)
library(anomalize)
library(tibbletime)
library(reshape2)
library(magrittr)

site<-read.csv("/home/zhengh/AP/site_a.csv")
setwd("/home/zhengh/AP/input")
#define function to read required data
get_ap<-function(.file){
  date<-ymd(substr(unlist(strsplit(.file, "_"))[3], 1, 8))
  if(date>ymd("2014-5-12") & date<ymd("2022-9-1")){
    input<-data.table::fread(.file)
    input<-subset(input, type%in%c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3"))
    input 
  }
}


ap<-lapply(list.files("/home/zhengh/AP/input/", pattern = ".csv", full.names = TRUE),
           get_ap)
ap<-do.call(plyr::rbind.fill, ap)
ap$date<-lubridate::ymd_h(paste(ap$date, ap$hour))


#get all site code from raw input data
site_code<-as.character(names(ap)[4:ncol(ap)])

#calculate the delta prior and after data processing
delta<-as.data.frame(matrix(ncol=nrow(site)+1, nrow=6))
cover<-as.data.frame(matrix(ncol=nrow(site)+1, nrow=6))
names(cover)[1]<-"ap"
names(delta)[1]<-"ap"
delta$ap<-c("CO", "NO2", "O3", "PM10", "PM2.5", "SO2")
cover$ap<-c("CO", "NO2", "O3", "PM10", "PM2.5", "SO2")

ap<-data.frame(ap)
for (i in 1:nrow(site)){
  n<-which(site_code==site$code[i])
  code<-site$code[i]
  
  names(delta)[i+1]<-site$code[i]
  names(cover)[i+1]<-site$code[i]
  
  input<-ap[, c(1,3, n+3)]
  names(input)[3]<-"value"
  input[which(input$value>999), "value"]<-NA
  input<-reshape2::dcast(input, date~type, value.var = "value")
  input$code<-code
  input[,2:7]<-apply(input[, 2:7], 2, as.numeric)
  #convert to 298k and 1atom
  input[which(input$date<ymd_h("2018-9-1 0")), 2:7]<-input[which(input$date<ymd_h("2018-9-1 0")), 2:7]*0.916
  input$ratio<-input$PM2.5/input$PM10
  input[which(input$ratio>1), c("PM2.5", "PM10")]<-NA
  mean_pre<-apply(input[,2:7], 2, mean, na.rm=TRUE)
  input[is.na(input)]<-0
  
  stat<-list()
  
  #remove outliners
  
  for (j in c(2:7)){
    pollutant<-colnames(out)[j]
  #see 
    anomalized <-
      as_tbl_time(out, date) %>%
      time_decompose(pollutant, merge = TRUE,
                     frequency = "1 day",
                     trend = "1 month",
                     message = F) %>%
      anomalize(remainder, max_anoms = 0.001) %>%
      time_recompose() %>%
      data.frame()
    
    out[which(anomalized$anomaly=="Yes"), j]<-NA
    out[which(out[, j]==0), j]<-NA
    stat[[pollutant]]<-round(100*as.numeric(table(is.na(out[, j]))[1])/nrow(out),1)
  }
  
  input<-input[,1:8]
  
  mean_post<-apply(input[,2:7], 2, mean, na.rm=TRUE)
  delta[, i+1]<- round(as.numeric(mean_pre-mean_post),2)
  cover[, i+1]<- round(as.numeric(do.call(rbind, stat)),2)
  
  write.csv(out, file=paste0("/home/zhengh/AP/O3/AP/", code, ".csv"), row.names = FALSE)
  print (i)
}

cover<-melt(cover, measure.vars = 2:ncol(cover), variable.name = "code", value.name = "coverage")
delta<-melt(delta, measure.vars = 2:ncol(delta), variable.name = "code", value.name = "delta")

write.csv(cover, file="/home/zhengh/AP/O3/cover.csv", row.names = F)
write.csv(delta, file = "/home/zhengh/AP/O3/delta.csv", row.names = F)

#----sub-script 1: Removing outliers and extract site-specific air pollutants----



##----sub-script 2: Calculae summer MDA8 O3 from 2014 to 2022----

#define function to extract summer O3_8h data from raw inputs.
get_o3<-function(.file){
  month<-as.numeric(substr(unlist(strsplit(.file, "_"))[3],5, 6))
  if (month %in% seq(6, 8, 1)){
    input<-data.table::fread(.file, header = T)
    input<-subset(input, type=="O3_8h")
    input<-melt(input, measure.vars = 4:ncol(input), variable.name = "code", value.name = "O3_8h")
    input
  } 
}


o3<-lapply(list.files("/home/zhengh/AP/O3/AP", pattern = ".csv", full.names = T), get_o3)
o3<-do.call(plyr::rbind.fill, o3)
o3<-subset(o3, hour>7)
o3$date<-ymd(o3$date)


#Calculating daily MDA8 O3
o3<-openair::timeAverage(o3, pollutant="O3_8h", "code", statistic = "max", data.thresh = 0, avg.time="day")
o3$year<-year(o3$date)

#Define function to calculate seasonal MDA8 O3 with enough observations (>= 81 observations during summer)
get_mda8<-function(data){
  if (nrow(na.omit(data))>=81){
    mda8<-mean(data$O3_8h, na.rm=T)
  } else (mda8<-NA)
  return(mda8)
}

mda8<-ddply(o3, c("code", "year"), get_mda8)
mda8<-dcast(mda8, code~year, value.var = "V1")
mda8$mean<-apply(mda8[,2:9], 1, function(x) mean(x, na.rm=T))
mda8$delta<-mda8$`2022`-mda8$mean

site<-read.csv("/home/zhengh/AP/site.csv")
mda8<-merge(mda8, site, by="code")
save(mda8, file="/home/zhengh/AP/O3/mda8.Rdata")
##----sub-script 2: Calculae summer MDA8 O3 from 2014 to 2022 ----



###----sub-script 3: Get mean surface O3 and NO2 concentration at 13:00 and 14:00 local time----

#site<-read.csv("/home/zhengh/AP/site_a.csv")
setwd("/home/zhengh/AP/input/")
#Define the function to read summertime file
get_s5p<-function(.file){
  year<-as.numeric(substr(unlist(strsplit(.file, "_"))[3], 1, 4))
  month<-as.numeric(substr(unlist(strsplit(.file, "_"))[3], 5, 6))
  if (year %in%(seq(2014, 2022, 1)) & month %in% seq(6, 8, 1)){
      input<-data.table::fread(.file, header=T, na.strings = "")
      input<-reshape2::melt(input, measure.var=4:ncol(input), variable.name="code", value.name = "conc")
      input<-subset(input, type%in%c("O3", "NO2") & hour %in% c(13,14))
      return(input[, c("date", "hour", "code", "conc", "type")])
  }
}


ap<-lapply(list.files("./", pattern = ".csv", full.names = T), get_s5p)
ap<-do.call(plyr::rbind.fill, ap)


ap$date<-lubridate::ymd_h(paste0(ap$date, ap$hour))
#convert standard condition to reference condition (see Wang et al., 2021 ACP for details)
ap[which(ap$date<ymd("2018-9-1")), "conc"]<-ap[which(ap$date<ymd("2018-9-1")), "conc"]*0.9161

#data.thresh =8 to make sure availabe data at both 13:00 and 14:00 to calculate mean value
S5P_O3<-openair::timeAverage(subset(ap, type=="O3"), pollutant="conc", type="code", data.thresh = 8, avg.time = "day")
names(S5P_O3)[4]<-"O3"
save(S5P_O3, file="/home/zhengh/AP/O3/S5P_O3.Rdata")


S5P_NO2<-openair::timeAverage(subset(ap, type=="NO2"), pollutant="conc", type="code", data.thresh = 8, avg.time = "day")
save(S5P_NO2, file="/home/zhengh/AP/O3/S5P_NO2.Rdata")
###----sub-script 3: Get mean surface O3 and NO2 concentration at 13:00 and 14:00 local time----
