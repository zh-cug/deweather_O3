#src-s5p.R
#This script is used to process the S5P raw data and generate data to visualization.
#Notes: Download the NC file from the https://disc.gsfc.nasa.gov/datasets using the GES DISC Subsetter
#NOtes: One should install CDO (https://code.mpimet.mpg.de/projects/cdo) prior to use this script
#Four sub-script in this script to
#1. Convert the raw S5P TROPOMI HCHO nc file into daily nc file;
#2. Extract site-specific HCHO and NO2 TropOMI;
#3. Calculate anomalies of HCHO and NO2 in summer of 2022 and FNR in 2022;
#4. Extract site-specific anomalies of HCHO and NO2.

#----sub-script 1: Convert the raw S5P TROPOMI HCHO nc file into daily nc file that can be processed by CDO----
hcho<-list.files("/home/zhengh/AP/O3/s5p/HCHO/", pattern = "SUB.nc4", full.names = T)

seq<-unique(substr(unlist(strsplit(hcho, "HCHO___"))[seq(2,2*length(hcho),2)], 1,9))
for (i in 1:length(seq)){
  input<-list.files("/home/zhengh/AP/O3/s5p/HCHO/", pattern = paste0("L2__HCHO___", seq[i]), full.names = T)
  out<-list()
  for (j in 1:length(input)){
    nc<-nc_open(input[j])
    lon<-as.vector(ncvar_get(nc, varid = "PRODUCT/longitude"))
    lat<-as.vector(ncvar_get(nc, varid = "PRODUCT/latitude"))
    values<-as.vector(ncvar_get(nc, varid = "PRODUCT/formaldehyde_tropospheric_vertical_column"))
    qa<-as.vector(ncvar_get(nc, varid = "PRODUCT/qa_value"))
    values[which(qa<0.5)]<-NA
    values[which(values<0.000000116625)]<-NA
    nc_close(nc)
    df<-NULL
    df<-rbind(df,data.frame(lon=lon,
                            lat=lat,
                            val=values))
    out[[j]]<-na.omit(df)
    print(j)
  }
  out<-do.call(rbind, out)
  out$lon<-plyr::round_any(out$lon, 0.1)
  out$lat<-plyr::round_any(out$lat, 0.1)
  out<-ddply(out, c("lon", "lat"), summarise, HCHO=mean(val, na.rm=T))
  
  
  if(nrow(out)>0){
    grid<-data.frame(expand.grid(seq(72, 135, 0.1), seq(15, 55, 0.1)))
    names(grid)<-c("lon", "lat")
    
    out<-merge(grid,
               out,
               by=c("lat", "lon"),
               all.x=T)
    
    
    longitude<-seq(72, 135, 0.1)
    latitude<-seq(15, 55, 0.1)
    date<-as.numeric(ymd(substr(seq[i], 1, 8)))-730
    lon<-ncdim_def("lon", "degrees_east", vals = longitude)
    lat<-ncdim_def("lat", "degrees_north", vals = latitude)
    time<-ncdim_def("Time", "days since 1972-1-1", date)
    
    HCHO<-ncvar_def(name="HCHO", units="mol/m^2", dim=list(lon, lat,time), missval=NA, prec="float")
    out_nc<-nc_create(file=paste0("/home/zhengh/AP/O3/s5p/HCHO/grid_01/HCHO_", seq[i], ".nc"), vars=list(HCHO))
    
    ncvar_put(nc=out_nc, varid = HCHO, vals = out$HCHO)
    nc_close(out_nc)
    print(seq[i])
    
  }
}


no2<-list.files("/home/zhengh/AP/O3/s5p/NO2/", pattern = "SUB.nc4", full.names = T)
seq<-unique(substr(unlist(strsplit(no2, "NO2____"))[seq(2,2*length(no2),2)], 1,9))
for (i in 1:3){
  input<-paste0(list.files("/home/zhengh/AP/O3/s5p/NO2/", pattern = seq[i], full.names = T))
  out<-list()
  for (j in 1:length(input)){
    nc<-nc_open(input[j])
    lon<-as.vector(ncvar_get(nc, varid = "PRODUCT/longitude"))
    lat<-as.vector(ncvar_get(nc, varid = "PRODUCT/latitude"))
    values<-as.vector(ncvar_get(nc, varid = "PRODUCT/nitrogendioxide_tropospheric_column"))
    qa<-as.vector(ncvar_get(nc, varid = "PRODUCT/qa_value"))
    values[which(qa<0.5)]<-NA
    values[which(values<0)]<-NA
    nc_close(nc)
    df<-NULL
    df<-rbind(df,data.frame(lon=lon,
                            lat=lat,
                            val=values))
    out[[j]]<-na.omit(df)
    print(j)
  }
  out<-do.call(rbind, out)
  out$lon<-plyr::round_any(out$lon, 0.1)
  out$lat<-plyr::round_any(out$lat, 0.1)
  out<-ddply(out, c("lon", "lat"), summarise, NO2=mean(val, na.rm=T))
  
  
  grid<-data.frame(expand.grid(seq(72, 135, 0.05), seq(15, 55, 0.05)))
  names(grid)<-c("lon", "lat")
  
  out<-merge(grid,
             out,
             by=c("lat", "lon"),
             all.x=T)
  
  
  longitude<-seq(72, 135, 0.1)
  latitude<-seq(15, 55, 0.1)
  date<-as.numeric(ymd(substr(seq[i], 1, 8)))-730
  lon<-ncdim_def("lon", "degrees_east", vals = longitude)
  lat<-ncdim_def("lat", "degrees_north", vals = latitude)
  time<-ncdim_def("Time", "days since 1972-1-1", date)
  
  NO2<-ncvar_def(name="NO2", units="mol/m^2", dim=list(lon, lat,time), missval=NA, prec="float")
  out_nc<-nc_create(file=paste0("/home/zhengh/AP/O3/s5p/NO2/grid_01/NO2_", seq[i], ".nc"), vars=list(NO2))
  ncvar_put(nc=out_nc, varid = NO2, vals = out$NO2)
  nc_close(out_nc)
  print(seq[i])
}
#----sub-script 1: Convert the raw S5P TROPOMI HCHO nc file into daily nc file that can be processed by CDO----


##----sub-script 2 Extract site-specific HCHO and NO2 TropOMI----
#Load all available site from 2014 to 2022
site<-read.csv("/home/zhengh/AP/O3/s5p/AP_site.csv")

#For HCHO
setwd("/home/zhengh/AP/O3/s5p/HCHO/grid_01")
cmd<-paste0("/home/zhengh/apps/bin/cdo copy HCHO_20*.nc HCHO.nc")
system (cmd)

hcho<-list()
for (i in 1:nrow(site)){
  cmd<-paste0("/home/zhengh/apps/bin/cdo -outputtab,date,value -remapbil,lon=", site$lon[i], "_lat=", site$lat[i],
              " ./HCHO.nc >./temp.csv")
  system(cmd)
  input<-data.table::fread("./temp.csv", select = 1:2)
  names(input)[1:2]<-c("date", "hcho")
  input$code<-site$code[i]
  hcho[[i]]<-input
  print(i)
}
hcho<-do.call(rbind, hcho)


#For NO2
setwd("/home/zhengh/AP/O3/s5p/NO2/grid_01")
cmd<-paste0("/home/zhengh/apps/bin/cdo copy NO2_20*.nc NO2.nc")
system (cmd)

no2<-list()
for (i in 1:nrow(site)){
  cmd<-paste0("/home/zhengh/apps/bin/cdo -outputtab,date,value -remapbil,lon=", site$lon[i], "_lat=", site$lat[i],
              " ./NO2.nc >./temp.csv")
  system(cmd)
  input<-data.table::fread("./temp.csv", select = 1:2)
  names(input)[1:2]<-c("date", "no2")
  input$code<-site$code[i]
  no2[[i]]<-input
  print(i)
}
no2<-do.call(rbind, no2)


#Merge site-specific HCHO and NO2 data for further analysis
FNR<-merge(hcho, no2, by=c("date", "code"))
FNR$fnr<-FNR$hcho/fnr$no2
FNR<-data.frame(FNR)
FNR$date<-ymd(FNR$date)
FNR<-merge(FNR, site, by="code")
save(fnr, file="/home/zhengh/AP/O3/FNR.Rdata")
##----sub-script 2 Extract site-specific HCHO and NO2 TropOMI----



###----sub-script 3: Calculate anomalies of HCHO and NO2 in summer of 2022 and FNR in 2022----
#Calculate anomalies of HCHO in summer of 2022 (mean_2022-mean_2018-2021)
setwd("/home/zhengh/AP/O3/s5p/HCHO/grid_01/")
cmd1<-paste0("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2018/2021 HCHO.nc ./mean_2018-2021")
system(cmd1)
cmd2<-paste0("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2022 HCHO.nc ./mean_2022")
system(cmd2)
cmd3<-paste0("/home/zhengh/apps/bin/cdo sub mean_2022 mean_2018-2021 ./dif.nc")
system(cmd3)
cmd4<-paste0("/home/zhengh/apps/bin/cdo -ifthen China_mask_01.nc dif ./Delta_HCHO.nc")
system(cmd4)


#Calculate anomalies of NO2 in summer of 2022 (mean_2022-mean_2018-2021)
setwd("/home/zhengh/AP/O3/s5p/NO2/grid_01/")
cmd1<-paste0("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2018/2021 NO2.nc ./mean_2018-2021")
system(cmd1)
cmd2<-paste0("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2022 NO2.nc ./mean_2022")
system(cmd2)
cmd3<-paste0("/home/zhengh/apps/bin/cdo sub mean_2022 mean_2018-2021 ./dif.nc")
system(cmd3)
cmd4<-paste0("/home/zhengh/apps/bin/cdo -ifthen China_mask_01.nc dif ./Delta_NO2.nc")
system(cmd4)


#Calculate HCHO to NO2 ratio (FNR) in summer of 2022
setwd("/home/zhengh/AP/O3/s5p")
cmd<-paste0("/home/zhengh/apps/bin/cdo -ifthen Chian_01_mask -div ./HCHO/grid_01/mean_2022 ./NO2/grid_01/mean_2022 ./FNR.nc")
system(cmd)
###----sub-script 3: Calculate anomalies of HCHO and NO2 in summer of 2022 and FNR in 2022----



####----sub-script 4: Extract site-specific anomalies of HCHO and NO2----
site<-read.csv("/home/zhengh/AP/site_a.csv")
setwd("/home/zhengh/AP/O3/s5p/HCHO/grid_01/")
delta_HCHO<-list()
for(i in 1:dim(site)){
  cmd<-paste0("/home/zhengh/apps/bin/cdo -outputtab,lon,lat,value -remapbil,lon=",site$lon[i], "_lat=", site$lat[i],
              " dif >temp.csv")
  system(cmd)
  input<-data.table::fread("./temp.csv", select = 1:3)
  input$code<-site$code[i]
  delta_HCHO[[i]]<- input
  print(i)
}
delta_HCHO<-do.call(rbind, delta_HCHO)
names(delta_HCHO)<-c("lon", "lat", "HCHO", "code")
save(delta_HCHO, file="/home/zhengh/AP/O3/delta_HCHO.Rdata")


setwd("/home/zhengh/AP/O3/s5p/NO2/grid_01/")
delta_NO2<-list()
for(i in 1:dim(site)){
  cmd<-paste0("/home/zhengh/apps/bin/cdo -outputtab,lon,lat,value -remapbil,lon=",site$lon[i], "_lat=", site$lat[i],
              " dif >temp.csv")
  system(cmd)
  input<-data.table::fread("./temp.csv", select = 1:3)
  input$code<-site$code[i]
  delta_NO2[[i]]<- input
  print(i)
}
delta_NO2<-do.call(rbind, delta_NO2)
names(delta_NO2)<-c("lon", "lat", "NO2", "code")
save(delta_NO2, file="/home/zhengh/AP/O3/delta_NO2.Rdata")
####----sub-script 4: Extract site-specific anomalies of HCHO and NO2----



