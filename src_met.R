#src-met.R
#This script is used to process the meteorological variables and generate data to visualization.
#NOtes: One should install CDO (https://code.mpimet.mpg.de/projects/cdo) prior to use this script.
#Five sub-script in this script to 
#1. Calculate relative humidity using T2M&D2M;
#2. Calculate anomalies of daily maximum temperature at 2 m (MDT2M) in summer of 2022 compared to that in 2014-2021;
#3. Calculate anomalies of meteorological variables in summer of 2022 compared to 2014-2021;
#4. Extract anomalies of site-specific t2m at 13:00 (5:00 UTC) and 14:00 (6:00 UTC) LT in summer of 2022 compared to 2018-2021;
#5. Extract anomalies of site-specific met between summer in 2022 and summer in 2014-2021.


#----sub-script 1: Calculate relative humidity using T2M and D2M----
setwd("/home/data/ERA-5/res_0.25")
seq<-data.frame(year=rep(seq(2014, 2022, 1), each=3),
                month=seq(6, 8, 1))

for (i in 1:nrow(seq)){
  year<-seq$year[i]
  month<-seq$month[i]
  
  t2m<-paste0("./t2m/t2m_", year,"_", month, ".nc")
  d2m<-paste0("./d2m/d2m_", year,"_", month, ".nc")
  
  cmd1<-paste0("/home/zhengh/apps/bin/cdo merge ", t2m, " ", d2m, " ./temp.nc")
  system(cmd1)
  cmd2<-paste0("/home/zhengh/apps/bin/cdo expr,'rh=exp(((597.3-0.57*(t2m-273.15))/0.1102)*(1/t2m-1/d2m))'",
               " ./temp.nc",
               " ./rh/rh_", year, "_", month, ".nc")
  system(cmd2)
  cmd3<-paste0("rm ./res_0.25/temp.nc")
  system(cmd3)
}
#----Calculate relative humidity using T2M and D2M----

##----sub-script 2: Calculate anomalies of  daily maximum temperature at 2 m (MDT2M) in summer of 2022 compared to that in 2014-2021----
setwd("/home/data/ERA-5/res_0.25/t2m/")

seq<-data.frame(year=rep(seq(2014, 2022, 1), each=3),
               month=seq(6, 8, 1))

#Select daily maximum T2M and save it to temp file.
for (i in 1:nrow(seq)){
  year<-seq$year[i]
  month<-seq$month[i]
  
  cmd1<-paste0("/home/zhengh/apps/bin/cdo daymax t2m_", year, "_", month, 
               ".nc ./temp_", year, "_", month, ".nc")
  system(cmd1)
  print(i)
}
#Merge all temp file into one file
system(paste0("/home/zhengh/apps/bin/cdo -b F32 copy temp_* ./merged.nc"))

#Calculating seasonal mean value of MDT2M from 2014 to 2021
system(paste0("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2014/2021, merged.nc mean_2014-2021")) 

#Calculating seasonal mean value of MDT2M in 2022 
system(paste0("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2022, merged.nc mean_2022")) 

#Calculating the anomalies
system(paste0("/home/zhengh/apps/bin/cdo sub mean_2022 mean_2014-2021 dif.nc"))

#Extract China region with mask in metdata
system(paste0("/home/zhengh/apps/bin/cdo ifthen ./China_mask_0.25 dif.nc ./MDT2M.nc"))


#Remove the intermedia output files
system(paste0("rm ./temp_*.nc"))
system(paste0("rm ./merged*.nc"))
system(paste0("rm ./mean_*.nc"))
##----sub-script 2: Calculate anomalies of  daily maximum temperature at 2 m (MDT2M) in summer of 2022 compared to that in 2014-2021----



###----sub-script 3: Calculate anomalies of meteorological variables in summer of 2022 compared to 2014-2021----
vars<-c("t2m", "msl", "u10", "v10", "rh", "tp", "blh", "ssr", "tcc")

for (i in 1:length(vars)){
  setwd(paste0("/home/data/ERA-5/res_0.25/", vars[i]))
  
  for (j in 1:nrow(seq)){
    year<-seq$year[j]
    month<-seq$month[j]
    
    #calculate monthly mean value of meteorological parameters
    cmd1<-paste0("/home/zhengh/apps/bin/cdo monmean ./", vars[i], "_", year, "_", month, 
                 ".nc ./monmean_", year, "_", month, ".nc" )
    system(cmd1) 
  }
  
  #merged monthly mean value into a file
  cmd2<-paste0("/home/zhengh/apps/bin/cdo -b F32 copy monmean_", "*.nc ./", vars[i], "_merged.nc")
  system(cmd2)
  
  #remove monthly mean files
  cmd3<-paste0("rm ./monmean_*.nc")
  system(cmd3)
  
}

#Merge different meteorology into a nc file

setwd("/home/data/ERA-5/res_0.25/")
cmd4<-paste0("/home/zhengh/apps/bin/cdo merge ./u10/u10_merged.nc ./msl/msl_merged.nc ./u10/u10_merged.nc ./v10/v10_merged.nc ./rh/rh_merged.nc
              ./tp/tp_merged.nc ./blh/blh_merged.nc ./tcc/tcc_merged.nc ./ssr/ssr_merged.nc ./met_merged.nc")
system(cmd4)

#calculate summer mean values from 2014 to 2021

cmd5<-("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2014/2021 met_merged.nc mean_2014_2021.nc")
system(cmd5)


#calculate summer mean values from 2022
cmd6<-("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2022 met_merged.nc mean_2022.nc")
system(cmd6)

#calculate anomalies of met
cmd7<-paste0("/home/zhengh/apps/bin/cdo sub mean_2022.nc mean_2014_2021.nc met_dif.nc")
system(cmd7)

#Extract China region data
cmd8<-paste0("/home/zhengh/apps/bin/cdo ifthen China_mask_0.25 met_dif.nc Delta_Met.nc")
system(cmd8)


###----sub-script 3: Calculate anomalies of meteorological variables in summer of 2022 compared to 2014-2021----



####----sub-script 4: Extract anomalies of site-specific t2m at 13:00 (5:00 UTC) and 14:00 (6:00 UTC) LT in summer of 2022 compared to 2018-2021----
setwd("/home/data/ERA-5/res_0.25/t2m/")
seq<-data.frame(year=rep(seq(2018, 2022, 1), each=3),
                month=seq(6, 8, 1))

for (i in 1:nrow(seq)){
  year<-seq$year[i]
  month<-seq$month[i]
  
  cmd1<-paste0("/home/zhengh/apps/bin/cdo selhour,5,6 t2m_", year, "_", month, ".nc ./temp_", year, "_", month, ".nc")
  
  system(cmd1)
  
}

cmd2<-paste0("/home/zhengh/apps/bin/cdo -b F32 copy ./temp_*.nc ./t_merged.nc")

cmd3<-paste0("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2014/2021 t_merged.nc t_mean_2014-2021.nc")
system(cmd3)

cmd4<-paste0("/home/zhengh/apps/bin/cdo -yseasmean -selyear,2022 t_merged.nc t_mean_2022.nc")
system(cmd4)



cmd5<-paste0("/home/zhengh/apps/bin/cdo sub t_mean_2022.nc t_mean_2018-2021.nc t_dif.nc")
system(cmd5)

site<-read.csv("/home/zhengh/AP/site_a.csv")
S5P_T2M<-list()
for(i in 1:nrow(site)){
  cmd<-paste0("/home/zhengh/apps/bin/cdo -outputtab,lon,lat,value -remapbil,lon=",site$lon[i], "_lat=", site$lat[i],
              " t_dif >temp.txt")
  system(cmd)
  input<-data.table::fread("./temp.txt", select = 1:3)
  names(input)[1:3]<-c("lon", "lat", "T2M")
  input$code<-site$code[i]
  S5P_T2M[[i]]<- input
  print(i)
}
S5P_T2M<-do.call(rbind, S5P_T2M)
save(S5P_T2M, file="/home/zhengh/AP/O3/S5P_T2M.Rdata")

system(paste0("rm ./t_*"))
system((paste0("rm ./temp*")))


####----sub-script 4: Extract anomalies of site-specific t2m at 13:00 (5:00 UTC) and 14:00 (6:00 UTC) LT in summer of 2022 compared to 2018-2021----



#####----sub-script 5: Extract anomalies of site-specific met between summer in 2022 and summer in 2014-2021----

site<-read.csv("/home/zhengh/AP/site_a.csv")
Delta_Met<-list()
setwd("/home/data/ERA-5/res_0.25/")
for (i in 1:nrow(site)){
  cmd<-paste0("/home/zhengh/apps/bin/cdo -outputtab,name,value -remapbil,lon=", site$lon[i], "_lat=", site$lat[i], " ./Delta_Met.nc >temp.txt")
  system(cmd)
  input<-data.table::fread("./temp.txt", select = 1:2)
  names(input)[1:2]<-c("met", "delta")
  input[which(input$met=="rh"), "delta"]<-input[which(input$met=="rh"), "delta"]*100#convert to %
  input[which(input$met=="tp"), "delta"]<-input[which(input$met=="tp"), "delta"]*1000#from m to mm
  input[which(input$met=="ssr"), "delta"]<-input[which(input$met=="ssr"), "delta"]/10^6#from J m-2 to MJ m-2
  input$code<-site$code[i]
  input[which(input$delta<(-1000)), "delta"]<-NA
  Delta_Met[[i]]<-input
  print(i)
}
Delta_Met<-do.call(rbind, Delta_Met)
save(Delta_Met, file="/home/zhengh/AP/O3/Delta_Met.Rdata")


#####----sub-script 5: Extract anomalies of site-specific met between summer in 2022 and summer in 2014-2021----





