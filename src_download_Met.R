#Script 1
# Downloading meteorological conditions from ERA-5
#Notes: Create an ECMWF account by self registering and retrieving your key at https://api.ecmwf.int/v1/key/ after you log in.

library(ecmwfr)
library(lubridate)

wf_set_key(user = "××××××",#user name 
           key = "××××××××××××××××××××××××××××××", #key
           service = "cds")

met<-c ("2m_temperature",
        "2m_dewpoint_temperature",
        "mean_sea_level_pressure",
        "10m_u_component_of_wind",
        "10m_v_component_of_wind",
        "total_precipitation",
        "boundary_layer_height",
        "surface_net_solar_radiation",
        "total_cloud_cover")

#direction name 
outname<-c("t2m", "d2m","msl", "u10", "v10", "tp", "blh", "ssr", "tcc") 
#Pathway to store the meteorological data
dir<-"/home/data/ERA-5/res_0.25/" 

for (year in seq(2014, 2021, 1)){#specify the start an end years and month interval (1 month)
  for (i in 1:9){
    for (month in c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12)){
      if (month %in% c(01, 03, 05, 07, 08, 10, 12) ==T) {
        day=c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10,
              11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
              21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
      } else if(month %in% c(04, 06, 09, 11) ==T){
        day=c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10,
              11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
              21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
      } else if (month==02){
        if (leap_year(year)==T){
          day=c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10,
                11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                21, 22, 23, 24, 25, 26, 27, 28, 29)
        } else (day=c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10,
                      11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                      21, 22, 23, 24, 25, 26, 27, 28))
      }
      
      request <- list("dataset_short_name" = "reanalysis-era5-single-levels",
                      "product_type" = "reanalysis",
                      "variable" = met[i],
                      "year" = year,
                      "month" = month,
                      "day" = day,
                      "time" = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00",
                                 "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00",
                                 "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00",
                                 "21:00", "22:00", "23:00"),
                      #"area" = "-180/180/-90/90",
                      "format" = "netcdf",
                      "grid" = "0.25/0.25",
                      "target" = paste0(outname[i], "_", year,"_", month, ".nc"))
      
      file <- wf_request(user = "××××××",   # user ID (for authentification)
                         request = request,  # the request
                         transfer = TRUE,     # download the file
                         time_out = 7200,
                         path = paste0("/home/data/ERA-5/res_0.25/", outname[i]))
    }
    
  }
  
}
