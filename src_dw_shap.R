#!/usr/bin/Rscript
library(reshape2)
library(lubridate)
library(plyr)
library(normalweatherr)
library(enlightenr)
library(shapFlex)
library(randomForest)
library(openair)
site<-read.csv("/home/zhengh/AP/site_a.csv")

vars<-c("t2m", "msl", "u10", "v10", "rh", "tp", "blh", "ssr", "tcc")



for (i in 1:nrow(site)){
  #--------------------load air pollutant data-----------------------             
  ap<-data.table::fread(paste0("/home/zhengh/AP/O3/AP/", site$code[i], ".csv"))
  ap<-ap[, c("date", "O3")]
  ap<-na.omit(ap)
  ap$date<-ymd_hms(ap$date)
  
  #--------------------merge meteorological conditions---------------
  met<-list()
  for (j in 1:length(vars)){
    input<-data.table::fread(paste0("/home/zhengh/m_site/", vars[j], "/", site$code[i], ".csv"))
    input$var<-vars[j]
    met[[j]]<-input
  }
  met<-do.call(rbind, met)
  met$date<-lubridate::ymd_hms(met$date)+28800
  met<-dcast(met, date~var, value.var = "value", mean)
  met[which(met$ssr<0), "ssr"]<-0
  met[which(met$tp<0), "tp"]<-0
  
  #--------------------merge met and ap data-----------------------
  out<-merge(ap, met, by="date")
  out$month<-month(out$date)
  out<-subset(out, month==6| month==7 | month==8)
  out<-na.omit(out[, 1:11])
  
  #--------------------dewather processing ----------------------
  dw <- add_date_variables(out, impute = F)
  dw$value<-dw[, "O3"]
  list_input_data <- split_input_data(dw, fraction = 0.7)#split into the training and testing sets
  # Build model
  # What variables will be used?
  variables <- c("t2m", "msl", "u10", "v10", "rh", "tp", "blh", "ssr", "tcc", "hour", "day_julian", "date_unix", "weekday")
  # Build the random forest model
  set.seed(1)
  model_rf <- calculate_model(
    list_input_data, 
    variables = variables,
    mtry = 3,
    nodesize = 3,
    model = "rf",
    ntree = 300,
    verbose = F)
  
  # Normalise for meteorology
  # Implements parallel processing to speed things up
  register_cores(10)
  data_normalised <- normalise_for_meteorology(
    model_rf$model,
    dw,
    variables = setdiff(variables, c("hour", "day_julian", "date_unix", "weekday")),
    n = 1000
  )


  names(data_normalised)[2]<-"EMI"
  out<-merge(out, data_normalised, by="date")
  out$MET<-out$O3-out$EMI
  
  write.csv(out, paste0(file="/home/zhengh/AP/O3/rf/", site$code[i], ".csv"), row.names = F)

  #------------------------------------------------------------------------------
  # Train a machine learning model; currently limited to single outcome regression and binary classification.
  
  model_formula <- formula(MET~t2m+msl+u10+v10+rh+tp+blh+ssr+tcc)
  
  model <- randomForest::randomForest(model_formula,
                                      data = out, 
                                      ntree = 300, 
                                      mtry = 3, 
                                      nodesize = 3)
  
  predict_function <- function(model, data) {
    data_pred <- data.frame("y_pred" = predict(model,data, type = "response"))
    return(data_pred)
  }
  #------------------------------------------------------------------------------
  # shapFlex setup.
  explain <- out[, -c(1,2)]  # Compute Shapley feature-level predictions for 300 instaces.

  #reference <- out[, 2]  # An optional reference population to compute the baseline prediction.
  target_features <- c("t2m", "msl", "u10", "v10", "rh", "tp", "blh", "ssr", "tcc")
  set.seed(1)
  shap <- shapFlex::shapFlex(explain = explain,
                             #reference = reference,
                             model = model,
                             predict_function = predict_function(model, out),
                             target_features = target_features,
                             sample_size = 60)

  shap$date<-rep(out$date, each=9)
  shap<-dcast(shap, date+intercept~feature_name, value.var = "shap_effect")
  write.csv(shap, paste0("/home/zhengh/AP/O3/shap/", site$code[i], ".csv"), row.names = F)
  
  

  # r_model<-data.frame(site = site$code[i],
  #                          r_dw = cor(x=model_rf$model$y, y=model_rf$model$predicted),
  #                          r_shap = cor(x=model$y, y=model$predicted))
  
  dw_model<-data.frame(obs=model_rf$model$y,
                       mod=model_rf$model$predicted)
  
  shap_model<-data.frame(obs=model$y,
                         mod=model$predicted)
  
  stats<-data.frame(cbind(t(modStats(dw_model, obs="obs", mod="mod")[2:10]),
                          t(modStats(shap_model, obs="obs", mod="mod")[2:10])))
  
  colnames(stats)<-c("dw", "shap")
  stats$vars<-row.names(stats)
  
  imp_dw<-data.frame(model_rf$model$importance)
  imp_dw$var<-row.names(imp_dw)
  imp_dw$code<-site$code[i]
  
  imp_shap<-data.frame(model$importance)
  imp_shap$var<-row.names(imp_shap)
  imp_shap$code<-site$code[i]
  
  write.csv(stats, file=paste0("/home/zhengh/AP/O3/stats/", site$code[i], ".csv"), row.names = F)
  write.csv(imp_dw, file=paste0("/home/zhengh/AP/O3/imp/dw/", site$code[i], ".csv"), row.names = F)
  write.csv(imp_shap, file=paste0("/home/zhengh/AP/O3/imp/shap/", site$code[i], ".csv"), row.names = F)
  
  
  print(site$code[i])
  }
  
setwd("/home/zhengh/AP/O3/rf")
get_emi<-function(.file){
  code<-substr(unlist(strsplit(.file, "/"))[2], 1, 5)
  input<-data.table::fread(.file)
  input<-input[, c("date", "EMI", "MET")]
  input$code<-code
  input
}


emi<-lapply(list.files(".", pattern = ".csv", full.names = T), get_emi)
emi<-do.call(rbind, emi)
emi<-melt(emi, measure.vars = 2:3, variable.name = "var", value.name = "conc")
emi<-openair::timeAverage(emi, avg.time = "year", type=c("code", "var"))
emi<-dcast(emi, code+var~date, value.var = "conc")

emi$mean<-apply(emi[, 3:10], 1, mean)
emi$delta<-emi$`2022-01-01`-emi$mean
emi<-merge(emi, site, by="code")
head(emi)
save(emi, file="/home/zhengh/AP/O3/EMI.Rdata")

setwd("/home/zhengh/AP/O3/shap/")
get_shap<-function(.file){
  code<-substr(unlist(strsplit(.file, "/"))[2], 1, 5)
  input<-data.table::fread(.file)
  input$code<-code
  input
}


shap<-lapply(list.files(".", pattern = ".csv", full.names = T), get_shap)
shap<-do.call(rbind, shap)


shap<-melt(shap[, -"intercept"], measure.vars = 2:10, variable.name = "var", value.name = "conc")
shap<-openair::timeAverage(shap, avg.time = "year", type=c("code", "var"))
shap<-dcast(shap, code+var~date, value.var = "conc")

shap$mean<-apply(shap[, 3:10], 1, mean)
shap$delta<-shap$`2022-01-01`-shap$mean
shap<-merge(shap, site, by="code")
save(shap, file="/home/zhengh/AP/O3/SHAP.Rdata")

#get yearly EMI and MET data
get_ts<-function(.file){
  code<-substr(unlist(strsplit(.file, split = "rf"))[2], 3, 7)
  input<-data.table::fread(.file)
  input$code<-code
  input
}

ts<-lapply(list.files("/home/zhengh/AP/O3/rf/", pattern = ".csv", full.names = T), get_ts)
ts<-do.call(rbind, ts)

site<-read.csv("/home/zhengh/AP/site_a.csv")
ts<-merge(ts[, c("date", "EMI", "MET", "code")], site[, c("code", "region")], by="code")
ts$year<-year(ts$date)

get_stats<-function(data){
  emi<-t.test(data$EMI)
  met<-t.test(data$MET)
  out<-data.frame(mean_emi=as.numeric(emi$estimate),
                  up_emi=as.numeric(emi$conf.int)[2],
                  low_emi=as.numeric(emi$conf.int)[1],
                  mean_met=as.numeric(met$estimate),
                  up_met=as.numeric(met$conf.int)[2],
                  low_met=as.numeric(met$conf.int)[1])
  out
}
ts<-ddply(ts, c("year", "region"), get_stats)
save(ts, file="/home/zhengh/AP/O3/TS.Rdata")


#Get pearson coefficient between shap values of different meteorological variable.
setwd("/home/zhengh/AP/O3/shap")
get_coef_shap<-function(.file){
  code<-substr(.file, 4,8)
  input<-data.table::fread(.file)
  r<-data.frame(cor(input[, 3:11]))
  r$var1<-row.names(r)
  r<-melt(r, measure.vars = 1:9, variable.name = "var2", value.name = "coef")
  r[which(r$coef==1), "coef"]<-NA
  r$code<-code
  r
  
}

coef<-lapply(list.files("./", pattern = ".csv", full.names = T), get_coef_shap)
coef<-do.call(rbind, coef)
coef<-merge(coef, site, by="code")
coef$var1<-toupper(coef$var1)
coef$var2<-toupper(coef$var2)
coef$var1<-factor(coef$var1, levels = c("T2M", "MSL", "U10", "V10", "RH", "TP", "BLH", "TCC", "SSR"))
coef$var2<-factor(coef$var2, levels = c("T2M", "MSL", "U10", "V10", "RH", "TP", "BLH", "TCC", "SSR"))

save(coef, file="/home/zhengh/AP/O3/SHAP_coef.Rdata")

