#Fig.5& Fig.S9: Delta Met and SHAP
setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
library(ncdf4)
library(ggplot2)
library(maptools)
library(gtable)
library(plyr)
library(cowplot)
library(ggplotify)
library(magrittr)
library(ggnewscale)

#load China map
china<-readShapePoly("./Metadata/china.shp")
china<-fortify(china)

#load South China Sea region
scs<-readShapeLines("./Metadata/scs.shp")
scs<-fortify(scs)

#Define Fenwei Plain region
FWP<-data.frame(x=c(106.25, 111.25, 111.25, 113.75, 113.75, 108.75, 108.75, 106.25),
                y=c(33, 33, 35, 35, 37, 37, 35, 35))

SCS<-ggplotGrob(
                ggplot()+geom_line(data=scs,aes(x=long,y=lat,group=group),color="black",size=0.2)+
                  geom_path(data=china, aes(long, lat, group=group), size=0.2)+
                  scale_x_continuous(expand = c(0, 0), limits = c(105, 125))+
                  scale_y_continuous(expand = c(0, 0), limits=c(3, 24))+
                  theme_bw()+
                  theme(axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title = element_blank(),
                        panel.grid = element_blank(),
                        plot.margin = unit(rep(0, 4), "npc"))
                )


nc<-nc_open("./Fig_data/Delta_Met.nc")
lon<-ncvar_get(nc, varid = "longitude")
lat<-ncvar_get(nc, varid = "latitude")


met_dif<-data.frame(expand.grid(lon, lat))
names(met_dif)[1:2]<-c("lon", "lat")
met_dif$T2M<-as.numeric(ncvar_get(nc, varid = "t2m"))
met_dif$MSL<-as.numeric(ncvar_get(nc, varid = "msl"))
met_dif$U10<-as.numeric(ncvar_get(nc, varid = "u10"))
met_dif$V10<-as.numeric(ncvar_get(nc, varid = "v10"))
met_dif$RH<-as.numeric(ncvar_get(nc, varid = "rh"))*100
met_dif$TP<-as.numeric(ncvar_get(nc, varid = "tp"))*1000#convert to mm
met_dif$BLH<-as.numeric(ncvar_get(nc, varid = "blh"))
met_dif$TCC<-as.numeric(ncvar_get(nc, varid = "tcc"))
met_dif$SSR<-as.numeric(ncvar_get(nc, varid = "ssr"))/10^6#convet to MJ/m2
nc_close(nc)

# nc<-nc_open("/home/data/ERA-5/rh_dif_mask")
# met_dif$RH<-as.numeric(ncvar_get(nc, varid = "rh"))*100#convert to %

met_fig<-list()
met_dif<-reshape2::melt(met_dif, 
                        measure.vars = c("T2M", "MSL", "U10", "V10", "RH", "TP", "BLH", "TCC", "SSR"), 
                        variable.name = "met", value.name = "delta")

load("./Fig_data/SHAP.Rdata")
levels(shap$region)[levels(shap$region)=="HHB"]<-"THB"

shap_delta<-shap
shap_delta$var<-toupper(shap_delta$var)
shap_delta$var<-factor(shap_delta$var, levels = c("T2M", "MSL", "U10", "V10", "RH", "TP", "BLH", "TCC", "SSR"))

fill_lab<-c(expression(Delta*" T2M ("^o*"C)"),
            expression(Delta* " MSL (Pa)"),
            expression(Delta* " U10 (m s"^-1*")"),
            expression(Delta* " V10 (m s"^-1*")"),
            expression(Delta* " RH (%)"),
            expression(Delta* " TP (mm)"),
            expression(Delta* " BLH (m)"),
            expression(Delta* " TCC (unitless)"),
            expression(Delta* " SSR (MJ m"^-2*")"))



color_lab<-c(expression(Delta* " SHAP"[T2M]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[MSL]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[U10]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[V10]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[RH]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[TP]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[BLH]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[TCC]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[SSR]* " ("*mu*g*" m"^-3*")"))

met_fig<-list()
shap_box<-list()
shap$var<-factor(shap$var, levels = c("t2m", "msl", "u10", "v10", "rh", "tp", "blh", "tcc", "ssr"))


for (i in 1:9){
  p1<-ggplotGrob(
    ggplot()+
      geom_tile(data=subset(na.omit(met_dif), met==levels(met_dif$met)[i]),
                aes(lon, lat, fill=delta))+
      scale_fill_gradient2(low="#4D9221", mid = "white", high="#C51B7D", midpoint = 0)+
      new_scale_fill()+
      geom_point(data=subset(shap_delta, var==levels(shap_delta$var)[i]), aes(lon, lat, fill=delta), shape=21, colour="gray", stroke=0.15)+
      scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 0)+
      geom_path(data=china, aes(long, lat, group=group), size=0.1)+
      scale_x_continuous(limits = c(72, 136), expand=c(0, 0), breaks = c(80, 100, 120),
                         labels = c(expression(80^o* E), expression(100^o* E), expression(120^o* E)))+
      scale_y_continuous(limits = c(17, 55), expand=c(0, 0), breaks = c(25, 35, 45),
                         labels = c(expression(25^o* N), expression(35^o* N), expression(45^o* N)))+
      annotate("rect", xmin=114, xmax=118, ymin=37, ymax=41, colour="#F8766D", fill=NA, size=0.5)+
      annotate("rect", xmin=118, xmax=122, ymin=30, ymax=33, colour="#FB617D", fill=NA)+
      annotate("rect", xmin=112, xmax=115.5, ymin=21.5, ymax=24, colour="#00B6EB", fill=NA)+
      annotate("rect", xmin=103.5, xmax=107, ymin=28.5, ymax=31.5, colour="#A58AFF", fill=NA)+
      annotate("rect", xmin=110.75, xmax=114.75, ymin=28.5, ymax=31.5, colour="#53B400", fill=NA)+
      geom_polygon(data=FWP, aes(x, y), colour="#C49A00", fill=NA)+
      theme_bw()+
      theme(legend.key.height = unit(0.2, 'cm'),
            legend.key.width = unit(0.4, 'cm'),
            legend.position = "bottom",
            legend.spacing.x = unit(0.1, 'cm'),#space between different legend
            panel.background = element_blank(),
            axis.title = element_blank())+
      guides(colour=guide_colorbar(title.position = "bottom", 
                                  title.theme = element_text(angle = 0, size=8),
                                  title.hjust = 0.5, 
                                  title = color_lab[i],
                                  order=0),
             colour=guide_colorbar(title.position = "bottom", 
                                 title.theme = element_text(angle = 0, size=8),
                                 title.hjust = 0.5, 
                                 title = fill_lab[i],
                                 order=1)))
  plot_grid(p1)
  
  p2<-ggplotGrob(
    ggplot()+
      geom_tile(data=subset(na.omit(met_dif), met==levels(met_dif$met)[i]),
                aes(lon, lat, fill=delta))+
      scale_fill_gradient2(low="#4D9221", mid = "white", high="#C51B7D", midpoint = 0)+
      geom_point(data=subset(shap_delta, var==levels(shap_delta$var)[i]), aes(lon, lat, colour=delta))+
      scale_color_gradient2(low="blue", mid = "white", high="red", midpoint = 0)+
      geom_path(data=china, aes(long, lat, group=group), size=0.1)+
      theme_bw()+
      theme(panel.background = element_blank(),
            legend.position = "bottom",
            legend.key.height = unit(0.2, 'cm'),
            legend.key.width = unit(0.4, 'cm'),
            axis.title  = element_blank(),
            legend.title = element_text(size=8),
            legend.text = element_text(size=8),
            legend.background = element_blank(),
            legend.box.spacing = unit(0, 'cm'))+
      guides(color=guide_colorbar(title.position = "bottom", 
                                  title.theme = element_text(angle = 0, size=8),
                                  title.hjust = 0.5, 
                                  title = color_lab[i],
                                  order=0),
             fill=guide_colorbar(title.position = "bottom", 
                                  title.theme = element_text(angle = 0, size=8),
                                  title.hjust = 0.5, 
                                  title = fill_lab[i],
                                  order=1)))
  
  
  
  
  panel <- gtable::gtable_filter(p1, "panel")
  
  fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                      heights = unit(rep(0.01, 100), "null"))%>%
    gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
    gtable_add_grob(SCS, t=83, l=2, b=98, r=12)
  
  p1$grobs[[6]]<-fig_embed
  p1$grobs[[15]]<-p2$grobs[[15]]

  
  
  

  data<-subset(shap, region!="OR" & var==levels(shap$var)[i])
  seq<-ddply(data, "region", summarise, mean=mean(delta))
  seq<-seq[order(seq$mean, decreasing = T),]
  
  data$region=factor(data$region, levels = seq$region)
  seq[which(seq$region=="BTH"), "color"]<-"#F8766D"
  seq[which(seq$region=="FWP"), "color"]<-"#C49A00"
  seq[which(seq$region=="THB"), "color"]<-"#53B400"
  seq[which(seq$region=="PRD"), "color"]<-"#00B6EB"
  seq[which(seq$region=="SCB"), "color"]<-"#A58AFF"
  seq[which(seq$region=="YRD"), "color"]<-"#FB617D"
  
  
  
  
  p3<-ggplot(data, aes(region, delta, fill=region))+
    stat_boxplot(geom = "errorbar", width=0.4)+
    geom_boxplot(width=0.4)+
    scale_fill_manual(values = c(seq$color))+
    stat_summary(fun.y="mean", geom="point", shape=22, size=1, fill="white")+
    theme_bw()+
    theme(legend.position = "none",
          #panel.grid = element_blank(),
          axis.title = element_text(size=10),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=7.8))+
    ylab(color_lab[i])
  
  met_fig[[i]]<-as.ggplot(p1)
  shap_box[[i]]<-p3
}



#Fig.5
plot_grid(met_fig[[1]], met_fig[[5]], met_fig[[7]],
          shap_box[[1]], shap_box[[5]], shap_box[[7]], ncol=3,
          labels = letters[1:6], label_size = 11, rel_heights = c(1.4, 1))

export::graph2pdf(file="./Figure /Fig.5.pdf", width=19/2.54, height=12/2.54)


#Fig.S9
plot_grid(met_fig[[2]], met_fig[[3]], met_fig[[4]],
          shap_box[[2]], shap_box[[3]], shap_box[[4]], 
          met_fig[[6]], met_fig[[8]], met_fig[[9]],
          shap_box[[6]], shap_box[[8]], shap_box[[9]],
          ncol=3,
          labels = letters[1:12], label_size = 11, rel_heights = c(1.4, 1))

export::graph2pdf(file="./Figure /Fig.S10.pdf", width=19/2.54, height=24/2.54)



