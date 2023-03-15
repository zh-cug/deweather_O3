#Figure 4
setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
library(ncdf4)
library(reshape2)
library(plyr)



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


#Fig.4a
nc<-nc_open("./Fig_data/FNR.nc")
lon<-ncvar_get(nc, varid = "lon")
lat<-ncvar_get(nc, varid = "lat")
val<-ncvar_get(nc, varid = "HCHO")
nc_close(nc)

regime<-data.frame(expand.grid(as.numeric(lon), as.numeric(lat)))
regime$val<-as.numeric(val)
regime[which(regime$val<2.67), "regime"]<-"VOC"
regime[which(regime$val>3.47), "regime"]<-"NOx"
regime[which(regime$val>=2.67 & regime$val<=3.47), "regime"]<-"TR"


fig.4a<-ggplotGrob(
                  ggplot()+
                    geom_tile(data=na.omit(regime), aes(Var1, Var2, fill=val))+
                    geom_path(data=china, aes(long, lat, group=group), size=0.1)+
                    scale_fill_fermenter(palette = "PiYG",direction = 1, 
                                         breaks=c(1.5, 2, 2.5, 2.67, 3, 3.3, 3.47, 4, 8, 12),
                                         labels=c(1.5, 2, 2.5, 2.67, 3, 3.3, 3.47, 4, 8, 12))+
                    annotate("rect", xmin=114, xmax=118, ymin=37, ymax=41, colour="#F8766D", fill=NA, size=0.5)+
                    annotate("rect", xmin=118, xmax=122, ymin=30, ymax=33, colour="#FB617D", fill=NA)+
                    annotate("rect", xmin=112, xmax=115.5, ymin=21.5, ymax=24, colour="#00B6EB", fill=NA)+
                    annotate("rect", xmin=103.5, xmax=107, ymin=28.5, ymax=31.5, colour="#A58AFF", fill=NA)+
                    annotate("rect", xmin=110.75, xmax=114.75, ymin=28.5, ymax=31.5, colour="#53B400", fill=NA)+
                    geom_polygon(data=FWP, aes(x, y), colour="#C49A00", fill=NA)+
                    scale_x_continuous(limits = c(72, 136), expand=c(0, 0), breaks = c(80, 100, 120),
                                       labels = c(expression(80^o* E), expression(100^o* E), expression(120^o* E)))+
                    scale_y_continuous(limits = c(17, 55), expand=c(0, 0), breaks = c(25, 35, 45),
                                       labels = c(expression(25^o* N), expression(35^o* N), expression(45^o* N)))+
                    theme_bw()+
                    theme(panel.background = element_blank(),
                          legend.position = c(0, 1),
                          legend.justification = c(0, 1),
                          legend.box.spacing = unit(0, 'cm'),
                          legend.background = element_blank(),
                          legend.direction = "horizontal",
                          legend.key.height = unit(0.2, "cm"),
                          legend.key.width = unit(0.8, "cm"),
                          axis.title = element_blank(),
                          legend.text = element_text(size=7),
                          legend.title = element_blank())
                  )


panel <- gtable::gtable_filter(fig.4a, "panel")
panel_layout<-c(subset(fig.4a$layout, name=="panel"))

fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                    heights = unit(rep(0.01, 100), "null"))%>%
             gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
             gtable_add_grob(SCS, t=83, l=2, b=98, r=12)

fig.4a$grobs[[6]]<-fig_embed



#Fig.4b
nc<-nc_open("./Fig_data/Delta_HCHO.nc")
lon<-as.numeric(ncvar_get(nc, varid = "lon"))
lat<-as.numeric(ncvar_get(nc, varid = "lat"))
hcho<-data.frame(expand.grid(lon, lat))
#convet unit from mol/m2 to mole/cm2(10^15)
hcho$HCHO<-as.numeric(ncvar_get(nc, varid = "HCHO"))*6.02214*10^4
nc_close(nc)


fig.4b<-ggplotGrob(
                  ggplot()+
                    geom_tile(data=na.omit(hcho), aes(Var1, Var2, fill=HCHO))+
                    geom_path(data=china, aes(long, lat, group=group), size=0.1)+
                    scale_fill_fermenter(palette = "PiYG", breaks=c(-4, -2, -1, 0, 1, 2, 4))+
                    annotate("rect", xmin=114, xmax=118, ymin=37, ymax=41, colour="#F8766D", fill=NA, size=0.5)+
                    annotate("rect", xmin=118, xmax=122, ymin=30, ymax=33, colour="#FB617D", fill=NA)+
                    annotate("rect", xmin=112, xmax=115.5, ymin=21.5, ymax=24, colour="#00B6EB", fill=NA)+
                    annotate("rect", xmin=103.5, xmax=107, ymin=28.5, ymax=31.5, colour="#A58AFF", fill=NA)+
                    annotate("rect", xmin=110.75, xmax=114.75, ymin=28.5, ymax=31.5, colour="#53B400", fill=NA)+
                    geom_polygon(data=FWP, aes(x, y), colour="#C49A00", fill=NA)+
                    scale_x_continuous(limits = c(72, 136), expand=c(0, 0), breaks = c(80, 100, 120),
                                       labels = c(expression(80^o* E), expression(100^o* E), expression(120^o* E)))+
                    scale_y_continuous(limits = c(17, 55), expand=c(0, 0), breaks = c(25, 35, 45),
                                       labels = c(expression(25^o* N), expression(35^o* N), expression(45^o* N)))+
                    theme_bw()+
                    theme(panel.background = element_blank(),
                          legend.position = c(0, 1),
                          legend.justification = c(0, 1),
                          legend.box.spacing = unit(0, 'cm'),
                          legend.background = element_blank(),
                          legend.direction = "horizontal",
                          legend.key.height = unit(0.2, "cm"),
                          legend.key.width = unit(0.8, "cm"),
                          axis.title = element_blank(),
                          legend.text = element_text(size=7),
                          legend.title = element_blank())
                  )
    



panel <- gtable::gtable_filter(fig.4b, "panel")
panel_layout<-c(subset(fig.4b$layout, name=="panel"))

fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                    heights = unit(rep(0.01, 100), "null"))%>%
             gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
             gtable_add_grob(SCS, t=83, l=2, b=98, r=12)

fig.4b$grobs[[6]]<-fig_embed



#Fig.4c
nc<-nc_open("./Fig_data/Delta_NO2.nc")
lon<-as.numeric(ncvar_get(nc, varid = "lon"))
lat<-as.numeric(ncvar_get(nc, varid = "lat"))
no2<-data.frame(expand.grid(lon, lat))
#convet unit from mol/m2 to mole/cm2(10^15)
no2$no2<-as.numeric(ncvar_get(nc, varid = "NO2"))*6.02214*10^4
nc_close(nc)



fig.4c<-ggplotGrob(
                  ggplot()+
                    geom_tile(data=na.omit(no2), aes(Var1, Var2, fill=no2))+
                    geom_path(data=china, aes(long, lat, group=group), size=0.1)+
                    scale_fill_fermenter(palette = "PiYG", breaks=c(-2, -1, -0.1, 0, 0.1, 0.5, 1, 1.5))+
                    annotate("rect", xmin=114, xmax=118, ymin=37, ymax=41, colour="#F8766D", fill=NA, size=0.5)+
                    annotate("rect", xmin=118, xmax=122, ymin=30, ymax=33, colour="#FB617D", fill=NA)+
                    annotate("rect", xmin=112, xmax=115.5, ymin=21.5, ymax=24, colour="#00B6EB", fill=NA)+
                    annotate("rect", xmin=103.5, xmax=107, ymin=28.5, ymax=31.5, colour="#A58AFF", fill=NA)+
                    annotate("rect", xmin=110.75, xmax=114.75, ymin=28.5, ymax=31.5, colour="#53B400", fill=NA)+
                    geom_polygon(data=FWP, aes(x, y), colour="#C49A00", fill=NA)+
                    scale_x_continuous(limits = c(72, 136), expand=c(0, 0), breaks = c(80, 100, 120),
                                       labels = c(expression(80^o* E), expression(100^o* E), expression(120^o* E)))+
                    scale_y_continuous(limits = c(17, 55), expand=c(0, 0), breaks = c(25, 35, 45),
                                       labels = c(expression(25^o* N), expression(35^o* N), expression(45^o* N)))+
                    theme_bw()+
                    theme(panel.background = element_blank(),
                          legend.position = c(0, 1),
                          legend.justification = c(0, 1),
                          legend.box.spacing = unit(0, 'cm'),
                          legend.background = element_blank(),
                          legend.direction = "horizontal",
                          legend.key.height = unit(0.2, "cm"),
                          legend.key.width = unit(0.8, "cm"),
                          axis.title = element_blank(),
                          legend.text = element_text(size=7),
                          legend.title = element_blank())
                  )

panel <- gtable::gtable_filter(fig.4c, "panel")
panel_layout<-c(subset(fig.4c$layout, name=="panel"))

fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                    heights = unit(rep(0.01, 100), "null"))%>%
  gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
  gtable_add_grob(SCS, t=83, l=2, b=98, r=12)

fig.4c$grobs[[6]]<-fig_embed



#Fig.4d~Fig.4f
load("./Fig_data/EMI.Rdata")
load("./Fig_data/Delta_HCHO.Rdata")
load("./Fig_data/Delta_NO2.Rdata")

#hh<-emi
emi$mean<-apply(emi[,10:13], 1, function(x) mean(x))
emi$delta<-emi$`2022-01-01`-emi$mean



delta<-merge(delta_HCHO, delta_NO2[, c("code", "NO2")], by="code")
delta<-merge(delta, dcast(emi, region+code~var, value.var = "delta"), by="code")
delta$NO2<-delta$NO2*6.02214*10^4
delta$HCHO<-delta$HCHO*6.02214*10^4
delta$Var1<-round_any(delta$lon, 0.1)
delta$Var2<-round_any(delta$lat, 0.1)


delta<-merge(delta, regime, by=c("Var1", "Var2"))
delta$regime<-factor(delta$regime, levels = c("TR", "VOC", "NOx"))


fig<-list()
class<-c("VOC", "TR", "NOx")

#regional mean of 
region_mean<-ddply(delta, c("regime", "region"), 
          summarise,
          mean=mean(EMI), 
          x=mean(NO2), 
          y=mean(HCHO))

for (i in 1:3){
  p<-ggplot()+
        geom_point(data=subset(delta, region!="OR" & regime==class[i]), aes(NO2, HCHO, fill=EMI), shape=21, colour="gray")+
        geom_point(data=subset(region_mean, region!="OR"  & regime==class[i]), aes(x, y, fill=mean), shape=22, size=3)+
        scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = 0)+
        geom_text(data=subset(region_mean, region!="OR" & regime==class[i]),aes(x=x, y=y, label=region), hjust=1.5, size=2)+
        geom_hline(yintercept = 0, linetype="dashed", size=0.5)+
        geom_vline(xintercept = 0, linetype="dashed", size=0.5)+
        scale_color_gradient2(low="blue", mid="white", high="red", midpoint = 0)+
        theme_bw()+
        theme(panel.background = element_blank(),
              legend.position = c(0, 1),
              legend.justification = c(0, 1),
              legend.box.spacing = unit(0, 'cm'),
              legend.background = element_blank(),
              legend.direction = "horizontal",
              legend.key.height = unit(0.2, "cm"),
              legend.key.width = unit(0.6, "cm"),
              axis.title = element_text(size=10),
              legend.text = element_text(size=7),
              legend.title = element_blank())+
        xlab(expression(Delta *" NO"[2]* " (mole cm"^-2*")"))+
        ylab(expression(Delta* " HCHO"* " (mole cm"^-2*")"))+
        guides(fill=guide_colorbar(title.position = "right",
                                   title.theme = element_text(vjust=0.5, size=8),
                                    title = expression(Delta*" O"[3]^EMI)))
    
  fig[[i]]<-p
}

plot_grid(fig.4a, fig.4b, fig.4c, 
          fig[[1]], fig[[2]], fig[[3]], ncol=3, 
          labels = letters[1:6], label_size = 11)
export::graph2pdf("./Figure/Fig.4.pdf", width=19/2.54, height=12/2.54)






