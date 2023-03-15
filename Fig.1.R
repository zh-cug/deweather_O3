##Fig.1
library(maptools)
library(maptools)
library(ggnewscale)
library(cowplot)
library(gtable)
library(ggplotify)
library(magrittr)
library(ncdf4)

setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
load("./Fig_data/MDA8.Rdata")

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
                ggplot()+
                  geom_line(data=scs,aes(x=long,y=lat,group=group),color="black",size=0.2)+
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


#read data to generage plot
nc<-nc_open("./Fig_data/Delta_MDT2M.nc")
lon<-ncvar_get(nc, varid = "longitude")
lat<-ncvar_get(nc, varid = "latitude")
mdt2m<-data.frame(expand.grid(lon, lat))
names(mdt2m)[1:2]<-c("lon", "lat")
mdt2m$t2m<-as.numeric(ncvar_get(nc, varid = "t2m"))


p1<-ggplotGrob(
  ggplot()+
    geom_tile(data=na.omit(mdt2m), aes(lon, lat, fill=t2m))+
    scale_fill_gradient2(low="#4D9221", mid = "white", high="#C51B7D", midpoint = 0,
                         limits=c(-4.5,4.5), breaks=seq(-4, 4, 2))+
    new_scale_fill()+
    geom_point(data=na.omit(mda8), aes(lon, lat, fill=delta/10), shape=21, colour="gray", stroke=0.3)+
    scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 0,
                         limits=c(-4.5,4.5), breaks=seq(-4, 4, 2), labels=seq(-40, 40, 20))+
    geom_path(data=china, aes(long, lat, group=group), size=0.1)+
    scale_x_continuous(limits = c(72, 136), expand=c(0, 0), breaks = c(80, 100, 120),
                       labels = c(expression(80^o* E), expression(100^o* E), expression(120^o* E)))+
    scale_y_continuous(limits = c(17, 55), expand=c(0, 0), breaks = c(25, 35, 45),
                       labels = c(expression(25^o* N), expression(35^o* N), expression(45^o* N)))+
    annotate("rect", xmin=114, xmax=118, ymin=37, ymax=41, colour="#F8766D", fill=NA, size=0.5)+#BTH
    annotate("text", x=125, y=37, label="BTH", size=2.5, hjust=0)+
    annotate("segment", x=118, xend=125, y=37, yend=37, colour="black", size=0.25, arrow=arrow(length = unit(0.05, "inches")))+
    annotate("rect", xmin=118, xmax=122, ymin=30, ymax=33, colour="#FB617D", fill=NA)+#YRD
    annotate("text", x=125, y=30, label="YRD", size=2.5, hjust=0)+
    annotate("segment", x=122, xend=125, y=30, yend=30, colour="black", size=0.25, arrow=arrow(length = unit(0.05, "inches")))+
    annotate("rect", xmin=112, xmax=115.5, ymin=21.5, ymax=24, colour="#00B6EB", fill=NA)+#PRD+
    annotate("text", x=120, y=20, label="PRD", size=2.5, hjust=0)+
    annotate("segment", x=115.5, xend=120, y=21.5, yend=20, colour="black", size=0.25, arrow=arrow(length = unit(0.05, "inches")))+
    annotate("rect", xmin=103.5, xmax=107, ymin=28.5, ymax=31.5, colour="#A58AFF", fill=NA)+#SCB
    annotate("text", x=95, y=25, label="SCB", size=2.5, hjust=1)+
    annotate("segment", x=103.5, xend=95, y=28.5, yend=25, colour="black", size=0.25, arrow=arrow(length = unit(0.05, "inches")))+
    annotate("rect", xmin=110.75, xmax=114.75, ymin=28.5, ymax=31.5, colour="#53B400", fill=NA)+#THB
    annotate("text", x=125, y=26.5, label="THB", size=2.5, hjust=0)+
    annotate("segment", x=114.75, xend=125, y=28.5, yend=26.5, colour="black", size=0.25, arrow=arrow(length = unit(0.05, "inches")))+
    geom_polygon(data=FWP, aes(x, y), colour="#C49A00", fill=NA)+#FWP
    annotate("text", x=105, y=45, label="FWP", size=2.5, hjust=0.5, vjust=0)+
    annotate("segment", x=108.75, xend=105, y=37, yend=45, colour="black", size=0.25, arrow=arrow(length = unit(0.05, "inches")))+
    theme_bw()+
    theme(legend.key.height = unit(0.3, 'cm'),
          legend.key.width = unit(0.6, 'cm'),
          legend.position = "bottom",
          legend.box.spacing = unit(0.4, 'cm'),
          legend.spacing.x = unit(0.1, 'cm'),#space between different legend
          panel.background = element_blank(),
          axis.title = element_blank()) +
    guides(colour=guide_colorbar(title.position = "bottom", 
                                 title.theme = element_text(angle = 0, size=9),
                                 title.hjust = 0.5, 
                                 order=0,
                                 title = expression(Delta*" MDA8 O"[3]*" ("*mu *"g m"^-3*")")),
           colour=guide_colorbar(title.position = "bottom", 
                                 title.theme = element_text(angle = 0, size=9),
                                 title.hjust = 0.5, 
                                 order=1,
                                 label=T,
                                 title = expression(Delta*" MDT2M ("^o*"C)")))
  
  
              )

p2<-ggplotGrob(
  ggplot()+
    geom_tile(data=na.omit(mdt2m), aes(lon, lat, fill=t2m))+
    scale_fill_gradient2(low="#4D9221", mid = "white", high="#C51B7D", midpoint = 0,
                         limits=c(-4.5,4.5), breaks=seq(-4, 4, 2))+
    geom_point(data=na.omit(mda8), aes(lon, lat, colour=delta/10), shape=21)+
    scale_colour_gradient2(low="blue", mid = "white", high="red", midpoint = 0,
                           limits=c(-4.5,4.5), breaks=seq(-4, 4, 2), labels=seq(-40, 40, 20))+
    theme(legend.key.height = unit(0.3, 'cm'),
          legend.key.width = unit(0.6, 'cm'),
          legend.position = "bottom",
          legend.box.spacing = unit(0.4, 'cm'),
          legend.spacing.x = unit(0.1, 'cm'),#space between different legend
          panel.background = element_blank(),
          axis.title = element_blank()) +
    guides(colour=guide_colorbar(title.position = "bottom", 
                               title.theme = element_text(angle = 0, size=9),
                               title.hjust = 0.5, 
                               order=0,
                               title = expression(Delta*" MDA8 O"[3]*" ("*mu *"g m"^-3*")")),
           fill=guide_colorbar(title.position = "bottom", 
                               title.theme = element_text(angle = 0, size=9),
                               title.hjust = 0.5, 
                               order=1,
                               label=T,
                               title = expression(Delta*" MDT2M ("^o*"C)"))))


panel <- gtable::gtable_filter(p1, "panel")
panel_layout<-c(subset(p1$layout, name=="panel"))

fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                    heights = unit(rep(0.01, 100), "null"))%>%
             gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
             gtable_add_grob(SCS, t=83, l=2, b=98, r=12)

p1$grobs[[6]]<-fig_embed
p1$grobs[[15]]<-p2$grobs[[15]]

cowplot::plot_grid(p1)

export::graph2pdf(file="./Figure/Fig.1.pdf", width=9/2.54, height=9/2.54)


