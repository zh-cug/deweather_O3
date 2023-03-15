#Fig.S5
library(ggplot2)
library(maptools)

setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
load("./Fig_data/MDA8.Rdata")

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



fig.s5<-melt(mda8[, c("lon", "lat", "mean", "2022")], measure.vars = 3:4, variable.name = "var", value.name = "conc")
levels(fig.s5$var)[levels(fig.s5$var)=="mean"]<-"2014-2021"


fig<-list()
for (i in 1:2){
  p<-ggplotGrob(
               ggplot()+
                  geom_polygon(data=china, aes(long, lat, group=group), fill="white")+
                  geom_path(data=china, aes(long, lat, group=group), size=0.1)+
                  geom_point(data=subset(fig.s5, var==levels(fig.s5$var)[i]), aes(lon, lat, fill=conc), 
                             shape=21, colour="gray", size=2, stroke=0.2)+
                  scale_fill_gradient(low="white", high="red", limits=c(45, 165))+
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
                  theme(legend.key.height = unit(1, 'cm'),
                        legend.key.width = unit(0.3, 'cm'),
                        legend.position = "right",
                        legend.box.spacing = unit(0, 'cm'),
                        panel.background = element_blank(),
                        axis.title = element_blank())+
                  guides(fill=guide_colorbar(title.position = "right",
                                             title.theme = element_text(angle = 90, size=9),
                                             title.hjust = 0.5,
                                             title = expression(" MDA8 O"[3]*" ("*mu *"g m"^-3*")")))
               )
  
  panel <- gtable::gtable_filter(p, "panel")
  
  fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                      heights = unit(rep(0.01, 100), "null"))%>%
               gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
               gtable_add_grob(SCS, t=83, l=2, b=98, r=12)
  
  p$grobs[[6]]<-fig_embed
  
  fig[[i]]<-p
}


plot_grid(fig[[1]], fig[[2]], labels = letters[1:2], label_size = 11, ncol=1)

export::graph2pdf(file="./Figure/Fig.S5.pdf", width=9/2.54, height=12/2.54)

