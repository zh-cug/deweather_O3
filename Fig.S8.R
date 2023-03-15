#Fig.S8
#This figure is too large to produce and we omitted to draw South China sea in this script but it was inserted by Adobe illustrator.
library(ggplot2)
library(maptools)
setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
load("./Fig_data/SHAP_coef.Rdata")

china<-readShapePoly("./Metadata/china.shp")
china<-fortify(china)



ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white")+
  geom_path(data=china, aes(long, lat, group=group), size=0.1)+
  geom_point(data=na.omit(coef), aes(lon, lat, colour=coef), size=0.5)+
  scale_colour_gradient2(low="blue", mid = "white", high = "red", midpoint = 0, limits=c(-1, 1))+
  scale_x_continuous(limits = c(72, 136))+
  scale_y_continuous(limits = c(17, 55))+
  facet_grid(var1~var2)+
  theme_bw()+
  theme(axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(3, 'cm'),
        legend.box.spacing = unit(0, 'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  guides(colour=guide_colorbar(title.position = "bottom", 
                               title.theme = element_text(angle = 0, size=9),
                               title.hjust = 0.5, 
                               title = "r"))

export::graph2jpg(file="./Figure/Fig.S8.jpg", width=19/2.54, height=19/2.54)
