#Fig.6
library(ncdf4)
library(ggplot2)
library(maptools)

#load map
world<-readShapePoly("./Metadata/world.shp")
world<-fortify(world)
china<-readShapePoly("./Metadata/china.shp")
china<-fortify(china)

nc<-nc_open("./Fig_data/mon_mean_ghp")
lon<-as.numeric(ncvar_get(nc, varid = "longitude"))
lat<-as.numeric(ncvar_get(nc, varid = "latitude"))
grid<-data.frame(expand.grid(lon, lat))
grid$gph_clm_6<-as.numeric(ncvar_get(nc, varid = "gph_clm")[,,1])
grid$gph_clm_7<-as.numeric(ncvar_get(nc, varid = "gph_clm")[,,2])
grid$gph_clm_8<-as.numeric(ncvar_get(nc, varid = "gph_clm")[,,3])
grid$gph_2022_6<-as.numeric(ncvar_get(nc, varid = "gph_2022")[,,1])
grid$gph_2022_7<-as.numeric(ncvar_get(nc, varid = "gph_2022")[,,2])
grid$gph_2022_8<-as.numeric(ncvar_get(nc, varid = "gph_2022")[,,3])
nc_close(nc)

grid$dif_6<-grid$gph_2022_6-grid$gph_clm_6
grid$dif_7<-grid$gph_2022_7-grid$gph_clm_7
grid$dif_8<-grid$gph_2022_8-grid$gph_clm_8


fig<-list()
for (i in seq(6, 8, 1)){
  data<-grid[,c("Var1", "Var2", paste0("gph_clm_", i), paste0("gph_2022_", i), paste0("dif_", i))]
  names(data)[3:5]<-c("gph_clm", "gph_2022", "dif")
  data<-na.omit(subset(data, Var1>89 & Var1<151 &Var2>9 & Var2<46))
  
  p<-ggplot()+
        geom_tile(data = data, aes(Var1, Var2, fill=dif/9.8))+
        geom_path(data=world, aes(long, lat, group=group), colour="gray", size=0.2)+
        geom_path(data=china, aes(long, lat, group=group), colour="black", size=0.2)+
        geom_contour(data = data, aes(Var1, Var2, z=gph_clm/9.8), breaks = 5880, colour="black")+
        geom_contour(data = data, aes(Var1, Var2, z=gph_2022/9.8), breaks = 5880, colour="red")+
        scale_fill_gradient2(low="#4D9221", mid = "white", high="#C51B7D", midpoint = 0,
                             breaks=seq(-50, 50, 20), limits=c(-60, 60))+
        scale_x_continuous(limits = c(90, 150), expand=c(0, 0), breaks = c(100, 120, 140),
                           labels = c(expression(80^o* E), expression(100^o* E), expression(120^o* E)))+
        scale_y_continuous(limits = c(10, 45), expand=c(0, 0), breaks = c(15, 25, 35),
                           labels = c(expression(25^o* N), expression(35^o* N), expression(45^o* N)))+
        annotate("rect", xmin=114, xmax=118, ymin=37, ymax=41, colour="#F8766D", fill=NA, size=0.5)+
        annotate("rect", xmin=118, xmax=122, ymin=30, ymax=33, colour="#FB617D", fill=NA)+
        annotate("rect", xmin=112, xmax=115.5, ymin=21.5, ymax=24, colour="#00B6EB", fill=NA)+
        annotate("rect", xmin=103.5, xmax=107, ymin=28.5, ymax=31.5, colour="#A58AFF", fill=NA)+
        annotate("rect", xmin=110.75, xmax=114.75, ymin=28.5, ymax=31.5, colour="#53B400", fill=NA)+
        geom_polygon(data=FWP, aes(x, y), colour="#C49A00", fill=NA)+
        theme_bw()+
        theme(legend.key.height = unit(0.3, 'cm'),
              legend.key.width = unit(1, 'cm'),
              legend.position = "bottom",
              legend.box.spacing = unit(0, 'cm'),
              legend.spacing.x = unit(0.1, 'cm'),#space between different legend
              panel.background = element_blank(),
              axis.title = element_blank()) +
        guides(fill=guide_colorbar(title.position = "bottom", 
                                   title.theme = element_text(angle = 0, size=9),
                                   title.hjust = 0.5, 
                                   title = expression(Delta*" gpm")))
  fig[[i]]<-p
}


cowplot::plot_grid(fig[[6]], fig[[7]], fig[[8]], ncol=3, labels = letters[1:3], label_size = 11)

export::graph2pdf(file="./Figure/Fig.6.pdf", width=19/2.54, height=7/2.54)  









