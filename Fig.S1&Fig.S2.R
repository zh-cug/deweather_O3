#Fig.S1 & Fig.S2
setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts")
load("./Fig_data/STAT.Rdata")

stat$vars<-factor(stat$vars)
var<-c("COE", "FAC2", "MGE", "MB", "r", "RMSE")
fig.s1<-list()
for (i in 1:6){
  p<- ggplotGrob(
                ggplot()+
                geom_polygon(data=china, aes(long, lat, group=group), fill="white")+
                geom_path(data=china, aes(long, lat, group=group), size=0.1)+
                geom_point(data=subset(stat, vars==var[i]), 
                           aes(lon, lat, fill=dw), shape=21, colour="gray", stroke=0.2, size=2)+
                scale_fill_continuous(low="white", high="red")+
                scale_x_continuous(limits = c(72, 136), expand=c(0, 0), breaks = c(80, 100, 120),
                                   labels = c(expression(80^o* E), expression(100^o* E), expression(120^o* E)))+
                scale_y_continuous(limits = c(17, 55), expand=c(0, 0), breaks = c(25, 35, 45),
                                   labels = c(expression(25^o* N), expression(35^o* N), expression(45^o* N)))+
                theme_bw()+
                theme(axis.title = element_blank(),
                      panel.background = element_blank(),
                      legend.position = c(0, 1),
                      legend.justification = c(0, 1),
                      legend.box.spacing = unit(0, 'cm'),
                      legend.background = element_blank(),
                      legend.direction = "horizontal",
                      legend.key.height = unit(0.2, "cm"),
                      legend.key.width = unit(0.5, "cm"),
                      legend.text = element_text(size=7),
                      legend.title = element_text(size=8))+
                guides(fill=guide_colorbar(title.position = "right", 
                                           title.theme = element_text(angle = 0, size=9),
                                           title.vjust = 1,
                                           title=var[i]))
                )
  
  
  panel <- gtable::gtable_filter(p, "panel")
  fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                      heights = unit(rep(0.01, 100), "null"))%>%
               gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
               gtable_add_grob(SCS, t=83, l=2, b=98, r=12)
  
  p$grobs[[6]]<-fig_embed
  
  fig.s1[[i]] <- as.ggplot(p)
}
plot_grid(fig.s1[[1]], fig.s1[[2]], fig.s1[[3]],
          fig.s1[[4]], fig.s1[[5]], fig.s1[[6]], 
          ncol=3, labels = letters[1:6], label_size = 11)

export::graph2pdf(file="./Figure/Fig.S1", width=19/2.54, height=12/2.54)


fig.s2<-list()
for (i in 1:6){
  p<- ggplotGrob(
                ggplot()+
                geom_polygon(data=china, aes(long, lat, group=group), fill="white")+
                geom_path(data=china, aes(long, lat, group=group), size=0.1)+
                geom_point(data=subset(stat, vars==var[i]), 
                           aes(lon, lat, fill=shap), shape=21, colour="gray", stroke=0.2, size=2)+
                scale_fill_continuous(low="white", high="blue")+
                scale_x_continuous(limits = c(72, 136), expand=c(0, 0), breaks = c(80, 100, 120),
                                   labels = c(expression(80^o* E), expression(100^o* E), expression(120^o* E)))+
                scale_y_continuous(limits = c(17, 55), expand=c(0, 0), breaks = c(25, 35, 45),
                                   labels = c(expression(25^o* N), expression(35^o* N), expression(45^o* N)))+
                theme_bw()+
                theme(axis.title = element_blank(),
                      panel.background = element_blank(),
                      legend.position = c(0, 1),
                      legend.justification = c(0, 1),
                      legend.box.spacing = unit(0, 'cm'),
                      legend.background = element_blank(),
                      legend.direction = "horizontal",
                      legend.key.height = unit(0.2, "cm"),
                      legend.key.width = unit(0.5, "cm"),
                      legend.text = element_text(size=7),
                      legend.title = element_text(size=8))+
                guides(fill=guide_colorbar(title.position = "right", 
                                           title.theme = element_text(angle = 0, size=9),
                                           title.vjust = 1,
                                           title=var[i])))
  
  
  panel <- gtable::gtable_filter(p, "panel")
  
  fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                      heights = unit(rep(0.01, 100), "null"))%>%
               gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
               gtable_add_grob(SCS, t=83, l=2, b=98, r=12)
  
  p$grobs[[6]]<-fig_embed
  
  fig.s2[[i]] <- as.ggplot(p)
}
plot_grid(fig.s2[[1]], fig.s2[[2]], fig.s2[[3]],
          fig.s2[[4]], fig.s2[[5]], fig.s2[[6]], ncol=3,
          align = "vh", labels = letters[1:6], label_size = 11)
export::graph2pdf(file="./Figure/Fig.S2", width=19/2.54, height=12/2.54)


#----------------------------Fig.S1 & Fig.S2---------------------------