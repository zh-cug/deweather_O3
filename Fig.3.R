#Figure 3
library(ggplotify)
load("./Fig_data/EMI.Rdata")

vars<-c("EMI", "MET")
legend.title<-c(expression(Delta* "O"[3]^EMI*" ("*mu*g*" m"^-3*")"),
                expression(Delta* "O"[3]^MET*" ("*mu*g*" m"^-3*")"))
fig.3<-list()

for (i in 1:2){
  p<-ggplotGrob(
    ggplot()+
      geom_polygon(data=china, aes(long, lat, group=group), fill="white")+
      geom_point(data=subset(emi, var==vars[i]), aes(lon, lat, fill=delta), shape=21, colour="gray", stroke=0.3)+
      geom_path(data=china, aes(long, lat, group=group), size=0.1)+
      scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 0, limits=c(-28, 28))+
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
      #annotate("text", label="b", x=75, y=53)+
      theme_bw()+
      theme(legend.key.height = unit(1, 'cm'),
            legend.key.width = unit(0.3, 'cm'),
            legend.position = "right",
            legend.box.spacing = unit(0, 'cm'),
            legend.spacing.x = unit(0.1, 'cm'),#space between different legend
            panel.background = element_blank(),
            axis.title = element_blank()) +
      guides(fill=guide_colorbar(title.position = "right", 
                                   title.theme = element_text(angle = 90, size=9),
                                   title.hjust = 0.5, 
                                   title = legend.title[i])))
  
  panel <- gtable::gtable_filter(p, "panel")
  
  fig_embed <- gtable(widths = unit(rep(0.01, 100), "null"), 
                      heights = unit(rep(0.01, 100), "null"))%>%
    gtable_add_grob(panel, t=1, l=1, b=100, r=100)%>%
    gtable_add_grob(SCS, t=83, l=2, b=98, r=12)
  
  p$grobs[[6]]<-fig_embed  
  
  
  
  fig.3[[i]] <- as.ggplot(p)
}

plot_grid(fig.3[[1]], fig.3[[2]], ncol=1, labels = letters[1:2], label_size = 11)
export::graph2pdf(file="./Figure/Fig.3.pdf", width=9/2.54, height=12/2.54)





