#Fig.S4
library(ggplot2)

setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
load("./Fig_data/MDA8.Rdata")
load("./Fig_data/delta_t2m.Rdata")


fig.s4<-mda8[, c("code", "region", "delta")]
fig.s4<-merge(fig.s4, delta_t2m, by="code")

#summary(lm(delta~T2M, data = fig.s4))
ggplot(fig.s4, aes(T2M, delta, colour=region))+
  geom_point(shape=1)+
  stat_smooth(method = "lm", aes(T2M, delta), inherit.aes = F, colour="black")+
  scale_color_manual(values = c("#F8766D", "#C49A00", "gray", "#00B6EB", "#A58AFF",  "#53B400", "#FB617D"))+
  annotate("text", x=0, y=40, label="y=7.61x+0.92, r=0.72***", size=3)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.box.spacing = unit(0, 'cm'),
        axis.title = element_text(size=10))+
  xlab(expression(Delta*" MDT2M ("^o*"C)"))+
  ylab(expression(Delta*" MDA8 O"[3]*" ("*mu *"g m"^-3*")"))

export::graph2pdf(file="./Figure/Fig.S4.pdf", width=9/2.54, height=10/2.54)

