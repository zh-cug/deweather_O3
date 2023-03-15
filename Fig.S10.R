#Figure S10
library(ggplot2)

setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
load("./Fig_data/delta_HCHO.Rdata")
load("./Fig_data/delta_t2m.Rdata")
site<-read.csv("./Fig_data/site_a.csv")

fig.s10<-merge(delta_HCHO[, c("code", "HCHO")], delta_t2m[, c("code", "T2M")], by="code")
fig.s10<-merge(fig.s10, site, by="code")


fig.s10$HCHO=fig.s10$HCHO*6.022*10^4#convert from mol/m2 to mole/cm2


#summary(lm(HCHO~T2M, fig.s10))
ggplot(fig.s10, aes(T2M, HCHO, colour=region))+
  geom_point(shape=1)+
  stat_smooth(method = "lm", colour="black")+
  scale_color_manual(values = c("#F8766D", "#C49A00", "gray", "#00B6EB", "#A58AFF",  "#53B400", "#FB617D"))+
  annotate("text", x=0, y=4, label="y=1.02x-0.63, r=0.79***", size=3)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.box.spacing = unit(0, 'cm'),
        axis.title = element_text(size=10))+
  ylab(expression(Delta* HCHO*" (mole cm"^-2*")"))+
  xlab(expression(Delta*" T2M ("^o*"C)"))

export::graph2pdf(file="./Figure/Fig.S10.pdf", width=9/2.54, height=10/2.54)

