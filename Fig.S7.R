#Fig.S7
library(ggplot2)
setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")

load("./Fig_data/S5P_NO2.Rdata")
load("./Fig_data/FNR.Rdata")
no2<-as.data.frame(S5P_NO2)


fig.s7<-merge(no2, fnr[, c("date", "code", "no2", "region")], by=c("code", "date"))
fig.s7$no2<-fig.s7$no2*6.02214*10^4


summary(lm(NO2~no2, data = fig.s7))
ggplot(fig.s7, aes(no2, NO2, colour=region))+
  geom_point(shape=1)+
  stat_smooth(method = "lm", aes(no2, NO2), inherit.aes = F, colour="black")+
  scale_color_manual(values = c("#F8766D", "#C49A00", "gray", "#00B6EB", "#A58AFF",  "#53B400", "#FB617D"))+
  annotate("text", x=75, y=150, label="y=1.09x+7.06, r=0.44***", size=3)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.box.spacing = unit(0, 'cm'),
        axis.title = element_text(size=10))+
  xlab(expression(X[NO2]*" (10"^15*" mole cm"^-2*")"))+
  ylab(expression(NO[2]*" ("*mu *"g m"^-3*")"))

export::graph2pdf(file="./Figure/Fig.S7.pdf", width=9/2.54, height=10/2.54)
