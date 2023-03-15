#Fig.S6
library(ggplot2)
library(reshape2)

setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
load("./Fig_data/EMI.Rdata")

fig.s6<-dcast(emi, code+region~var, value.var = "delta")
fig.s6$sum<-fig.s6$EMI+fig.s6$MET
fig.s6<-melt(fig.s6, measure.vars = 3:4, variable.name = "var", value.name = "con")

#ddply(fig.s6, c("var", "region"), summarise, mean=mean(con), sd=sd(con))

model_lable<-function(data){
  mod <- lm(con ~ sum+0, data=data)
  formula <- sprintf("italic(y) == %.2f * italic(x)",
                     round(coef(mod)[1], 2))
  r <-cor(data$con, data$sum)
  r <- sprintf("r == %.2f", r)
  data.frame(formula=formula, r=r, stringsAsFactors=FALSE)
}
labels<-ddply(subset(fig.s6, region!="OR"), c("var", "region"), model_lable)


#ddply(subset(fig.s6, region!="OR"), c("var", "region"), function(data){
#   broom::glance(cor.test(data$con, data$sum))
# })

ggplot(subset(fig.s6, region!="OR"), aes(sum, con, colour=var))+
  geom_point(shape=1)+
  scale_color_manual(values = c("red", "blue"))+
  stat_smooth(method="lm", formula=y~x+0, alpha=0.3, linetype="dashed", size=0.5, show.legend=F)+
  geom_abline(intercept=0, slope=0.5, show.legend = F, linetype="dashed")+
  geom_text(x=15, y=20, aes(label=formula), data=subset(labels, var=="EMI"), parse=TRUE, hjust=0, size=3, show.legend=F) +
  geom_text(x=30, y=20.3, aes(label=r), data=subset(labels, var=="EMI"), parse=TRUE, hjust=0, size=3, show.legend=F)+
  geom_text(x=15, y=-5, aes(label=formula, colour=var), data=subset(labels, var=="MET"), parse=TRUE, hjust=0, size=3, show.legend=F) +
  geom_text(x=30, y=-4.7, aes(label=r, colour=var), data=subset(labels, var=="MET"), parse=TRUE, hjust=0, size=3, show.legend=F)+
  #geom_text(x=40, y=27, aes(label=region), data=subset(labels, var=="MET"), parse=TRUE, hjust=1, size=3, colour="black", show.legend=F) +
  facet_wrap(region~., ncol=3)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    axis.title = element_text(size=10))+
  xlab(expression(Delta*" O"[3]*" ("*mu *g * " m"^-3*")"))+
  ylab(expression(Delta* " O"[3]^EMI * " or "*Delta* " O"[3]^MET*" ("*mu *g * " m"^-3*")"))

export::graph2pdf(file="./Figure/Fig.S6.pdf", width=19/2.54, height=12/2.54)

