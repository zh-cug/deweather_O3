#Fig.S11
library(ggplot2)
library(package)
library(plyr)
library(gridExtra)

setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
load("./Fig_data/SHAP.Rdata")
load("./Fig_data/Delta_Met.Rdata")


fig.s11<-shap[, c("var", "code", "region", "delta")]
names(fig.s11)[c(1, 4)]<-c("met", "shap")
fig.s11<-merge(fig.s11, Delta_Met, by=c("code", "met"))
fig.s11$met<-toupper(fig.s11$met)


model_lable<-function(data){
  mod <- lm(shap ~ delta, data=data)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1], 2), round(coef(mod)[2], 2))
  r <-cor(data$shap, data$delta, use = "complete.obs")
  r <- sprintf("r == %.2f", r)
  data.frame(formula=formula, r=r, stringsAsFactors=FALSE)
}
labels<-ddply(fig.s11, "met", model_lable)
labels$text<-paste0(labels$formula, ", ", labels$r)


broom::glance(cor.test(fig.s11$shap, fig.s11$delta))

p.value<-ddply(fig.s11, "met", function(data) {
  broom::glance(cor.test(data$shap, data$delta))
})



fig.s11$met<-factor(fig.s11$met,
                   levels = c("T2M", "MSL", "U10", "V10", "RH", "TP", "BLH", "TCC", "SSR"))
labels$met<-factor(labels$met,
                   levels = c("T2M", "MSL", "U10", "V10", "RH", "TP", "BLH", "TCC", "SSR"))


fill_lab<-c(expression(Delta*" T2M ("^o*"C)"),
            expression(Delta* " MSL (Pa)"),
            expression(Delta* " U10 (m s"^-1*")"),
            expression(Delta* " V10 (m s"^-1*")"),
            expression(Delta* " RH (%)"),
            expression(Delta* " TP (mm)"),
            expression(Delta* " BLH (m)"),
            expression(Delta* " TCC (unitless)"),
            expression(Delta* " SSR (MJ m"^-2*")"))



color_lab<-c(expression(Delta* " SHAP"[T2M]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[MSL]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[U10]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[V10]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[RH]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[TP]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[BLH]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[TCC]* " ("*mu*g*" m"^-3*")"),
             expression(Delta* " SHAP"[SSR]* " ("*mu*g*" m"^-3*")"))

fig<-list()

col<-c("#F8766D", "#D39200", "#93AA00", "#00BA38", "#00C19F", "#00B9E3", "#619CFF", "#DB72FB", "#FF61C3")
for (i in 1:9){
  p<-ggplot(subset(fig.s11, met==levels(fig.s11$met)[i]), aes(delta, shap, colour=met),)+
    geom_point(shape=1, colour=col[i])+
    stat_smooth(method="lm", fill=col[i], colour=col[i])+
    geom_text(x=0, y=min(subset(fig.s11, met==levels(fig.s11$met)[i])[, "shap"])+1, 
              aes(label=formula), data=subset(labels, met==levels(labels$met)[i]), 
              parse=TRUE, hjust=0, size=3, show.legend=F, color=col[i]) +
    geom_text(x=0, y=min(subset(fig.s11, met==levels(fig.s11$met)[i])[, "shap"]), 
              aes(label=r), data=subset(labels, met==levels(labels$met)[i]), 
              parse=TRUE, hjust=0, size=3, show.legend=F, color=col[i]) +
    facet_wrap(met~., ncol=1)+
    theme_bw()+
    theme(legend.position = "none",
          axis.title = element_text(size=10))+
    ylab(color_lab[i])+
    xlab(fill_lab[i])
  fig[[i]]<-p
}

gridExtra::grid.arrange(fig[[1]], fig[[2]], fig[[3]],
                        fig[[4]], fig[[5]], fig[[6]],
                        fig[[7]], fig[[8]], fig[[9]], ncol=3)

export::graph2pdf(file="./Figure/Fig.S11.pdf", width=19/2.54, height=15/2.54)


