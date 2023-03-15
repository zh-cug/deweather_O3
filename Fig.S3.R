#Fig.S3
library(lubridate)
library(ggplot2)
library(boot)

setwd("/Users/zhenghuang/Desktop/Submitted/O3/scripts/")
load("./Fig_data/FNR.Rdata")
load("./Fig_data/S5P_O3.Rdata")

o3<-as.data.frame(S5P_O3)
fnr<-merge(na.omit(fnr), na.omit(o3), by=c("date", "code"))
fnr$year<-year(fnr$date)

fnr$bin<-cut(fnr$fnr, seq(0, 8, 0.1))
get_OEP<-function(data){
  data<-na.omit(data[, c("fnr", "O3")])
  ratio<-length(which(data$O3>=160))/nrow(data)
  out<-data.frame(x=mean(data$fnr, na.rm = T),
                  y=ratio)
  out
}

fitting<-ddply(subset(fnr, year==2022), "bin", get_OEP)

lmp3<-lm(y~x+I(x^2)+I(x^3), data=na.omit(fitting))
summary(lmp3)

# num<-as.numeric(lmp3$coefficients)
# fw <- function (x){
#   num[1]+num[2]*x+num[3]*x^2+num[4]*x^3
# }
# dd<-data.frame(x=seq(0, 8, 0.01))
# dd$y<-apply(dd, 2, fw)
# 
# View(dd[which(dd$y>as.numeric(quantile(as.numeric(lmp3$fitted.values), c(0.9, 1))[1])),])


# peak_un<-function(formula, data, indices){
#   d<-data[indices,]
#   fit<-lm(formula, data=d)
#   num<-as.numeric(fit$coefficients)
#   fw <- function (x){
#     num[1]+num[2]*x+num[3]*x^2+num[4]*x^3
#   }
#   dd<-data.frame(x=seq(0, 8, 0.01))
#   dd$y<-apply(dd, 2, fw)
#   return(as.numeric(dd[which.max(dd$y),][1]))
# }
# 
# results<-boot(data=fitting, statistic = peak_un, R=1000, formula=y~x+I(x^2)+I(x^3))
# print(results)


# stats<-function(data){
#   lmp3<-lm(y~x+I(x^2)+I(x^3), data=na.omit(data))
#   out<-broom::glance(lmp3)
#   
#   num<-as.numeric(lmp3$coefficients)
#   fw <- function (x){
#     num[1]+num[2]*x+num[3]*x^2+num[4]*x^3
#   }
#   dd<-data.frame(x=seq(0, 8, 0.01))
#   dd$y<-apply(dd, 2, fw)
#   
#   
#   tr<-dd[which(dd$y>as.numeric(quantile(as.numeric(lmp3$fitted.values), c(0.9, 1))[1])),]
#   
#   
#   return(data.frame(
#     low=tr$x[1],
#     up=tr$x[nrow(tr)],
#     peak= dd[which.max(dd$y), "x"],
#     r=sqrt(out$r.squared),
#     p=out$p.value))
# }
# 
# ddply(fitting, "region", stats)

#--------------------Fig.S3--------------------
ggplot(na.omit(fitting), aes(x, y))+
  geom_point(shape=1)+
  geom_smooth(method = "lm", formula = y~x+I(x^2)+I(x^3), level=0.95, colour="black")+
  geom_vline(xintercept = 3.06, colour="red")+
  annotate("rect", xmin=2.67, xmax=3.47, ymin=-0.12, ymax=0.5, fill="red", alpha=0.3, size=0.5)+
  annotate("text", x=4, y=0.45, label="r = 0.91***", size=3, hjust=0)+
  annotate("text", x=4, y=0.4, label="TR: [2.67, 3.47]", size=3, hjust=0)+
  annotate("text", x=4, y=0.35, label=expression("Peak = 3.06 (2"*delta*" = 0.18)"), size=3, hjust=0)+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    axis.title = element_text(size=10))+
  xlab(expression("HCHO/NO"[2]))+
  ylab("OEP")

export::graph2pdf(file="./Figure/Fig.S3.pdf", width=9/2.54, height=8/2.54)
