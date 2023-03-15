#Figure 2
library(ggplot2)
load("./Fig_data/TS.Rdata")

a<-ggplot(subset(ts, region!="OR"), aes(year, mean_emi, colour=region))+geom_line()+
    geom_ribbon(aes(ymax=up_emi, ymin=low_emi, fill=region), alpha=0.5)+
    scale_colour_manual(values = c("#F8766D", "#C49A00", "#00B6EB", "#A58AFF", "#53B400", "#FB617D"))+
    scale_fill_manual(values = c("#F8766D", "#C49A00", "#00B6EB", "#A58AFF", "#53B400", "#FB617D"))+
    annotate("text", x=2014, y=84, label="BTH", size=2.5, hjust=0, colour="#F8766D")+
    annotate("text", x=2014, y=71, label="YRD", size=2.5, hjust=0, colour= "#FB617D")+
    annotate("text", x=2022, y=88, label="FWP", size=2.5, hjust=1, colour= "#C49A00")+
    annotate("text", x=2014, y=55, label="THB", size=2.5, hjust=0, colour= "#53B400")+
    annotate("text", x=2022, y=68, label="SCB", size=2.5, hjust=1, colour= "#A58AFF")+
    annotate("text", x=2022, y=56, label="YRD", size=2.5, hjust=1, colour= "#00B6EB")+
    theme_bw()+
    theme(axis.title =element_text(size=10),
          legend.position = "none")+
    xlab("Year")+
    ylab(expression(O[3]^EMI*" ("*mu*g* " m"^-3*")"))

b<-ggplot(subset(ts, region!="OR"), aes(year, mean_met, colour=region))+geom_line()+
    geom_ribbon(aes(ymax=up_met, ymin=low_met, fill=region), alpha=0.5)+
    scale_colour_manual(values = c("#F8766D", "#C49A00", "#00B6EB", "#A58AFF", "#53B400", "#FB617D"))+
    scale_fill_manual(values = c("#F8766D", "#C49A00", "#00B6EB", "#A58AFF", "#53B400", "#FB617D"))+
    theme_bw()+
    theme(axis.title =element_text(size=10),
          legend.position = "none")+
    xlab("Year")+
    ylab(expression(O[3]^MET*" ("*mu*g* " m"^-3*")"))

cowplot::plot_grid(a, b, ncol=1, align = "vh", labels = letters[1:2], label_size = 11)

export::graph2pdf(file="./Figure /Fig.2.pdf", width=9/2.54, height=12/2.54)


