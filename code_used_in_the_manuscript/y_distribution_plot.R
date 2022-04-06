# load data
setwd("Z:/Projects/Kinetics based SAR/Data/Calculation/R regression/20210907_new des")

data_dist <- read.csv('data_dist.csv')
data_dist['ID'] <- abs(data_dist['ID']-1)
  
library(ggplot2)
library(patchwork)
library(cowplot)

p1 <- ggplot(data_dist, aes(x=factor(data_dist[,1]), y=lnKD))+
  geom_boxplot() +
  geom_jitter(aes(color = factor(data_dist[,1])),size = 2,alpha =.6, show.legend = TRUE)+
  #geom_point(aes(y=lnKD, color = factor(data_dist[,1])),size = 2,alpha =.6, show.legend = TRUE)+
  labs(x = expression(bold("ln"~bolditalic(K)[bold(D)]*"")), y = "", color = "") +
  scale_color_manual(labels = c("Training set", "Test set"), values = c("dodgerblue", "red2")) +
  scale_y_continuous(breaks=seq(-20, -5,5), limits=c(-20, -5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.y = element_text(size = 10), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 15))

p2 <- ggplot(data_dist, aes(x=factor(data_dist[,1]), y=lnkon))+
  geom_boxplot() +
  geom_jitter(aes(color = factor(data_dist[,1])),size = 2,alpha =.6, show.legend = TRUE)+
  #geom_point(aes(y=lnkon, color = factor(data_dist[,1])),size = 2,alpha =.6, show.legend = TRUE)+
  labs(x = expression(bold("ln"~bolditalic(k)[bold(on)]*"")), y = "", color = "") +
  scale_color_manual(labels = c("Training set", "Test set"), values = c("dodgerblue", "red2")) +
  scale_y_continuous(breaks=seq(0, 15,5), limits=c(0, 15))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.y = element_text(size = 10), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 15,face = 'bold'))

p3 <- ggplot(data_dist, aes(x=factor(data_dist[,1]), y=lnkoff))+
  geom_boxplot() +
  geom_jitter(aes(color = factor(data_dist[,1])),size = 2,alpha =.6, show.legend = TRUE)+
  #geom_point(aes(y=lnkoff, color = factor(data_dist[,1])),size = 2,alpha =.6, show.legend = TRUE)+
  labs(x = expression(bold("ln"~bolditalic(k)[bold(off)]*"")), y = "", color = "") +
  scale_color_manual(labels = c("Training set", "Test set"), values = c("dodgerblue", "red2")) +
  scale_y_continuous(breaks=seq(-5, 2.5,2.5), limits=c(-5, 2.5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.y = element_text(size = 10), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 15,face = 'bold'))

prow <- plot_grid( p1 + theme(legend.position="none"),
                   p2 + theme(legend.position="none"),
                   p3 + theme(legend.position="none"),
                   align = 'vh',
                   #labels = c("A", "B", "C"),
                   #hjust = -1,
                   nrow = 1
)
legend_b <- get_legend(p1 + theme(legend.position="bottom", 
                                  legend.text = element_text(color = "black",size = 15,face = "bold")))
#title <- ggdraw() + draw_label("Data Distribution", fontface='bold')
p <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, 0.1))
p

ggsave("distribution_plot_border.tiff", p, width=4,height=4, unit="in", dpi=300)
#ggarrange(plot1, plot2, plot3, ncol=2, common.legend = TRUE, legend="bottom")
#grid.arrange(plot1,plot2,plot3, ncol=3)
#combined <- plot1 + plot2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# ======================================OLD CODE========================================================
"
# plot of lnkon data 
p2 <- ggplot(data_dist, aes(x=1,y=lnkon)) +labs(x = expression("lnk"[on]*""),y='')+
# THE DATA POINT
  geom_point(aes(color = factor(data_dist[,1])),size = 3,alpha =.8,show.legend = FALSE) +
# title
  ggtitle('')+
    theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(axis.title.x = element_text(size = 30,face = 'bold'))+
  theme(legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 20,face = "bold"))
p2
# scale_color_manual(values = c("Trainingset" = "blue4", "Testset" = "red"))


# plot of lnkon data 
p3 <- ggplot(data_dist, aes(x=1,y=lnkoff)) +labs(x = expression("lnk"[off]*""),y='')+
  
  
  # legends
  scale_color_discrete(labels=c("Testset", "Trainingset"))+
  # THE DATA POINT
  geom_point(aes(color = factor(data_dist[,1])),size = 3,alpha =.8) +
  # title
  ggtitle('')+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(axis.title.x = element_text(size = 30,face = 'bold'))+
  theme(legend.title = element_blank(),
        legend.text = element_text(color = "black",size = 20,face = "bold"))
p3

# plot of lnkon data 
p1 <- ggplot(data_dist, aes(x=1,y=lnKD)) +labs(x = expression("lnK"[D]*""),y='')+
  # THE DATA POINT
  geom_point(aes(color = factor(data_dist[,1])),size = 3,alpha =.8,show.legend = FALSE) +
  # title
  ggtitle('')+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(axis.title.x = element_text(size = 30,face = 'bold'))
p1

library(gridExtra)
grid.arrange(p1, p2,p3, ncol=3)"
