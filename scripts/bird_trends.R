# bird trends

# libraries
library(ggplot2)
library(patchwork)
library(mgcv)
library(data.table)

setwd("C:/R_projects/Trend_compare")
trends_temp <- read.csv("time_indices_birds_sj_proper.csv")
trends_temp$smooth <- NA
trends_temp_l <- trends_temp

num_species <- length(unique(trends_temp$species))

trends_temp$smooth[1:25]<-gam(trends_temp$imputed[1:25]~s(trends_temp$time[1:25]),method="REML")$fitted.values
trends_temp$smooth[26:50]<-gam(trends_temp$imputed[26:50]~s(trends_temp$time[26:50]),method="REML")$fitted.values
trends_temp$smooth[51:75]<-gam(trends_temp$imputed[51:75]~s(trends_temp$time[51:75]),method="REML")$fitted.values
trends_temp$smooth[76:100]<-gam(trends_temp$imputed[76:100]~s(trends_temp$time[76:100]),method="REML")$fitted.values
trends_temp$smooth[101:125]<-gam(trends_temp$imputed[101:125]~s(trends_temp$time[101:125]),method="REML")$fitted.values
trends_temp$smooth[126:150]<-gam(trends_temp$imputed[126:150]~s(trends_temp$time[126:150]),method="REML")$fitted.values
trends_temp$smooth[151:175]<-gam(trends_temp$imputed[151:175]~s(trends_temp$time[151:175]),method="REML")$fitted.values
trends_temp$smooth[176:200]<-gam(trends_temp$imputed[176:200]~s(trends_temp$time[176:200]),method="REML")$fitted.values
trends_temp$smooth[201:225]<-gam(trends_temp$imputed[201:225]~s(trends_temp$time[201:225]),method="REML")$fitted.values
trends_temp$smooth[226:250]<-gam(trends_temp$imputed[226:250]~s(trends_temp$time[226:250]),method="REML")$fitted.values

trends_temp_l$smooth[1:25]<-loess(trends_temp_l$imputed[1:25]~(trends_temp_l$time[1:25]),span=0.75)$fitted
trends_temp_l$smooth[26:50]<-loess(trends_temp_l$imputed[26:50]~(trends_temp_l$time[26:50]),span=0.75)$fitted
trends_temp_l$smooth[51:75]<-loess(trends_temp_l$imputed[51:75]~(trends_temp_l$time[51:75]),span=0.75)$fitted
trends_temp_l$smooth[76:100]<-loess(trends_temp_l$imputed[76:100]~(trends_temp_l$time[76:100]),span=0.75)$fitted
trends_temp_l$smooth[101:125]<-loess(trends_temp_l$imputed[101:125]~(trends_temp_l$time[101:125]),span=0.75)$fitted
trends_temp_l$smooth[126:150]<-loess(trends_temp_l$imputed[126:150]~(trends_temp_l$time[126:150]),span=0.75)$fitted
trends_temp_l$smooth[151:175]<-loess(trends_temp_l$imputed[151:175]~(trends_temp_l$time[151:175]),span=0.75)$fitted
trends_temp_l$smooth[176:200]<-loess(trends_temp_l$imputed[176:200]~(trends_temp_l$time[176:200]),span=0.75)$fitted
trends_temp_l$smooth[201:225]<-loess(trends_temp_l$imputed[201:225]~(trends_temp_l$time[201:225]),span=0.75)$fitted
trends_temp_l$smooth[226:250]<-loess(trends_temp_l$imputed[226:250]~(trends_temp_l$time[226:250]),span=0.75)$fitted

trends_temp_l$sm_fixed <- NA
row_counter <- 1
for (i in 1:(2*num_species)) {
  
  trends_temp_l$sm_fixed[row_counter] <- trends_temp_l$smooth[row_counter] / trends_temp_l$smooth[row_counter]

  row_counter_2 <- row_counter + 1
  
  for (j in 1:24) {
    
    trends_temp_l$sm_fixed[row_counter_2] <- trends_temp_l$smooth[row_counter_2] / trends_temp_l$smooth[row_counter]
    
    row_counter_2 <- row_counter_2 + 1
    
  }
  
  
  row_counter <- (25 * i) + 1
  
}

p1 <- ggplot(trends_temp, aes(x = time, y = imputed, group=interaction(species,trend))) +
  geom_point() +
  geom_line(aes(colour=interaction(species,trend)),size=1, show.legend=FALSE) +
  coord_cartesian(ylim=c(0, 2)) +
  scale_x_continuous(breaks=seq(1995,2015, by=10)) +
  ggtitle("Unsmoothed") +
  xlab("time") +
  ylab("index value") +
  facet_grid(species~trend) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = 'grey90'),
        panel.grid.minor = element_line(color = 'grey90'),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12, angle=0, hjust=0.5, vjust=0.5),
        axis.title.y=element_text(size=14, vjust=1),
        axis.title.x=element_text(size=14, vjust=0),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14),
        plot.title=element_text(size=16, hjust=0.5))

p2 <- ggplot(trends_temp, aes(x = time, y = imputed, group=interaction(species,trend))) +
  geom_point(aes(colour=interaction(species,trend)), show.legend=FALSE) +
  geom_smooth(aes(colour=interaction(species,trend)),method = "loess") +
  coord_cartesian(ylim=c(0, 2)) +
  scale_x_continuous(breaks=seq(1995,2015, by=10)) +
  xlab("time") +
  ylab("index value") +
  facet_grid(species~trend) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = 'grey90'),
        panel.grid.minor = element_line(color = 'grey90'),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12, angle=0, hjust=0.5, vjust=0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=14, vjust=0),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14))

p3 <- ggplot(trends_temp_l, aes(x = time, y = sm_fixed, group=interaction(species,trend))) +
  geom_point() +
  geom_line(aes(colour=interaction(species,trend)),size=1, show.legend=FALSE) +
  coord_cartesian(ylim=c(0, 2)) +
  scale_x_continuous(breaks=seq(1995,2015, by=10)) +
  ggtitle("Smoothed")+
  xlab("time") +
  #ylab("index value") +
  facet_grid(species~trend) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = 'grey90'),
        panel.grid.minor = element_line(color = 'grey90'),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12, angle=0, hjust=0.5, vjust=0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=14, vjust=0),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14),
        plot.title=element_text(size=16, hjust=0.5))

#same as p3 but with better y limits for plotting by itself
p4 <- ggplot(trends_temp_l, aes(x = time, y = sm_fixed, group=interaction(species,trend))) +
  geom_point() +
  geom_line(aes(colour=interaction(species,trend)),size=1, show.legend=FALSE) +
  coord_cartesian(ylim=c(0, 2)) +
  scale_x_continuous(breaks=seq(1995,2015, by=10)) +
  xlab("time") +
  ylab("index value") +
  facet_grid(species~trend) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = 'grey90'),
        panel.grid.minor = element_line(color = 'grey90'),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12, angle=0, hjust=0.5, vjust=0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=14, vjust=0),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14))

p_all <- p1 + p2 + p3

p1_3 <- p1 + p3

#ggsave(paste("c:/R_projects/Trend_compare/plots/bird_trends_fixed.jpg", sep=""), 
#       p_all, width=7480, height=9055, units="px", dpi=1000, scale=1, compression="lzw")

ggsave(paste("c:/R_projects/Trend_compare/figures/bird_trends_present.tiff", sep=""), 
       p1_3, width=7480, height=9055, units="px", dpi=1000, scale=1, compression="lzw")

trends.smoothed_subset <- subset(trends_temp_l, select=c(1,2,9,10,12))
trends.unsmoothed_subset <- subset(trends_temp_l, select=c(1,2,5,9,10))
trends.smoothed_list <- list()
trends.unsmoothed_list <- list()
counter <- 0
for(i in 1:10) {
  trends.smoothed_list[[i]] <- subset(trends.smoothed_subset, X==counter+1:25)
  trends.unsmoothed_list[[i]] <- trends.unsmoothed_subset[counter+1:25,]
  
  counter <- counter + 25
}

trends.unsm.res.list <- list()
trends.unsm.res.list[1:5] <- trends.unsmoothed_list[1:5]
trends.unsm.cf.list <- list()
trends.unsm.cf.list[1:5] <- trends.unsmoothed_list[6:10]
trends.sm.res.list <- list()
trends.sm.res.list[1:5] <- trends.smoothed_list[1:5]
trends.sm.cf.list <- list()
trends.sm.cf.list[1:5] <- trends.smoothed_list[6:10]
