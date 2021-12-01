

# --

## load libraries ----

library(TSdist)
library(philentropy)
library(ggplot2)
library(ggpubr)

# Load bird trends ----

source("scripts/bird_trends.R")

# Load functions ----

source("scripts/get.dm.plots.birds.R")

## Prepare unsmoothed bird result plots ----

# create list of chosen distance measures with names and arguments
dist.fnlist.usplots2 <- list(k_divergence, kullback_leibler_distance)
dist.nameslist.usplots2 <- c("KDiv", "Kullback")
dist.argslist.usplots2 <- list(list(testNA=FALSE, unit="log"), list(testNA=FALSE, unit="log"))

# create plots for all distance measures in the list and put into a list of plots
unsmoothed.plots2 <- list()
for (i in seq_along(dist.fnlist.usplots2)) {
  
  unsmoothed.plots2[[i]] <- get.dm.plot.birds1(dist.fnlist.usplots2[[i]], dist.nameslist.usplots2[i], dist.argslist.usplots2[[i]])[[1]]
  
}

# create a data frame with percent improvement and t-test results from Jellesmark et al (2021)
sj_results <- data.frame("x" = c(3,1,4,2,5), "Percent Improvement" = c(108, 57, 36, 33, 0), "T-Test" = c(9.4, 7.6, 4.0, 5.4, -0.3), "Species" = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"))

# convert species column to a factor
sj_results$Species <- factor(sj_results$Species, levels = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"))

# create ymin and ymax for plotting
ymin.pi <- min(sj_results$Percent.Improvement) - 0.1*min(abs(sj_results$Percent.Improvement))
ymax.pi <- max(sj_results$Percent.Improvement) + 0.1*max(abs(sj_results$Percent.Improvement))
ymin.tt <- min(sj_results$T.Test) - 0.1*min(abs(sj_results$T.Test))
ymax.tt <- max(sj_results$T.Test) + 0.1*max(abs(sj_results$T.Test))

# create percent improvement plot
pi.birdplot <- ggplot(sj_results,
                      aes(x = x, 
                          y = Percent.Improvement,
                          colour = Species,
                          shape = Species)) +
  geom_point(size=4) +
  ylim(ymin.pi, ymax.pi) +
  xlim(0, 6) +
  theme(legend.title = element_text(color="blue")) +
  ggtitle(paste("% Improvement")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# create t-test plot
tt.birdplot <- ggplot(sj_results,
                      aes(x = x, 
                          y = T.Test,
                          colour = Species,
                          shape = Species)) +
  geom_point(size=4) +
  ylim(ymin.tt, ymax.tt) +
  xlim(0, 6) +
  theme(legend.title = element_text(color="blue")) +
  ggtitle(paste("T-Test")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# plots to unsmoothed plots list
unsmoothed.plots2[[3]] <- pi.birdplot
unsmoothed.plots2[[4]] <- tt.birdplot

## Prepare smoothed bird result plots ----

# create lists of chosen distance measures with names and arguments
dist.fnlist.plots <- list(EuclideanDistance, ERPDistance, ManhattanDistance)
dist.nameslist.plots <- c("Euclidean", "ERP", "Manhattan")
dist.argslist.plots <- list(NULL, list(g=0), NULL)
dist.fnlist.plots2 <- list(avg, gower, lorentzian, squared_euclidean)
dist.nameslist.plots2 <- c("AVG", "Gower", "Lorentz", "Sq. Euclid")
dist.argslist.plots2 <- list(list(testNA=FALSE), list(testNA=FALSE), list(testNA=FALSE, unit="log"), list(testNA=FALSE))

# create plots for distance measures in the lists and put into lists of plots
smoothed.plots <- list()
smoothed.plots2 <- list()
for (i in seq_along(dist.fnlist.plots)) {
  
  smoothed.plots[[i]] <- get.dm.plot.birds1(dist.fnlist.plots[[i]], dist.nameslist.plots[i], dist.argslist.plots[[i]])[[2]]
  
}
for (i in seq_along(dist.fnlist.plots2)) {
  
  smoothed.plots2[[i]] <- get.dm.plot.birds1(dist.fnlist.plots2[[i]], dist.nameslist.plots2[i], dist.argslist.plots2[[i]])[[2]]
  
}

# add percent improvement and t-test plots to the plot lists
#smoothed.plots[[4]] <- pi.birdplot
#smoothed.plots2[[5]] <- tt.birdplot

## Create bird result plots ----

## plot chosen distance measure results for smoothed bird trends

# arrange chosen unsmoothed results into one plot
gg_unsmoothed <- ggarrange(unsmoothed.plots2[[1]], 
                           unsmoothed.plots2[[2]], 
                           unsmoothed.plots2[[3]], 
                           unsmoothed.plots2[[4]],
                           ncol = 4, 
                           nrow = 1, 
                           legend = c("right"), 
                           common.legend = TRUE)

# add title and y-axis title
gg_unsmoothed <- annotate_figure(gg_unsmoothed, 
                                 left = text_grob("Distance Value", 
                                                  size = 12,
                                                  rot = 90,
                                                  hjust = 0.7),
                                 top = text_grob("Unsmoothed", 
                                                 size = 20,
                                                 hjust = 0.8,
                                                 vjust = 0))

# add margins at top and bottom
gg_unsmoothed <- gg_unsmoothed + theme(plot.margin = margin(0.5, 0, 0.5, 0, "cm"))

#ggsave(paste("figures/unsmoothed_plots.tiff", sep=""), 
#       gg_unsmoothed, 
#       width=7480, 
#       height=2494, 
#       units="px", 
#       dpi=1000, 
#       scale=1, 
#       compression = "lzw")

## plot chosen distance measure results for smoothed bird trends

# arrange all chosen smoothed results into one plot
gg_smoothed <- ggarrange(smoothed.plots[[1]], 
                         smoothed.plots[[2]], 
                         smoothed.plots[[3]], 
                         #smoothed.plots[[4]], 
                         smoothed.plots2[[1]], 
                         smoothed.plots2[[2]], 
                         smoothed.plots2[[3]], 
                         smoothed.plots2[[4]],
                         ncol = 4, 
                         nrow = 2, 
                         legend = c("right"), 
                         common.legend = TRUE)

# add title and y-axis title
gg_smoothed <- annotate_figure(gg_smoothed, 
                               left = text_grob("Distance Value", 
                                                size = 12,
                                                rot = 90),
                               top = text_grob("Smoothed", 
                                               size = 20,
                                               hjust = 0.9,
                                               vjust = 0))

# add margin at top
gg_smoothed <- gg_smoothed + theme(plot.margin = margin(1, 0, 0, 0, "cm"))

# save to tiff file
#ggsave(paste("figures/smoothed_plots.tiff", sep=""), 
#       gg_smoothed, 
#       width=7480, 
#       height=4986, 
#       unit="px", 
#       dpi=1000, 
#       scale=1, 
#       compression = "lzw")

## plot chosen distance measures results for both smoothed and unsmoothed bird trends

# arrange chosen smoothed and unsmoothed results into one plot
gg_all <- ggarrange(gg_unsmoothed, 
                    gg_smoothed, 
                    ncol = 1, 
                    nrow = 2, 
                    heights = c(1,2),
                    legend = c("right"), 
                    common.legend = TRUE)

# open a tiff file to save plot
if(!dir.exists("figures/")) {dir.create("figures/")}
tiff("figures/bird_dm_plots_all_chosen.tiff", 
     width = 7480, 
     height = 10000, 
     unit = "px", 
     res = 1000,
     compression = "lzw",
     pointsize = 10)

# draw the plot, then add a horizontal line separating smoothed and unsmoothed
grid.newpage()
grid.draw(gg_all)
grid.polygon(x = c(0, 1), y = c(0.66, 0.66))

# close tiff file
dev.off()
