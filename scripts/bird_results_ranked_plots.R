

# --

## load libraries ----

library(TSdist)
library(philentropy)
library(ggplot2)
library(ggpubr)

# Load bird trends ----

source("scripts/bird_trends.R")

# Load functions ----

source("scripts/get.ranked.bird.results.R")

# load distance measures ----

source("scripts/select_distance_measures.R")

# get names and arguments for all distance measures
dist.names <- c(dist.nameslist1, dist.nameslist2)
dist.args <- c(dist.argslist1, dist.argslist2)
dist.fn <- c(dist.fnlist1, dist.fnlist2)

## get bird results and rank them ----
bird.results.un <- list()
bird.results.sm <- list()
for (i in 1:length(dist.names)) {
  
bird.results.un[[i]] <- get.simp.unsmoothed.result.birds(dist.fn[[i]],
                                            dist.names[[i]],
                                            dist.args[[i]])

bird.results.un[[i]]$rank <- rank(-bird.results.un[[i]]$y, ties.method="first")

bird.results.sm[[i]] <- get.simp.smoothed.result.birds(dist.fn[[i]],
                                                         dist.names[[i]],
                                                         dist.args[[i]])

bird.results.sm[[i]]$rank <- rank(-bird.results.sm[[i]]$y, ties.method="first")

}
#names(bird.results.un) <- dist.names
#names(bird.results.sm) <- dist.names

# create a data frame with percent improvement and t-test results from Jellesmark et al (2021)
pi_results <- data.frame("y" = c(108, 57, 36, 33, 0), 
                         "Species" = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"),
                         "DM" = "%Improvement")
tt_results <- data.frame("y" = c(9.4, 7.6, 4.0, 5.4, -0.3), 
                         "Species" = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"),
                         "DM" = "T.Test")

pi_results$rank <- rank(-pi_results$y)
tt_results$rank <- rank(-tt_results$y)

bird.results.un$pi <- list()
bird.results.sm$tt <- list()

bird.results.un$pi <- pi_results
bird.results.sm$pi <- pi_results
bird.results.un$tt <- tt_results
bird.results.sm$tt <- tt_results

bird.res.un.vec <- do.call(rbind, bird.results.un)
bird.res.sm.vec <- do.call(rbind, bird.results.sm)

bird.res.un.vec$DM[bird.res.un.vec$DM=="KumarJohnson"]<-"Kumar"
bird.res.un.vec$DM[bird.res.un.vec$DM=="WaveHedges"]<-"Wave"
bird.res.un.vec$DM[bird.res.un.vec$DM=="Kullback"]<-"*Kullback"
bird.res.un.vec$DM[bird.res.un.vec$DM=="KDiv"]<-"*KDiv"
bird.res.un.vec$DM[bird.res.un.vec$DM=="EDR"]<-"\u2020EDR"
bird.res.un.vec$DM[bird.res.un.vec$DM=="TAM"]<-"\u2020TAM"

bird.res.sm.vec$DM[bird.res.sm.vec$DM=="KumarJohnson"]<-"Kumar"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="WaveHedges"]<-"Wave"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="ERP"]<-"*ERP"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="Euclidean"]<-"*Euclidean"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="Manhattan"]<-"*Manhattan"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="Gower"]<-"*Gower"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="Lorentz"]<-"*Lorentz"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="AVG"]<-"*AVG"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="SqEuclid"]<-"*SqEuclid"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="Chebyshev"]<-"\u2020Chebyshev"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="Fourier"]<-"\u2020Fourier"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="KDiv"]<-"\u2020KDiv"
bird.res.sm.vec$DM[bird.res.sm.vec$DM=="Kullback"]<-"\u2020Kullback"


bird.res.un.vec$family <- rep(c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", "Elastic", "Elastic", 
                                                             "Feature", "Feature",
                                                             "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
                                                             "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon's_Entropy", "Shannon's_Entropy",
                                                             "Intersection", "Shannon's_Entropy", "Other", "Shannon's_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
                                                             "Squared_L2", "Other", "Shannon's_Entropy", "Intersection", "Jellesmark", "Jellesmark"), each=5)

bird.res.un.vec <- bird.res.un.vec[order(bird.res.un.vec$family, bird.res.un.vec$DM),]

bird.res.sm.vec$family <- rep(c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", "Elastic", "Elastic", 
                                "Feature", "Feature",
                                "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
                                "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon's_Entropy", "Shannon's_Entropy",
                                "Intersection", "Shannon's_Entropy", "Other", "Shannon's_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
                                "Squared_L2", "Other", "Shannon's_Entropy", "Intersection", "Jellesmark", "Jellesmark"), each=5)

bird.res.sm.vec <- bird.res.sm.vec[order(bird.res.sm.vec$family, bird.res.sm.vec$DM),]

# reorder type column
bird.res.un.vec$family <- factor(bird.res.un.vec$family, levels=rev(c("Compression", "Model", "Feature", "Other", "Elastic", "Intersection", 
                                            "Inner_Product", "Fidelity", "Shannon's_Entropy", "Squared_L2", "L1", "Minkowski", "Jellesmark")),
                      labels=rev(c("Compression-Based", "Model-Based", "Feature-Based", "Other Shape-Based", "Elastic", "Intersection Family",  
                               "Inner Product Family", "Fidelity Family", "Shannon's Entropy Family", "Squared L2 Family", "L1 Family", "Minkowski Family", "Jellesmark et al")))

bird.res.sm.vec$family <- factor(bird.res.sm.vec$family, levels=rev(c("Compression", "Model", "Feature", "Other", "Elastic", "Intersection", 
                                                                      "Inner_Product", "Fidelity", "Shannon's_Entropy", "Squared_L2", "L1", "Minkowski", "Jellesmark")),
                                 labels=rev(c("Compression-Based", "Model-Based", "Feature-Based", "Other Shape-Based", "Elastic", "Intersection Family",  
                                              "Inner Product Family", "Fidelity Family", "Shannon's Entropy Family", "Squared L2 Family", "L1 Family", "Minkowski Family", "Jellesmark et al")))

library(viridis)
# create unsmoothed results plot
unsm <- ggplot(bird.res.un.vec, aes(x = rank, y = DM, colour=Species, group=family)) +
  geom_point(size=3, colour="white") +
  geom_text(aes(label=Species), size=4)+
  ggtitle(paste("Unsmoothed Rankings")) +
  facet_grid(family~., scales="free", drop = TRUE, space="free",
             labeller  = label_wrap_gen(width = 17))+
  scale_fill_viridis() +
  lims(x=c(0.5,5.5))+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text.y = element_text(angle=0, size=10),
        panel.spacing = unit(2, "pt"),
        legend.position="none")

# create grob file and edit to change colours of certain DMs on y-axis of plot
unsm.grob <- ggplotGrob(unsm)
unsm.grob$grobs[[21]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark red", "dark red", "grey30", "grey30", "grey30")
unsm.grob$grobs[[25]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark green", "dark green", "grey30", "grey30")
unsm.grob$grobs[[21]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 2, 1, 1, 1))
unsm.grob$grobs[[25]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 2, 1, 1))
grid::grid.newpage()
grid::grid.draw(unsm.grob)

# save plot
ggsave(file="unsmoothed_birds.tiff", 
       unsm.grob, 
       device=tiff,
       width=8000, 
       height=10000, 
       units="px", 
       dpi=1000, 
       scale=1, 
       compression = "lzw")

# create smoothed results plot
sm <- ggplot(bird.res.sm.vec, aes(x = rank, y = DM, colour=Species, group=family)) +
  geom_point(size=3, colour="white") +
  geom_text(aes(label=Species), size=4)+
  ggtitle(paste("Smoothed Rankings")) +
  facet_grid(family~., scales="free", drop = TRUE, space="free",
             labeller  = label_wrap_gen(width = 17))+
  scale_fill_viridis() +
  lims(x=c(0.5,5.5))+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text.y = element_text(angle=0, size=10),
        panel.spacing = unit(2, "pt"),
        legend.position="none")

# create grob file and edit to change colours of certain DMs on y-axis of plot
sm.grob <- ggplotGrob(sm)
sm.grob$grobs[[18]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark red", "dark red", "dark green")
sm.grob$grobs[[19]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark red", "dark red", "grey30", "grey30")
sm.grob$grobs[[20]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark red", "grey30", "grey30", "grey30", "grey30", "grey30")
sm.grob$grobs[[21]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark green", "dark green", "grey30", "grey30", "grey30")
sm.grob$grobs[[25]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark red", "grey30", "grey30", "grey30")
sm.grob$grobs[[26]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark red", "grey30", "grey30", "grey30", "grey30", "grey30")
sm.grob$grobs[[27]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark green", "grey30", "grey30", "grey30", "grey30")
sm.grob$grobs[[18]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 2, 2))
sm.grob$grobs[[19]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 2, 1, 1))
sm.grob$grobs[[20]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 1, 1, 1, 1, 1))
sm.grob$grobs[[21]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 2, 1, 1, 1))
sm.grob$grobs[[25]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 1, 1, 1))
sm.grob$grobs[[26]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 1, 1, 1, 1, 1))
sm.grob$grobs[[27]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 1, 1, 1, 1))
grid::grid.newpage()
grid::grid.draw(sm.grob)

# save plot
ggsave(file="smoothed_birds.tiff", 
       sm.grob, 
       device=tiff,
       width=8000, 
       height=10000, 
       units="px", 
       dpi=1000, 
       scale=1, 
       compression = "lzw")
