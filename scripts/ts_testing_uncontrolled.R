library(TSdist)

source("scripts/ts_testing_uncontrolled_fns.R")
source("scripts/select_distance_measures.R")

# apply all functions at once to a chosen distance measure
test_fn <- function(t, q_min, q_max, increment, dm, ...) {
  
  library(dplyr)
  
  set.seed(23)
  
  param <- seq(q_min, q_max, increment)
  
  counter <- 1
  
  results <- list()
  
  for (i in param) {
    
    try(results$translate[counter] <- dm(ts_translate(t, (i/q_max)), t,...))

    try(results$phase[counter] <- dm(ts_phase(t, i), t,...))

    try(results$extend[counter] <- dm(ts_local_extend(t, i), t,...))

    try(results$stretch[counter] <- dm(ts_stretch(t, (1 + (i/q_max))), t,...)/length(t))

    try(results$whitenoise[counter] <- dm(t, ts_whitenoise(t, (i/(q_max))),...))

    try(results$biasednoise[counter] <- dm(ts_biasednoise(t, (i/(0.5*q_max))), t,...))

    try(results$outlier[counter] <- dm(ts_outlier(t, i), t,...))

    counter <- counter + 1
    
  }
  
  return(results)

}

# apply a chosen function to all distance measures at once
test_fn2 <- function(t, trans, q_min, q_max, increment, dm, ...) {
  
  param <- seq(q_min, q_max, increment)
  
  counter <- 1
  
  results <- vector()
  
  if (trans=="translate") {
    
    for (i in param) {
      
      results[counter] <- dm(t, ts_translate(t, (i/q_max)), ...)
      
      counter <- counter + 1
      
    }
    
  } else if (trans=="scale") {
    
    for (i in param) {
      
      results[counter] <- dm(t, ts_scale(t, (1 + (i/(0.5*length(t))))), ...)
      
      counter <- counter + 1
      
    }
    
  } else if (trans=="phase") {
    
    for (i in param) {
      
      try(results[counter] <- dm(t, ts_phase(t, i), ...))
      
      counter <- counter + 1
      
    }
    
  } else if (trans=="stretch") {
    
    for (i in param) {
      
      try(results[counter] <- dm(t, ts_stretch(t, (1 + (i/q_max))), ...))
      
      counter <- counter + 1
      
    }
    
  } else if (trans=="whitenoise") {
    
    for (i in param) {
      
      results[counter] <- dm(t, ts_whitenoise(t, (i/length(t))), ...)
      
      counter <- counter + 1
      
    }
    
  } else if (trans=="biasednoise") {
    
    for (i in param) {
      
      results[counter] <- dm(t, ts_biasednoise(t, (i/(0.5*length(t)))), ...)
      
      counter <- counter + 1
      
    }
    
  } else if (trans=="outlier") {
    
    for (i in param) {
      
      results[counter] <- dm(t, ts_outlier(t, i), ...)
      
      counter <- counter + 1
      
    }
    
  } else if (trans=="delete") {
    
    for (i in param) {
      
      try(results[counter] <- dm(t, ts_local_delete(t, i), ...))
      
      counter <- counter + 1
      
    }
    
  } else if (trans=="extend") {
    
    for (i in param) {
      
      try(results[counter] <- dm(t, ts_local_extend(t, i), ...))
      
      counter <- counter + 1
      
    }
    
  }
  
  results <- ifelse(results < 0.000001, 0, results)
  
  return(results)
  
}

# extra arguments to apply to distance measures
dist.argslist1.1 <- list(NULL, #Euclidean
                       NULL, #Manhattan
                       NULL, #Chebyshev
                       # c(p=3), #Minkowski
                       # NULL, #Dissim
                       NULL, #CID
                       NULL, #DTW
                       NULL, #TAM
                       NULL, #NCD
                       NULL, #CDM
                       c(g=0), #ERP
                       # c(epsilon=0.1), #LCSS
                       c(epsilon=0), #EDR
                       NULL, #Fourier
                       NULL, #ACF
                       NULL, #PACF
                       NULL, #Per
                       NULL, #IntPer
                       NULL, #Piccolo
                       # NULL, #PDC
                       # c(w=5), #SAX
                       NULL, #STS
                       # NULL, #CCor
                       NULL) #Cort
                       #NULL, #GLK
                       #NULL, #LLR
                       #NULL, #Hellinger
                       #c(alpha=50)) #MyDist

dist.argslist2.1 <- list(c(testNA=FALSE), #Additive
                       c(testNA=FALSE), #AVG
                       # c(testNA=FALSE, unit="log"), #Bhat
                       c(testNA=FALSE), #Canb
                       c(testNA=FALSE), #Clark
                       # c(testNA=FALSE), #Cosine
                       c(testNA=FALSE), #Czek
                       c(testNA=FALSE), #Dice
                       c(testNA=FALSE), #Diverge
                       # c(testNA=FALSE), #Fidelity
                       c(testNA=FALSE), #Gower
                       # c(testNA=FALSE), #Harm
                       # c(testNA=FALSE), #InnerP
                       # c(testNA=FALSE), #Intersect
                       c(testNA=FALSE), #Jaccard
                       c(testNA=FALSE, unit="log"), #Jeffreys
                       c(testNA=FALSE, unit="log"), #Jensen
                       # c(testNA=FALSE, unit="log"), #JenShan
                       c(testNA=FALSE), #Kulcz
                       c(testNA=FALSE, unit="log"), #Kullback
                       # c(testNA=FALSE), #KumarHass
                       c(testNA=FALSE), #KumarJohson
                       c(testNA=FALSE, unit="log"), #KDiv
                       c(testNA=FALSE, unit="log"), #Lorentz
                       # c(testNA=FALSE), #Matusita
                       # c(testNA=FALSE), #Motyka
                       # c(testNA=FALSE), #Neyman
                       # c(testNA=FALSE), #Pearson
                       c(testNA=FALSE), #ProbSymm
                       # c(testNA=FALSE), #Ruzicka
                       c(testNA=FALSE), #Soergel
                       c(testNA=FALSE), #SqChi
                       c(testNA=FALSE), #SqChord
                       c(testNA=FALSE), #SqEuclid
                       c(testNA=FALSE, unit="log"), #Taneja
                       # c(testNA=FALSE), #Tanimoto
                       c(testNA=FALSE, unit="log"), #Topsoe
                       c(testNA=FALSE)) #WaveHedges




library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)
library(dplyr)
library(plyr)
library(scales)


# colour blind friendly palette for plotting
#cbPalette <- c("#000000", #black
#               "#E69F00", #orange
#               "#56B4E9", #sky blue
#               "#009E73", #bluish green
#               "#0072B2", #blue
#               "#D55E00", #vermillion
#               "#CC79A7") #reddish purple

cbPalette <- c("#000000", #black
               "#E69F00", #orange
               "#0072B2", #blue
               "#D55E00", #vermillion
               "#CC79A7") #reddish purple

# names for plot legend labels
#plot.names <- c("Translation", 
#                "Phase", 
#                "Warping", 
#                "Uniform Time Scaling", 
#                "White Noise", 
#                "Biased Noise", 
#                "Outliers")

plot.names <- c("Translation",
                "Phase",
                "White Noise",
                "Biased Noise",
                "Outliers")

# create list for plots
p <- list()

for (i in seq_along(dist.fnlist2)) {
  
  if (!is.null(dist.argslist2[[i]])) {

    uncon_test <- do.call(test_fn, c(list(t=as.numeric(testy)),
                          list(q_min=1),
                          list(q_max=20),
                          list(increment=1),
                          list(dm=dist.fnlist2[[i]]),
                          dist.argslist2[[i]]))

  } else {
    
    uncon_test <- do.call(test_fn, c(list(t=as.numeric(testy)),
                          list(q_min=1),
                          list(q_max=20),
                          list(increment=1),
                          list(dm=dist.fnlist2[[i]])))
    
  }

uncon_test1.5 <- lapply (uncon_test, function(x) 
  {
  y <- vector()
  y <- x
  x[is.nan(x)] <- NA
  if (all(is.na(x))) {y <- NA}
  else if (abs(max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) > 0.00000001) 
  {y <- rescale(x, to = c(sample(seq(0,0.05, 0.01), 1), 
                          sample(seq(0.95,1.05, 0.01), 1)))}
  else if (abs(max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) == 0) 
    {y <- (x/x) * sample(seq(0.5, 1, 0.1), 1)}
  else {y <- x + sample(seq(0, 0.05, 0.01), 1)}
  return(y)
  }
  )
    
uncon_test2 <- as.data.frame(do.call(cbind, uncon_test1.5))

# turn results list into a long-form data frame
uncon_df <- ldply(uncon_test2, 
                  .fun = data.frame)

# name columns
colnames(uncon_df) <- c("Test", 
                        "Result")


uncon_df$Time <- rep(seq(1, length(uncon_test[[1]])), length(uncon_test))

#uncon_df$Test <- as.factor(uncon_df$Test)

#uncon_df$Test <- factor(uncon_df$Test, levels=c("translate",
#                                                "phase",
#                                                "extend",
#                                                "stretch",
#                                                "whitenoise",
#                                                "biasednoise",
#                                                "outlier"))

uncon_df$Test <- factor(uncon_df$Test, levels=c("translate",
                                                "phase",
                                                "whitenoise",
                                                "biasednoise",
                                                "outlier"))
#"apb"))

#uncon_wide <- reshape(uncon_df, 
#                      timevar="Test",
#                      idvar="Time",
#                      direction = "wide")

# create results plots ----


# plot all tests in one plot
# note that the legend is suppressed, but will be added later
p[[i]] <- ggplot(uncon_df, aes(x = Time, y = Result, group=Test)) +
  geom_point(aes(color=Test), size=2) +
  scale_colour_manual(labels=plot.names, values=cbPalette) +
  ggtitle(dist.nameslist2[[i]]) +
  stat_smooth(aes(color=Test), method="loess", span = 0.1, size=0.4) +
  theme(legend.title = element_text(color="blue")) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

}

# create a plot with a legend
p_legend <- ggplot(uncon_df, aes(x = Time, y = Result, group=Test)) +
  geom_point(aes(color=Test), size=2) +
  scale_colour_manual(labels=plot.names, values=cbPalette) +
  stat_smooth(aes(color=Test), method="loess", span = 0.1, size=0.4) +
  theme(legend.title = element_text(color="blue")) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.key.size=unit(0.7, "cm"),
        legend.text=element_text(size=10)) +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# Apply user-defined function to extract legend
shared_legend <- extract_legend(p_legend)

#tiff(filename=paste(wd, "/plots/realworld/", "batch1_plots.jpg", sep=""), 
#     width = 90, 
#     height = 160, 
#     units = "mm", 
#     res = 1000,
#     compression="lzw")

# arrange all the plots into one
p.all <- grid.arrange(arrangeGrob(p[[1]],
                      p[[2]],
                      p[[3]],
                      p[[4]],
                      p[[5]],
                      p[[6]],
                      p[[7]],
                      p[[8]],
                      p[[9]],
                      p[[10]],
                      p[[11]],
                      p[[12]],
                      p[[13]],
                      p[[14]],
                      p[[15]],
                      p[[16]],
                      p[[17]],
                      p[[18]],
                      p[[19]],
                      p[[20]],
                      p[[21]],
                      p[[22]],
                      p[[23]],
                      p[[24]],
                      ncol=3),
                      heights=c(10,1),
                      left=textGrob(paste("Dissimilarity", sep=""),
                                    gp=gpar(fontsize=16),
                                    rot=90),
                      shared_legend,
                      top = textGrob(paste("Uncontrolled Test Results", sep=""),
                                     gp=gpar(fontsize=16)))


#dev.off()

#wd <- getwd()

# write the grid-arranged plot as a tiff image
ggsave("plots/realworld/yoga1_1to200_incr10_phil.tiff", p.all,
       compression="lzw", dpi=1000, height=10000, width=7500, units="px")

tiff(filename="plots/realworld/yoga_and_synthcontrol_ts_plot.jpg", 
          width = 5000, 
          height = 3000, 
          units = "px", 
          res = 1000,
          compression="lzw")
par(mfrow=c(2,1), mai=c(0.5,0.5,0.1,0.1))
plot(1:427, testy2, type="l", xlab="Time", ylab="", ylim=c(-2,2))
plot(1:61, tests2, type="l", xlab="Time", ylab="", ylim=c(-2,2))
dev.off()

#################
 
p <- list()
for (i in 1:length(uncon_test)) {
  new_df <- data.frame(Time = uncon_wide[,1], Results = uncon_wide[,(i+1)])
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(uncon_wide[,(i+1)])) 
  {
    
    # save ggplot of Translation sensitivity
    p[[i]] <- ggplot(new_df,
                     aes(x = Time, 
                         y = Results)) +
      geom_point(size=2) +
      #ylim(ymin[[i]], ymax[[i]]) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle(plot.names[i]) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p[[i]] <- ggplot() +
      theme_void()
    
  }
  
}

# arrange all the plots into one
p.all <- grid.arrange(p[[1]],
                      p[[2]],
                      p[[3]],
                      p[[4]],
                      p[[5]],
                      p[[6]],
                      p[[7]],
                      p[[8]],
                      p[[9]],
                      ncol=4,
                      top = textGrob(paste("Euclidean Test Results", sep=""),
                                     gp=gpar(fontsize=16,
                                             font=4))
                      )


trans_invivo <- list()

for (i in seq_along(dist.fnlist1)) {
  
  if (!is.null(dist.argslist1.1[[i]])) {
    
    trans_invivo[[i]] <- test_fn2(t=as.numeric(sc_test_noclass[23,]),
                                  trans="translate",
                                  q_min=1,
                                  q_max=100,
                                  increment=1,
                                  dm=dist.fnlist1[[i]],
                                  dist.argslist1.1[[i]])
    
  } else {
    
    trans_invivo[[i]] <- test_fn2(t=as.numeric(sc_test_noclass[23,]),
                                  trans="translate",
                                  q_min=1,
                                  q_max=100,
                                  increment=1,
                                  dm=dist.fnlist1[[i]])
    
  }
  
}

trans_invivo2 <- as.data.frame(do.call(cbind, trans_invivo))
colnames(trans_invivo2) <- dist.nameslist1
#time <- seq(1, length(trans_invivo2[,1]))
#trans_invivo2 <- cbind(trans_invivo2, time)
trans_invivo_long <- reshape(trans_invivo2, 
                             varying=as.vector(dist.nameslist1),
                             v.names="distance",
                             idvar="size",
                             timevar="DM",
                             times=as.vector(dist.nameslist1),
                             new.row.names = 1:(ncol(trans_invivo2)*nrow(trans_invivo2)),
                             direction = "long")

ggplot(trans_invivo_long, aes(x = size, y = log(distance+1), group=DM))+
  geom_point(aes(color=DM)) +
  geom_jitter(aes(color=DM), width=1)
#stat_smooth(aes(color=DM), method="loess", span = 0.8)

