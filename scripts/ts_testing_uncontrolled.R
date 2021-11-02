# translation sensitivity
ts_translate <- function(t, q) {

  u <- t + q
  
  return(u)
  
}

# scale sensitivity
ts_scale <- function(t, q) {
  
  u <- t * q
  
  return(u)
  
}

# white noise sensitivity
ts_whitenoise <- function(t, q) {
  
  norm_dist <- rnorm(length(t), mean = q, sd = 0.3*q)
  
  rand <- sample(seq_along(t), floor(length(t)/2), replace=FALSE)
  
  rand_2 <- seq_along(t)[-rand]
  
  u <- t
  
  for (i in rand) {
    
    u[i] <- t[i] + sample(norm_dist, 1)
    
  }
  
  for (i in rand_2) {
    
    u[i] <- t[i] - sample(norm_dist, 1)
    
  }
  
  #u[rand] <- t[rand] + q
  
  #u[-rand] <- t[-rand] - q
  
  
  return(u)
  
}

# biased noise sensitivity
ts_biasednoise <- function(t, q) {
  
  norm_dist <- rnorm(length(t), mean = q, sd = 0.3*q)
  
  rand <- sample(seq_along(t), floor(length(t)/2), replace=FALSE)
  
  u <- t
  
  for (i in rand) {
    
    u[i] <- t[i] + sample(norm_dist, 1)
    
  }
  
  #u[rand] <- t[rand] + q

  return(u)
  
}

# outlier sensitivity
ts_outlier <- function(t, q) {
  
  rand <- sample(seq_along(t[2:(length(t)-1)]), 1)
  
  u <- t
  
  u[rand] <- t[rand] + q
  
  return(u)
  
}

# antiparallelism bias
ts_apb <- function(t, q) {
  
  if (q >= 1) {
    
    stop("q is a portion of time series t, and must be less than 1")
    
  }
  
  num_to_flip <- floor(q * length(t))
  
  if (num_to_flip > (length(t)-1)) {
    
    stop("q is too large")
    
  }
  
  full_seq <- seq_along(t)
  
  can_flip <- full_seq[2:(length(t)-(num_to_flip-1))]
  
  ifelse(length(can_flip) > 1, start <- sample(can_flip, 1), start <- can_flip)
  
  which_to_flip <- full_seq[start:(start+(num_to_flip-1))]
  
  u <- t
  
  v <- t
  
  counter <- 0
  
  for (i in which_to_flip) {
    
    temp <- t[i] - t[i-1]
    
    u[i] <- u[i-1] - temp
    
    v[i] <- t[i] + (t[i]-u[i])
    
    if (u[i] == t[i]) {
      
      counter <- counter + 1
    }
    
  }
  
  if (counter == length(which_to_flip)) {
    
    warning("The transformed time series is identical to the original. 
         This occurs when the transformation is applied to a flat section of the
         time series (no change in values). Try making q larger.")
    
  }
  
  return(list(u,v))  

    
}

# phase invariance
ts_phase <- function(t, q) {
  
  if (round(q)!=q) {
    
    stop("Input for q must be a whole number.")
  }

  u <- c(t[(q+1):length(t)], t[1:q])
  
  return(u)
  
}

# warping invariance
ts_local_extend <- function(t, q) {
  
  if (round(q)!=q) {
    
    stop("Input for q must be a whole number.")
  }
  
  rand <- sample(seq(1, length(t)), 1)
  
  u <- c(t[1:rand], rep(t[rand], (q-1)), t[rand:length(t)])
  
  return(u)
  
}

# deletion invariance??
ts_local_delete <- function(t, q) {
  
  if (round(q)!=q) {
    
    stop("q must be a whole number")
  }
  
  if (q > (length(t)-1)) {
    
    stop("q must be smaller than the length of the time series")
    
  }
  
  rand <- sample(seq(1, ((length(t)+1)-q)), 1)
  
  if (rand==1) {
    
    u <- t[(q+1):length(t)]
    
  } else if ((rand+q) > length(t)) {
    
    u <- t[1:(rand-1)]
    
  } else {
    
    u <- c(t[1:(rand-1)], t[(rand+q):length(t)])
    
  }

  return(u)
  
}

# uniform time scale invariance
ts_stretch <- function(t, q) { # this is a stretch function, so q should be greater than 1
  
  u <- t
  
  t_x <- seq(0, (length(t)-1))
  
  u_x <- t_x * q
  
  temp_t <- data.frame(x=t_x, y=t)
  
  temp1 <- data.frame(x=u_x, y=u)
  
  temp2 <- data.frame(x=seq(1, floor(max(u_x))), y=NA)
  
  temp3 <- anti_join(temp2, temp1, by="x")
  
  temp4 <- full_join(temp1, temp3, by="x")
  
  temp5 <- temp4[order(temp4$x),1:2]
  
  colnames(temp5) <- c("x", "y")
  
  first_val <- min(temp5$x[!is.na(temp5$y)])
  
  final_val <- max(temp5$x[!is.na(temp5$y)])
  
  all_na <- temp5$x[is.na(temp5$y)]
  
  all_vals <- temp5$x[!is.na(temp5$y)]
  
  for (i in all_na[all_na > first_val]) {
    
    num_total <- min(all_vals[all_vals > i]) - max(all_vals[all_vals < i])
    
    num_high <- min(all_vals[all_vals > i]) - i
    
    num_low <- i - max(all_vals[all_vals < i])
    
    diff_total <- abs((temp5$y[temp5$x==max(all_vals[all_vals < i])] - temp5$y[temp5$x==min(all_vals[all_vals > i])]))
    
    actual_diff <- (num_low / num_total) * diff_total
    
    if (temp5$y[temp5$x==max(all_vals[all_vals < i])] < temp5$y[temp5$x==min(all_vals[all_vals > i])]) {
      
      temp5$y[temp5$x==i] <- temp5$y[temp5$x==max(all_vals[all_vals < i])] + actual_diff
      
    } else {
      
      temp5$y[temp5$x==i] <- temp5$y[temp5$x==max(all_vals[all_vals < i])] - actual_diff
      
    }
    
  }

  return(as.numeric(temp5$y[temp5$x==round(temp5$x)]))
  
}

# apply all functions at once to a chosen distance measure
test_fn <- function(t, q_min, q_max, increment, dm, ...) {
  
  library(dplyr)
  
  param <- seq(q_min, q_max, increment)
  
  counter <- 1
  
  results <- list()
  
  for (i in param) {
    
    try(results$translate[counter] <- dm(t, ts_translate(t, (i/length(t))), ...))
    
   # try(results$scale[counter] <- dm(t, ts_scale(t, (1 + (i/(0.5*length(t))))), ...))
    
    try(results$phase[counter] <- dm(t, ts_phase(t, i), ...))
    
   # try(results$delete[counter] <- dm(t, ts_local_delete(t, i), ...))
    
    try(results$extend[counter] <- dm(t, ts_local_extend(t, i), ...))
    
    try(results$stretch[counter] <- dm(t, ts_stretch(t, (1 + (i/q_max))), ...))
    
    try(results$whitenoise[counter] <- dm(t, ts_whitenoise(t, (i/length(t))), ...))
    
    try(results$biasednoise[counter] <- dm(t, ts_biasednoise(t, (i/(0.5*length(t)))), ...))
    
    try(results$outlier[counter] <- dm(t, ts_outlier(t, i), ...))
    
    #results$apb[counter] <- dm(t, ts_apb(t, i/(length(t))), ...)
    
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
      
      results[counter] <- dm(t, ts_translate(t, (i/length(t))), ...)
      
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
                       # NULL, #NCD
                       # NULL, #CDM
                       c(g=0), #ERP
                       # c(epsilon=0.1), #LCSS
                       c(epsilon=0.3), #EDR
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
                       NULL, #Cort
                       #NULL, #GLK
                       #NULL, #LLR
                       #NULL, #Hellinger
                       c(alpha=50)) #MyDist

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


# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}


library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)
library(dplyr)
library(plyr)


# colour blind friendly palette for plotting
cbPalette <- c("#000000", #black
               #"#999999", #grey
               "#E69F00", #orange
               "#56B4E9", #sky blue
               "#009E73", #bluish green
               #"#F0E442", #yellow
               "#0072B2", #blue
               "#D55E00", #vermillion
               "#CC79A7") #reddish purple

# names for plot legend labels
plot.names <- c("Translation", 
                #"Scaling", 
                "Phase", 
                "Warping", 
                "Uniform Time Scaling", 
                "White Noise", 
                "Biased Noise", 
                "Outliers")
                #"Antiparallelism")

# create list for plots
p <- list()

for (i in seq_along(dist.fnlist1)) {
  
  if (!is.null(dist.argslist1.1[[i]])) {

    uncon_test <- test_fn(t=as.numeric(y_test),
                          #trans="translate",
                          q_min=1,
                          q_max=30,
                          increment=1,
                          dm=dist.fnlist1[[i]],
                          dist.argslist1.1[[i]])

  } else {
    
    uncon_test <- test_fn(t=as.numeric(y_test),
                          #trans="translate",
                          q_min=1,
                          q_max=30,
                          increment=1,
                          dm=dist.fnlist1[[i]])
    
  }
  
uncon_test2 <- as.data.frame(do.call(cbind, uncon_test))

# turn results list into a long-form data frame
uncon_df <- ldply(uncon_test2, 
                  .fun = data.frame)

# name columns
colnames(uncon_df) <- c("Test", 
                        "Result")


uncon_df$Time <- rep(seq(1, length(uncon_test[[1]])), length(uncon_test))

#uncon_df$Test <- as.factor(uncon_df$Test)

uncon_df$Test <- factor(uncon_df$Test, levels=c("translate",
                                                #"scale",
                                                "phase",
                                                "extend",
                                                "stretch",
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
  ggtitle(dist.nameslist1[[i]]) +
  stat_smooth(aes(color=Test), method="loess", span = 0.1, size=0.4) +
  theme(legend.title = element_text(color="blue")) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank())
        #axis.text.y = element_blank())

}

# create a plot with a legend
p_legend <- ggplot(uncon_df, aes(x = Time, y = Result, group=Test)) +
  geom_point(aes(color=Test), size=2) +
  scale_colour_manual(labels=plot.names, values=cbPalette) +
  stat_smooth(aes(color=Test), method="loess", span = 0.1, size=0.4) +
  theme(legend.title = element_text(color="blue")) +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank())
        #axis.text.y = element_blank())

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
                      ncol=3),
                      shared_legend,
                      #nrow=2,
                      heights=c(10,1),
                      #respect=TRUE,
                      top = textGrob(paste("Test Results", sep=""),
                                     gp=gpar(fontsize=16,
                                             font=4)))


#dev.off()

#wd <- getwd()

# write the grid-arranged plot as a jpg image
#ggsave(paste(wd, "/plots/realworld/", "batch1_plots.jpg", sep=""), p.all)



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
