# load distance measures
source("scripts/select_distance_measures.R")
source("scripts/ts_testing_uncon_plot.R")

# perform uncontrolled testing and save plot of results
uncon_plot_fn(time_series = testy,
              dataset_name = "yoga",
              q_min = 1,
              q_max = 200,
              increment = 10,
              package = "ph",
              fn_list = dist.fnlist2,
              args_list = dist.argslist2,
              names_list = dist.nameslist2,
              plotname_special = "test")


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

