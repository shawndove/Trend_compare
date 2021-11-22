# function to perform uncontrolled testing on a list of distance measures 
# and plot all results in a single plot which is saved as a compressed tiff

uncon_plot_fn <- function(time_series,
                          dataset_name, # name of the dataset the time series is from
                          q_min = 1, 
                          q_max = 20, 
                          increment = 1,
                          package = "ts", # ts for TSdist, ph for philentropy
                          fn_list,
                          args_list,
                          names_list,
                          plotname_special="") # add text to the default filename
                                               # recommend starting with underscore
                                               # e.g., "_test"
{
  
  # load libraries
  library(ggplot2)
  library(patchwork)
  library(gridExtra)
  library(grid)
  library(plyr)
  library(dplyr)
  library(scales)
  library(TSdist)

  # load functions
  source("scripts/ts_testing_uncontrolled_fns.R")

  if (package=="ts") {
    
    # colour blind friendly palette for plotting
    cbPalette <- c("#000000", #black
                   "#E69F00", #orange
                   "#56B4E9", #sky blue
                   "#009E73", #bluish green
                   "#0072B2", #blue
                   "#D55E00", #vermillion
                   "#CC79A7") #reddish purple
    
    # names for plot legend labels
    plot.names <- c("Translation", 
                    "Phase", 
                    "Warping", 
                    "Uniform Time Scaling", 
                    "White Noise", 
                    "Biased Noise", 
                    "Outliers")
    
  } else if (package=="ph") {
    
    # colour blind friendly palette for plotting
    cbPalette <- c("#000000", #black
                   "#E69F00", #orange
                   "#0072B2", #blue
                   "#D55E00", #vermillion
                   "#CC79A7") #reddish purple
    
    # names for plot legend labels
    plot.names <- c("Translation",
                    "Phase",
                    "White Noise",
                    "Biased Noise",
                    "Outliers")
    
  } else {
    
    # stop the function
    stop("You have specified the package incorrectly. Please enter 'ts' or 'ph'.")
    
  }
  
  # create list for plots
  p <- list()
  
  # loop along list of distance measures
  for (i in seq_along(fn_list)) {
    
    # check for extra arguments
    #if (args_list[[i]]!="NULL") {
      
      # call the function
      uncon_test <- do.call(test_fn, c(list(t=as.numeric(time_series)),
                                       list(q_min),
                                       list(q_max),
                                       list(increment),
                                       list(dm=fn_list[[i]]),
                                       args_list[[i]]))
      
    # rescale results to [0,1]
    uncon_test1.5 <- lapply(uncon_test, function(x) {
      
      y <- vector()
      
      y <- x
      
      # replace NaN values with NA
      x[is.nan(x)] <- NA
      
      # if all values of x are NA, replace all y values with NA
      if (all(is.na(x))) {y <- NA}
      
      # if the maximum absolute value of x is approximately 0, convert all values
      # of x to the same randomly chosen value near 0 to convey invariance
      else if (abs(max(x, na.rm=TRUE)) <= 0.00000001) {y <- x + sample(seq(0, 0.05, 0.01), 1)}
      
      # otherwise, check whether the range of x is approximately 0
      else if (abs(max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) > 0.00000001) 
        
        # if the range is NOT extremely small, rescale x to approximately [0,1]
        # note that the starting and ending values of the range are chosen randomly
        # to ensure results will be visible even if the curves have the same shape
      {y <- rescale(x, to = c(sample(seq(0,0.05, 0.01), 1), 
                              sample(seq(0.95,1.05, 0.01), 1)))}
      
      
      else 
        
        # # if the range IS extremely small, convert all values to the same randomly chosen value
        # between 0.5 and 1. This is to convey insensitivity while avoiding overplotting
      {y <- (x/x) * sample(seq(0.5, 1, 0.1), 1)}
      
      return(y) 
      
    })
    
    # convert results to a data frame
    uncon_test2 <- as.data.frame(do.call(cbind, uncon_test1.5))
    
    # convert results to long form
    uncon_df <- ldply(uncon_test2, 
                      .fun = data.frame)
    
    # name columns
    colnames(uncon_df) <- c("Test", 
                            "Result")
    
    # add a time column for plotting purposes
    uncon_df$Time <- rep(seq(1, length(uncon_test[[1]])), length(uncon_test))
    
    if (package=="ts") {
      
      # convert test column to a factor
      uncon_df$Test <- factor(uncon_df$Test, levels=c("translate",
                                                      "phase",
                                                      "extend",
                                                      "stretch",
                                                      "whitenoise",
                                                      "biasednoise",
                                                      "outlier"))
      
    } else if (package=="ph") {
      
      # convert test column to a factor
      uncon_df$Test <- factor(uncon_df$Test, levels=c("translate",
                                                      "phase",
                                                      "whitenoise",
                                                      "biasednoise",
                                                      "outlier"))
      
    }
    
    # create results plots ----
    
    
    # plot all tests in one plot
    # note that the legend is suppressed, but will be added later
    p[[i]] <- ggplot(uncon_df, aes(x = Time, y = Result, group=Test)) +
      geom_point(aes(color=Test), size=2) +
      scale_colour_manual(labels=plot.names, values=cbPalette) +
      ggtitle(names_list[[i]]) +
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
  
  # create a single plot with a legend
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
  
  # Apply user-defined function to extract legend
  shared_legend <- extract_legend(p_legend)
  
  # arrange all plots into one
  p.all <- grid.arrange(do.call(arrangeGrob, c(p, ncol=3)),
                        heights=c(10,1),
                        left=textGrob(paste("Dissimilarity", sep=""),
                                      gp=gpar(fontsize=16),
                                      rot=90),
                        shared_legend,
                        top = textGrob(paste("Uncontrolled Test Results", sep=""),
                                       gp=gpar(fontsize=16)))
  
  # write the grid-arranged plot as a tiff image

  ggsave(filename=paste(dataset_name, 
                        "_", 
                        q_min, 
                        "to", 
                        q_max, 
                        "_incr", 
                        increment,
                        "_",
                        package, 
                        plotname_special,
                        ".tiff", 
                        sep = ""), 
         plot = p.all, 
         device = "tiff",
         path = "plots/realworld/",
         compression = "lzw", 
         dpi = 1000, 
         height = 10000, 
         width = 7500, 
         units = "px")
  
}
