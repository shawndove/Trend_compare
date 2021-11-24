# Function to perform desirable properties tests on distance measures
# and print the raw test results to Word tables and jpg images
# Shawn Dove (s.dove@ucl.ac.uk) - November, 2020

get.dm.result1 <- function(dist.fun, 
                          print_name, 
                          dist.args,
                          tsx.list,
                          tsy.list)
{
  
  ### packages ----
  
  library(ggplot2)
  library(patchwork)
  library(gridExtra)
  library(grid)
  library(flextable)
  library(plyr)
  
  ### load functions ----
  
  source("scripts/test_results_fn.R")
  
  ### prepare variables ----
  
  # create a list of test results with an element for each test
  test.results <- list()
  test.results$"Uniqueness" <- vector()
  test.results$"Symmetry" <- vector()
  test.results$"Translation Sensitivity" <- vector()
  test.results$"Amplitude Sensitivity" <- vector()
  test.results$"Duration Sensitivity" <- vector()
  test.results$"Frequency Sensitivity" <- vector()
  test.results$"White Noise Sensitivity" <- vector()
  test.results$"Biased Noise Sensitivity" <- vector()
  test.results$"Outlier Sensitivity" <- vector()
  test.results$"Antiparallelism Bias" <- vector()
  test.results$"Phase Invariance" <- vector()
  test.results$"Uniform Time Scaling Invariance" <- vector()
  test.results$"Warping Invariance" <- vector()
  test.results$"Non-positive Value Handling" <- vector()
  test.results$"Non-negativity" <- vector()
  test.results$"Triangle Inequality" <- vector()
  test.results$"Relative Sensitivity Ranges" <- vector()

  # create a list to hold plots of some test results
  plot.res <- list()
  plot.res$"Uniqueness" <- vector()
  plot.res$"Symmetry" <- vector()
  plot.res$"Translation Sensitivity" <- vector()
  plot.res$"Amplitude Sensitivity" <- vector()
  plot.res$"Duration Sensitivity" <- vector()
  plot.res$"Frequency Sensitivity" <- vector()
  plot.res$"White Noise Sensitivity" <- vector()
  plot.res$"Biased Noise Sensitivity" <- vector()
  plot.res$"Outlier Sensitivity" <- vector()
  plot.res$"Antiparallelism Bias" <- vector()
  plot.res$"Phase Invariance" <- vector()
  plot.res$"Uniform Time Scaling Invariance" <- vector()
  plot.res$"Warping Invariance" <- vector()
  plot.res$"Non-positive Value Handling" <- vector()
  plot.res$"Non-negativity" <- vector()
  plot.res$"Triangle Inequality" <- vector()
  plot.res$"Relative Sensitivity Ranges" <- vector()
  
  # create lists for ymin and ymax arguments
  ymin <- list()
  ymax <- list()
  
  # create list for plots
  p <- list()
  
  # testing: loop ----
  
  # loop along list of time series for each test
  for (i in 1:length(tsx.list)) {
  
  # call testing function
  temp.list <- test.results.fn(tsx.list[[i]], 
                               tsy.list[[i]], 
                               dist.fun, 
                               dist.args)
  
  # put returned objects into their respective lists
  
  # a vector of test results
  test.results[[i]] <- temp.list[[1]]
  
  # a dataframe for plotting
  plot.res[[i]] <- temp.list[[2]]
  
  # ylim values for ggplot
  ymin[[i]] <- temp.list[[3]]
  ymax[[i]] <- temp.list[[4]]
  
  }
  
  # testing: Non-negativity ----
  
  # call out to another function to get non-negativity and triangle inequality results
  
  temp_nn <- readRDS(file = paste("files/nn_results/", print_name, "_nnfull.RData", sep=""))
  
  test.results$"Non-negativity" <- temp_nn[[2]]
  
  # testing: Triangle inequality ----
  
  # use results from the function call in Non-negativity testing section
  
  temp_ti <- readRDS(file = paste("files/ti_results/", print_name, "_tifull.RData", sep=""))
  
  test.results$"Triangle Inequality" <- temp_ti[[length(temp_ti)-3]]
  
  
  # testing: Relative Sensitivity Ranges ----
  
  # divide range (max - min) of tested sensitivities by mean to get Relative Sensitivity Ranges
  
  test.results$"Relative Sensitivity Ranges"[[1]] <- 
    (max(test.results$"Translation Sensitivity") - min(test.results$"Translation Sensitivity")) / 
    (length(test.results$"Translation Sensitivity") - 1)

    test.results$"Relative Sensitivity Ranges"[[2]] <- 
    (max(test.results$"Amplitude Sensitivity") - min(test.results$"Amplitude Sensitivity")) / 
    (length(test.results$"Amplitude Sensitivity") - 1)
  
  test.results$"Relative Sensitivity Ranges"[[3]] <- 
    (max(test.results$"Duration Sensitivity") - min(test.results$"Duration Sensitivity")) / 
    (length(test.results$"Duration Sensitivity") - 1)
  
  test.results$"Relative Sensitivity Ranges"[[4]] <- 
    (max(test.results$"Frequency Sensitivity") - min(test.results$"Frequency Sensitivity")) / 
    (length(test.results$"Frequency Sensitivity") - 1)
  
  test.results$"Relative Sensitivity Ranges"[[5]] <- 
    (max(test.results$"White Noise Sensitivity") - min(test.results$"White Noise Sensitivity")) / 
    (length(test.results$"White Noise Sensitivity") - 1)
  
  test.results$"Relative Sensitivity Ranges"[[6]] <- 
    (max(test.results$"Biased Noise Sensitivity") - min(test.results$"Biased Noise Sensitivity")) / 
    (length(test.results$"Biased Noise Sensitivity") - 1)
  
  test.results$"Relative Sensitivity Ranges"[[7]] <- 
    (max(test.results$"Outlier Sensitivity") - min(test.results$"Outlier Sensitivity")) / 
    (length(test.results$"Outlier Sensitivity") - 1)

  rsr.mean <- mean(test.results$"Relative Sensitivity Ranges"[test.results$"Relative Sensitivity Ranges" > 0])
  
  for (i in seq_along(test.results$"Relative Sensitivity Ranges"))
  {
    
    test.results$"Relative Sensitivity Ranges"[[i]] <-
      test.results$"Relative Sensitivity Ranges"[[i]] / rsr.mean
    
  }
  
  # if there are no results, replace with NA
  if(length(test.results$"Relative Sensitivity Ranges")==0)
  {
    
    test.results$"Relative Sensitivity Ranges" <- NA
    
  }
  
  # create a data frame of relative sensitivity results for plotting
  
  sens <- seq(1,length(test.results$"Relative Sensitivity Ranges"))  
  
  plot.res$"Relative Sensitivity Ranges" <- data.frame(sens,
                                                  test.results$"Relative Sensitivity Ranges")
  
  colnames(plot.res$"Relative Sensitivity Ranges") <- c("x", "y")
  
  # this code sets the ylim values that will be used for ggplot later on
  
  # if all test results are NA, set ymin and ymax to NA so no plot will be made
  if (all(is.na(test.results$"Relative Sensitivity Ranges"))) 
  {
    
    ymin.range <- NA
    
    ymax.range <- NA
    
    # if all test results are NaN, set ymin and ymax to NA so no plot will be made
  } else if (all(is.nan(test.results$"Relative Sensitivity Ranges")))
  {
    
    ymin.range <- NA
    
    ymax.range <- NA
    
    # if max result is very small, set ymin and ymax very small 
  } else if (max(abs(test.results$"Relative Sensitivity Ranges"), na.rm=TRUE) < 0.01) 
  {
    
    ymin.range <- -0.01
    
    ymax.range <- 0.01
    
    # otherwise set ymin and ymax to slightly below and above (respectively) 
    # the range of test results
  } else 
  {
    
    ymin.range <- min(test.results$"Relative Sensitivity Ranges", na.rm=TRUE) - 
      0.1*min(abs(test.results$"Relative Sensitivity Ranges"), na.rm=TRUE)
    
    ymax.range <- max(test.results$"Relative Sensitivity Ranges", na.rm=TRUE) + 
      0.1*max(abs(test.results$"Relative Sensitivity Ranges"), na.rm=TRUE)
    
  }  
  
  # create results table ----

  # turn results list into a long-form data frame
  tr_dataframe <- ldply(test.results, 
                        .fun = data.frame)
  
  # name columns
  colnames(tr_dataframe) <- c("Test", 
                              "Res")
  
  # add a column to define row-column structure for conversion to wide-form
  tr_dataframe$Time <- c(seq(1, length(test.results$"Uniqueness")), 
                         seq(1, length(test.results$"Symmetry")),
                         seq(1, length(test.results$"Translation Sensitivity")),
                         seq(1, length(test.results$"Amplitude Sensitivity")), 
                         seq(1, length(test.results$"Duration Sensitivity")),
                         seq(1, length(test.results$"Frequency Sensitivity")),
                         seq(1, length(test.results$"White Noise Sensitivity")),
                         seq(1, length(test.results$"Biased Noise Sensitivity")),
                         seq(1, length(test.results$"Outlier Sensitivity")),
                         seq(1, length(test.results$"Antiparallelism Bias")),
                         seq(1, length(test.results$"Phase Invariance")),
                         seq(1, length(test.results$"Uniform Time Scaling Invariance")),
                         seq(1, length(test.results$"Warping Invariance")),
                         seq(1, length(test.results$"Non-positive Value Handling")),
                         seq(1, length(test.results$"Non-negativity")),
                         seq(1, length(test.results$"Triangle Inequality")),
                         seq(1, length(test.results$"Relative Sensitivity Ranges")))
  
  # convert to wide-form data frame with one row for each desirable property
  tr_wide <- reshape(tr_dataframe, 
                     timevar = "Time", 
                     idvar = "Test", 
                     direction = "wide")
  
  # create a flextable from the wide-form data frame
  tr_table <- flextable(tr_wide)
  
  # change the number of decimal places to display based on the size of the largest
  # value in the results. this ensures results will be displayed as precisely as
  # possible without the table becoming too wide
  if (max(tr_wide[,2:length(tr_wide)], na.rm=TRUE) < 10) 
  {
    
    tr_table <- colformat_num(tr_table, big.mark=",", digits = 4, na_str = "")
    
  } else if (max(tr_wide[,2:length(tr_wide)], na.rm=TRUE) < 100) 
  {
    
    tr_table <- colformat_num(tr_table, big.mark=",", digits = 3, na_str = "")
    
  } else if (max(tr_wide[,2:length(tr_wide)], na.rm=TRUE) < 1000) 
  {
    
    tr_table <- colformat_num(tr_table, big.mark=",", digits = 2, na_str = "")
    
    # number of decimal places displayed will never be less than 2
  } else 
  {
    
    tr_table <- colformat_num(tr_table, big.mark=",", digits = 2, na_str = "")
    
  }
    
  # this line adds a blank prefix and suffix to fix an issue where the first column 
  # of the table, containing the names of the desirable properties, was not left-justified
  # when exported to Word, although it displayed correctly as a flextable within R.
  tr_table <- colformat_char(tr_table, j=1, prefix="",suffix="")
  
  # minimize the cell padding to reduce table width
  tr_table <- padding(tr_table, padding.left = 0.1, padding.right = 0.1, part = "all")
  
  # set font size larger for results values if none are greater than 1000
  # and smaller if any values are greater than 1000. this allows retention of 2 
  # decimal places without making the table too wide when some results values are very large
  if (max(tr_wide[,2:length(tr_wide)], na.rm=TRUE) < 1000) 
  {
  
    tr_table <- fontsize(tr_table, size = 9, part = "body")
    
  } else 
  {
    
    tr_table <- fontsize(tr_table, size = 7, part = "body")
    
  }
  
  # set font size for column headers
  tr_table <- fontsize(tr_table, size = 10, part = "header")
  
  # center column headers
  tr_table <- align(tr_table, align="center", part="header")
  
  # left-justify the body of the table
  tr_table <- align(tr_table, align="left", part="body")
  
  # add a title for the table, including abbreviated name of distance measure
  tr_table <- add_header_row(tr_table, 
                             top=TRUE, 
                             values=paste(print_name, 
                                          "Raw Test Results", 
                                          separator=""),
                             colwidths=ncol(tr_wide))
  
  tr_table <- autofit(tr_table)
  
  # create results plots ----

  for (i in 1:length(tsx.list)) {
    
    # check for NA in ymin value
    # if NA, then plot will be blank
    if (!is.na(ymin[[i]])) 
    {
      
      # save ggplot of Translation sensitivity
      p[[i]] <- ggplot(plot.res[[i]],
                   aes(x = x, 
                       y = y)) +
        geom_point(size=2) +
        ylim(ymin[[i]], ymax[[i]]) +
        theme(legend.title = element_text(color="blue")) +
        ggtitle(names(plot.res[i])) +
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
  
  # choose plots to arrange together into single plot
  p_plot <- list(p[[3]], 
                 p[[4]], 
                 p[[5]], 
                 p[[6]], 
                 p[[7]], 
                 p[[8]], 
                 p[[9]], 
                 p[[10]],
                 p[[11]],
                 p[[12]],
                 p[[13]])
 
  # arrange chosen plots into one
  p.all <- grid.arrange(do.call(arrangeGrob, c(p_plot, ncol=3)),
                                 top = textGrob(paste(print_name, 
                                                      " Test Results", 
                                                      sep=""),
                                       gp=gpar(fontsize=16,
                                               font=4)))

  # write results to files ----

  # write the grid-arranged plot as a jpg image
  ggsave(filename = paste(print_name, 
                          "_plots.tiff", 
                          sep=""),
         path = "plots/controlled_results/",
         plot = p.all,
         device = "tiff",
         width = 8000,
         height = 8000,
         units = "px",
         dpi = 1000,
         compression = "lzw")
  
  # write the flextable as a Word document
  save_as_docx(tr_table, 
               path = paste("tables/controlled_results/", 
                            print_name, "_table.docx", 
                            sep=""))
  
  # write the wide-form data table to an R file
  saveRDS(tr_wide, 
          file = paste("files/controlled_results/", 
                       print_name, 
                       "_dfwide.RData", 
                       sep=""))
  
  # return the flextable object
  return(tr_table)
  
}
