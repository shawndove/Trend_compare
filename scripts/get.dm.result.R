# Function to perform desirable properties tests on distance measures
# and print the raw test results to Word tables and jpg images
# Shawn Dove (s.dove@ucl.ac.uk) - November, 2020

get.dm.result <- function(dist.fun, 
                          print_name, 
                          dist.args)
{
  
  # packages ----
  
  library(ggplot2)
  library(patchwork)
  library(gridExtra)
  library(grid)
  library(flextable)
  library(plyr)
  
  # prepare variables ----
  
  # create a list of test results with an element for each test
  test.results <- list()
  test.results$"Offset Sensitivity" <- vector()
  test.results$"Uniqueness" <- vector()
  test.results$"Symmetry" <- vector()
  test.results$"Temporal Scale Sensitivity" <- vector()
  test.results$"Order Dependence" <- vector()
  test.results$"Amplitude of Deviation" <- vector()
  test.results$"Relative Duration of Deviation" <- vector()
  test.results$"Direction of Deviation" <- vector()
  test.results$"Uniform Time Scaling Sensitivity" <- vector()
  test.results$"Scale Sensitivity" <- vector()
  
  # create a list to hold plots of some test results
  plot.res <- list()
  
  # testing: offset sensitivity ----
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Offset Sensitivity
    for (i in seq_along(tslistx.off)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Offset Sensitivity"[[i]] <- dist.fun(tslistx.off[[i]], 
                                                        tslisty.off[[i]])
    }
    
  } else 
  {
      
      # loop to test each distance measure for Offset Sensitivity
      for (i in seq_along(tslistx.off)) 
      {
        
        argslist.off <- list()
       
        argslist.off[[1]] <- tslistx.off[[i]]
       
        argslist.off[[2]] <- tslisty.off[[i]]
        
        for (j in 1:length(dist.args)) 
        {
          
          argslist.off[2+j] <- dist.args[j]
          
        }
        
        # use this line if there are extra arguments
        test.results$"Offset Sensitivity"[[i]] <- do.call(dist.fun, argslist.off)
        
    }
      
  }
  
  # create a data frame to hold test results in a format compatible with ggplot
  plot.res$"Offset Sensitivity" <- data.frame(seq_along(tslistx.off),
                                              test.results$"Offset Sensitivity")

  # name the columns
  colnames(plot.res$"Offset Sensitivity") <- c("x", "y")
  
  # check for NAs in test results
  if (is.na(max(test.results$"Offset Sensitivity"))) 
  {
    
    # set ymin and ymax to NA. ggplot will be told to avoid plotting these results
    ymin.off <- NA
    
    ymax.off <- NA
    
    # if the values returned are very small, set y axis from -0.01 to 0.01
    # this ensures that very small values will be clearly shown as near zero
    
  } else if (max(abs(test.results$"Offset Sensitivity")) < 0.01) 
  {
    
    ymin.off <- -0.01
    
    ymax.off <- 0.01
    
    # if the values are not very small, set y axis according to the data
    
  } else 
  {
    
    ymin.off <- min(test.results$"Offset Sensitivity") - 0.1*min(abs(test.results$"Offset Sensitivity"))
    
    ymax.off <- max(test.results$"Offset Sensitivity") + 0.1*max(abs(test.results$"Offset Sensitivity"))
    
  }

  # testing: uniqueness ----
  
  # see offset sensitivity section for specific comments
  # uniqueness is not plotted, so no need to check for NAs or adjust axes

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args))
  {
      
    # loop to test each distance measure for Uniqueness
    for (i in seq_along(tslistx.unq))
    {
        
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Uniqueness"[[i]] <- dist.fun(tslistx.unq[[i]], 
                                                 tslisty.unq[[i]])
        
    }
      
  } else 
  {
      
      # loop to test each distance measure for Uniqueness
    for (i in seq_along(tslistx.unq)) 
    {
        
      argslist.unq <- list()
        
      argslist.unq[[1]] <- tslistx.unq[[i]]
        
      argslist.unq[[2]] <- tslisty.unq[[i]]
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.unq[2+j] <- dist.args[j]
          
      }
        
        # use this line if there are extra arguments
      test.results$"Uniqueness"[[i]] <- do.call(dist.fun, argslist.unq)
        
    }
      
  }
    

  plot.res$"Uniqueness" <- data.frame(seq_along(tslistx.unq),
                                      test.results$"Uniqueness")
  
  colnames(plot.res$"Uniqueness") <- c("x", "y")
  
  # testing: symmetry ----

  # see offset sensitivity for specific comments
  # symmetry is not plotted, so no need to check for NAs or adjust axes

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
      
    # loop to test each distance measure for Symmetry
    for (i in seq_along(tslistx.sym)) 
    {
        
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Symmetry"[[i]] <- dist.fun(tslistx.sym[[i]], 
                                               tslisty.sym[[i]])
      
    }
      
  } else 
  {
      
    # loop to test each distance measure for Symmetry
    for (i in seq_along(tslistx.sym)) 
    {
        
      argslist.sym <- list()
        
      argslist.sym[[1]] <- tslistx.sym[[i]]
        
      argslist.sym[[2]] <- tslisty.sym[[i]]
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.sym[2+j] <- dist.args[j]
          
      }
        
      # use this line if there are extra arguments
      test.results$"Symmetry"[[i]] <- do.call(dist.fun, argslist.sym)
        
    }
      
  }
    
  plot.res$"Symmetry" <- data.frame(seq_along(tslistx.sym),
                                    test.results$"Symmetry")
  
  colnames(plot.res$"Symmetry") <- c("x", "y")
  
  # testing: temporal scale sensitivity ----

  # see offset sensitivity for specific comments

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
      
    # loop to test each distance measure for Temporal Scale Sensitivity
    for (i in seq_along(tslistx.tscale)) 
    {
        
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Temporal Scale Sensitivity"[[i]] <- dist.fun(tslistx.tscale[[i]], 
                                                                 tslisty.tscale[[i]])
    }
      
  } else 
  {
      
    # loop to test each distance measure for Temporal Scale Sensitivity
    for (i in seq_along(tslistx.tscale)) 
    {
        
      argslist.tscale <- list()
        
      argslist.tscale[[1]] <- tslistx.tscale[[i]]
        
      argslist.tscale[[2]] <- tslisty.tscale[[i]]
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.tscale[2+j] <- dist.args[j]
          
      }
        
      # use this line if there are extra arguments
      test.results$"Temporal Scale Sensitivity"[[i]] <- do.call(dist.fun, argslist.tscale)
        
    }
      
  }
    
  plot.res$"Temporal Scale Sensitivity" <- data.frame(seq_along(tslistx.tscale),
                                                      test.results$"Temporal Scale Sensitivity")
  
  colnames(plot.res$"Temporal Scale Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Temporal Scale Sensitivity"))) 
  {
    
    ymin.tscale <- NA
    
    ymax.tscale <- NA
    
  } else if (max(abs(test.results$"Temporal Scale Sensitivity")) < 0.01) 
  {
    
    ymin.tscale <- -0.01
    
    ymax.tscale <- 0.01
    
  } else 
  {
    
    ymin.tscale <- min(test.results$"Temporal Scale Sensitivity") - 0.1*min(abs(test.results$"Temporal Scale Sensitivity"))
    
    ymax.tscale <- max(test.results$"Temporal Scale Sensitivity") + 0.1*max(abs(test.results$"Temporal Scale Sensitivity"))
    
  } 
  
  # testing: order dependence ----
  
  # see offset sensitivity for specific comments
  # order dependence is not plotted, so no need to check for NAs or adjust axes

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
      
    # loop to test each distance measure for Order Dependence
    for (i in seq_along(tslistx.ord)) 
    {
        
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Order Dependence"[[i]] <- dist.fun(tslistx.ord[[i]], 
                                                       tslisty.ord[[i]])
    }
      
  } else 
  {
      
    # loop to test each distance measure for Order Dependence
    for (i in seq_along(tslistx.ord)) 
    {
        
      argslist.ord <- list()
        
      argslist.ord[[1]] <- tslistx.ord[[i]]
        
      argslist.ord[[2]] <- tslisty.ord[[i]]
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.ord[2+j] <- dist.args[j]
          
      }
        
      # use this line if there are extra arguments
      test.results$"Order Dependence"[[i]] <- do.call(dist.fun, argslist.ord)
        
    }
      
  }
    
  plot.res$"Order Dependence" <- data.frame(seq_along(tslistx.ord),
                                            test.results$"Order Dependence")
  
  colnames(plot.res$"Order Dependence") <- c("x", "y")
  
  # testing: amplitude of deviation ----
  
  # see offset sensitivity for specific comments

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
      
    # loop to test each distance measure for Amplitude of Deviation
    for (i in seq_along(tslistx.amp)) 
    {
        
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Amplitude of Deviation"[[i]] <- dist.fun(tslistx.amp[[i]], 
                                                             tslisty.amp[[i]])
      
    }
      
  } else 
  {
      
    # loop to test each distance measure for Amplitude of Deviation
    for (i in seq_along(tslistx.amp)) 
    {
        
      argslist.amp <- list()
        
      argslist.amp[[1]] <- tslistx.amp[[i]]
        
      argslist.amp[[2]] <- tslisty.amp[[i]]
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.amp[2+j] <- dist.args[j]
          
      }
        
      # use this line if there are extra arguments
      test.results$"Amplitude of Deviation"[[i]] <- do.call(dist.fun, argslist.amp)
        
    }
      
  }
  
  plot.res$"Amplitude of Deviation" <- data.frame(seq_along(tslistx.amp),
                                                  test.results$"Amplitude of Deviation")
  
  colnames(plot.res$"Amplitude of Deviation") <- c("x", "y")
  
  if (is.na(max(test.results$"Amplitude of Deviation"))) 
  {
    
    ymin.amp <- NA
    
    ymax.amp <- NA
    
  } else if (max(abs(test.results$"Amplitude of Deviation")) < 0.01) 
  {
    
    ymin.amp <- -0.01
    
    ymax.amp <- 0.01
    
  } else 
  {
    
    ymin.amp <- min(test.results$"Amplitude of Deviation") - 0.1*min(abs(test.results$"Amplitude of Deviation"))
    
    ymax.amp <- max(test.results$"Amplitude of Deviation") + 0.1*max(abs(test.results$"Amplitude of Deviation"))
    
  }  
  
  # testing: relative duration of deviation ----
  
  # see offset sensitivity for specific comments

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
      
    # loop to test each distance measure for Relative Duration of Deviation
    for (i in seq_along(tslistx.dur)) 
    {
        
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Relative Duration of Deviation"[[i]] <- dist.fun(tslistx.dur[[i]], 
                                                                     tslisty.dur[[i]])
    }
      
  } else 
  {
      
    # loop to test each distance measure for Relative Duration of Deviation
    for (i in seq_along(tslistx.dur)) 
    {
        
      argslist.dur <- list()
        
      argslist.dur[[1]] <- tslistx.dur[[i]]
        
      argslist.dur[[2]] <- tslisty.dur[[i]]
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.dur[2+j] <- dist.args[j]
          
      }
        
        # use this line if there are extra arguments
      test.results$"Relative Duration of Deviation"[[i]] <- do.call(dist.fun, argslist.dur)
        
    }
      
  }
  
  plot.res$"Relative Duration of Deviation" <- data.frame(seq_along(tslistx.dur),
                                                          test.results$"Relative Duration of Deviation")
  
  colnames(plot.res$"Relative Duration of Deviation") <- c("x", "y")
  
  if (is.na(max(test.results$"Relative Duration of Deviation"))) 
  {
    
    ymin.dur <- NA
    
    ymax.dur <- NA
    
  } else if (max(abs(test.results$"Relative Duration of Deviation")) < 0.01) 
  {
    
    ymin.dur <- -0.01
    
    ymax.dur <- 0.01
    
  } else 
  {
    
    ymin.dur <- min(test.results$"Relative Duration of Deviation") - 0.1*min(abs(test.results$"Relative Duration of Deviation"))
    
    ymax.dur <- max(test.results$"Relative Duration of Deviation") + 0.1*max(abs(test.results$"Relative Duration of Deviation"))
    
  }  
  
  # testing: direction of deviation ----
  
  # see offset sensitivity for specific comments

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
      
    # loop to test each distance measure for Direction of Deviation
    for (i in seq_along(tslistx.dir)) 
    {
        
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Direction of Deviation"[[i]] <- dist.fun(tslistx.dir[[i]], 
                                                             tslisty.dir[[i]])
    }
      
  } else 
  {
      
    # loop to test each distance measure for Direction of Deviation
    for (i in seq_along(tslistx.dir)) 
    {
        
      argslist.dir <- list()
        
      argslist.dir[[1]] <- tslistx.dir[[i]]
        
      argslist.dir[[2]] <- tslisty.dir[[i]]
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.dir[2+j] <- dist.args[j]
          
      }
        
      # use this line if there are extra arguments
      test.results$"Direction of Deviation"[[i]] <- do.call(dist.fun, argslist.dir)
        
    }
      
  }
  
  plot.res$"Direction of Deviation" <- data.frame(seq_along(tslistx.dir),
                                                  test.results$"Direction of Deviation")
  
  colnames(plot.res$"Direction of Deviation") <- c("x", "y")
  
  if (is.na(max(test.results$"Direction of Deviation"))) 
  {
    
    ymin.dir <- NA
    
    ymax.dir <- NA
    
  } else if (max(abs(test.results$"Direction of Deviation")) < 0.01) 
  {
    
    ymin.dir <- -0.01
    
    ymax.dir <- 0.01
    
  } else 
  {
    
    ymin.dir <- min(test.results$"Direction of Deviation") - 0.1*min(abs(test.results$"Direction of Deviation"))
    
    ymax.dir <- max(test.results$"Direction of Deviation") + 0.1*max(abs(test.results$"Direction of Deviation"))
    
  }
  
  # testing: uniform time scaling sensitivity ----
  
  # see offset sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Uniform Time Scaling Sensitivity
    for (i in seq_along(tslistx.utinv)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Uniform Time Scaling Sensitivity"[[i]] <- dist.fun(tslistx.utinv[[i]], 
                                                                       tslisty.utinv[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Amplitude of Deviation
    for (i in seq_along(tslistx.utinv)) 
    {
      
      argslist.utinv <- list()
      
      argslist.utinv[[1]] <- tslistx.utinv[[i]]
      
      argslist.utinv[[2]] <- tslisty.utinv[[i]]
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.utinv[2+j] <- dist.args[j]
        
      }
      
      # use this line if there are extra arguments
      test.results$"Uniform Time Scaling Sensitivity"[[i]] <- do.call(dist.fun, argslist.utinv)
      
    }
    
  }
  
  plot.res$"Uniform Time Scaling Sensitivity" <- data.frame(seq_along(tslistx.utinv),
                                                  test.results$"Uniform Time Scaling Sensitivity")
  
  colnames(plot.res$"Uniform Time Scaling Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Uniform Time Scaling Sensitivity"))) 
  {
    
    ymin.utinv <- NA
    
    ymax.utinv <- NA
    
  } else if (max(abs(test.results$"Uniform Time Scaling Sensitivity")) < 0.01) 
  {
    
    ymin.utinv <- -0.01
    
    ymax.utinv <- 0.01
    
  } else 
  {
    
    ymin.utinv <- min(test.results$"Uniform Time Scaling Sensitivity") - 0.1*min(abs(test.results$"Uniform Time Scaling Sensitivity"))
    
    ymax.utinv <- max(test.results$"Uniform Time Scaling Sensitivity") + 0.1*max(abs(test.results$"Uniform Time Scaling Sensitivity"))
    
  }
  
  # testing: scale sensitivity ----
  
  # see offset sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Amplitude of Deviation
    for (i in seq_along(tslistx.scl)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Scale Sensitivity"[[i]] <- dist.fun(tslistx.scl[[i]], 
                                                             tslisty.scl[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Amplitude of Deviation
    for (i in seq_along(tslistx.scl)) 
    {
      
      argslist.scl <- list()
      
      argslist.scl[[1]] <- tslistx.scl[[i]]
      
      argslist.scl[[2]] <- tslisty.scl[[i]]
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.scl[2+j] <- dist.args[j]
        
      }
      
      # use this line if there are extra arguments
      test.results$"Scale Sensitivity"[[i]] <- do.call(dist.fun, argslist.scl)
      
    }
    
  }
  
  plot.res$"Scale Sensitivity" <- data.frame(seq_along(tslistx.scl),
                                                  test.results$"Scale Sensitivity")
  
  colnames(plot.res$"Scale Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Scale Sensitivity"))) 
  {
    
    ymin.scl <- NA
    
    ymax.scl <- NA
    
  } else if (max(abs(test.results$"Scale Sensitivity")) < 0.01) 
  {
    
    ymin.scl <- -0.01
    
    ymax.scl <- 0.01
    
  } else 
  {
    
    ymin.scl <- min(test.results$"Scale Sensitivity") - 0.1*min(abs(test.results$"Scale Sensitivity"))
    
    ymax.scl <- max(test.results$"Scale Sensitivity") + 0.1*max(abs(test.results$"Scale Sensitivity"))
    
  }  
  
  # create results table ----

  # turn results list into a long-form data frame
  tr_dataframe <- ldply(test.results, 
                        .fun = data.frame)
  
  # name columns
  colnames(tr_dataframe) <- c("Test", 
                              "Res")
  
  # add a column to define row-column structure for conversion to wide-form
  tr_dataframe$Time <- c(seq(1, length(tslistx.off)), 
                         seq(1, length(tslistx.unq)), 
                         seq(1, length(tslistx.sym)), 
                         seq(1, length(tslistx.tscale)), 
                         seq(1, length(tslistx.ord)), 
                         seq(1, length(tslistx.amp)), 
                         seq(1, length(tslistx.dur)), 
                         seq(1, length(tslistx.dir)),
                         seq(1, length(tslistx.utinv)),
                         seq(1, length(tslistx.scl)))
  
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

  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.off)) 
  {
    
    # save ggplot of offset sensitivity
    p1 <- ggplot(plot.res$"Offset Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.off, ymax.off) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Offset Sensitivity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p1 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.tscale)) 
  {
 
    # save ggplot of temporal scale sensitivity
    p2 <- ggplot(plot.res$"Temporal Scale Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.tscale, ymax.tscale) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Temporal Scale Sensitivity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p2 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.amp)) 
  {
    
    # save ggplot of amplitude of deviation
    p3 <- ggplot(plot.res$"Amplitude of Deviation",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.amp, ymax.amp) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Amplitude of Deviation") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p3 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.dur)) 
  {

    # save ggplot of relative duration of deviation
    p4 <- ggplot(plot.res$"Relative Duration of Deviation",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.dur, ymax.dur) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Relative Duration of Deviation") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    # save blank plot
    p4 <- ggplot() +
          theme_void()
    
  }

  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.utinv)) 
  {
    
    # save ggplot of amplitude of deviation
    p5 <- ggplot(plot.res$"Uniform Time Scaling Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.utinv, ymax.utinv) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Uniform Time Scaling Sensitivity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p5 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.scl)) 
  {
    
    # save ggplot of amplitude of deviation
    p6 <- ggplot(plot.res$"Scale Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.scl, ymax.scl) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Scale Sensitivity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p6 <- ggplot() +
      theme_void()
    
  }
  
  # arrange the 4 saved plots into one
  p.all <- grid.arrange(p1,
                        p2,
                        p3,
                        p4,
                        p5,
                        p6,
                        ncol=3, 
                        top = textGrob(paste(print_name, " Test Results", sep=""), 
                                       gp=gpar(fontsize=16,
                                               font=4)))
  
  # write results to files ----
  
  # save working directory path to a variable
  wd <- getwd()
  
  # write the grid-arranged plot as a jpg image
  ggsave(paste(wd, "/plots/test_results/", print_name, "_plots.jpg", sep=""), p.all)
  
  # write the flextable as a Word document
  save_as_docx(tr_table, 
               path = paste(wd, "/tables/test_results/", print_name, "_table.docx", sep=""))
  
  # send the flextable object to the main R environment  
  return(tr_table)
  
}
