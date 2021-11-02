# Function to perform desirable properties tests on distance measures
# and print the raw test results to Word tables and jpg images
# Shawn Dove (s.dove@ucl.ac.uk) - November, 2020

get.dm.result1 <- function(dist.fun, 
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
  test.results$"Uniqueness" <- vector()
  test.results$"Symmetry" <- vector()
  test.results$"Translation Sensitivity" <- vector()
  test.results$"Amplitude Sensitivity" <- vector()
  test.results$"Duration Sensitivity" <- vector()
  test.results$"Frequency Sensitivity" <- vector()
  test.results$"Scale Sensitivity" <- vector()
  test.results$"Noise Sensitivity" <- vector()
  test.results$"White Noise Sensitivity" <- vector()
  test.results$"Biased Noise Sensitivity" <- vector()
  test.results$"Outlier Sensitivity" <- vector()
  test.results$"Point Order Dependence" <- vector()
  test.results$"Antiparallelism Bias" <- vector()
  test.results$"Phase Invariance" <- vector()
  test.results$"Uniform Time Scaling Invariance" <- vector()
  test.results$"Warping Invariance" <- vector()
  test.results$"Shuffling Invariance" <- vector()
  test.results$"Relative Sensitivity Ranges" <- vector()


  # create a list to hold plots of some test results
  plot.res <- list()
  
  # testing: Uniqueness ----
  
  # see Translation sensitivity section for specific comments
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
      
      names(argslist.unq)[1] <- "x"
      
      names(argslist.unq)[2] <- "y"
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.unq[2+j] <- dist.args[j]
        
        names(argslist.unq)[2+j] <- names(dist.args[j])
          
      }
        
        # use this line if there are extra arguments
      test.results$"Uniqueness"[[i]] <- do.call(dist.fun, argslist.unq)
        
    }
      
  }
    

  plot.res$"Uniqueness" <- data.frame(seq_along(tslistx.unq),
                                      test.results$"Uniqueness")
  
  colnames(plot.res$"Uniqueness") <- c("x", "y")
  
  # testing: Symmetry ----

  # see Translation sensitivity for specific comments
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
      
      names(argslist.sym)[1] <- "x"
      
      names(argslist.sym)[2] <- "y"
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.sym[2+j] <- dist.args[j]
        
        names(argslist.sym)[2+j] <- names(dist.args[j])
          
      }
        
      # use this line if there are extra arguments
      test.results$"Symmetry"[[i]] <- do.call(dist.fun, argslist.sym)
        
    }
      
  }
    
  plot.res$"Symmetry" <- data.frame(seq_along(tslistx.sym),
                                    test.results$"Symmetry")
  
  colnames(plot.res$"Symmetry") <- c("x", "y")
  
  # testing: Translation Sensitivity ----
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Translation Sensitivity
    for (i in seq_along(tslistx.trans)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Translation Sensitivity"[[i]] <- dist.fun(tslistx.trans[[i]], 
                                                              tslisty.trans[[i]])
    }
    
  } else 
  {
    
    # loop to test each distance measure for Translation Sensitivity
    for (i in seq_along(tslistx.trans)) 
    {
      
      argslist.trans <- list()
      
      argslist.trans[[1]] <- tslistx.trans[[i]]
      
      argslist.trans[[2]] <- tslisty.trans[[i]]
      
      names(argslist.trans)[1] <- "x"
      
      names(argslist.trans)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.trans[2+j] <- dist.args[j]
        
        names(argslist.trans)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Translation Sensitivity"[[i]] <- do.call(dist.fun, argslist.trans)
      
    }
    
  }
  
  # create a data frame to hold test results in a format compatible with ggplot
  plot.res$"Translation Sensitivity" <- data.frame(seq_along(tslistx.trans),
                                                   test.results$"Translation Sensitivity")
  
  # name the columns
  colnames(plot.res$"Translation Sensitivity") <- c("x", "y")
  
  # check for NAs in test results
  if (is.na(max(test.results$"Translation Sensitivity"))) 
  {
    
    # set ymin and ymax to NA. ggplot will be told to avoid plotting these results
    ymin.trans <- NA
    
    ymax.trans <- NA
    
    # if the values returned are very small, set y axis from -0.01 to 0.01
    # this ensures that very small values will be clearly shown as near zero
    
  } else if (max(abs(test.results$"Translation Sensitivity")) < 0.01) 
  {
    
    ymin.trans <- -0.01
    
    ymax.trans <- 0.01
    
    # if the values are not very small, set y axis according to the data
    
  } else 
  {
    
    ymin.trans <- min(test.results$"Translation Sensitivity") - 0.1*min(abs(test.results$"Translation Sensitivity"))
    
    ymax.trans <- max(test.results$"Translation Sensitivity") + 0.1*max(abs(test.results$"Translation Sensitivity"))
    
  }
  
  # testing: Scale Sensitivity ----
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Scale Sensitivity
    for (i in seq_along(tslistx.scale)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Scale Sensitivity"[[i]] <- dist.fun(tslistx.scale[[i]], 
                                                              tslisty.scale[[i]])
    }
    
  } else 
  {
    
    # loop to test each distance measure for Scale Sensitivity
    for (i in seq_along(tslistx.scale)) 
    {
      
      argslist.scale <- list()
      
      argslist.scale[[1]] <- tslistx.scale[[i]]
      
      argslist.scale[[2]] <- tslisty.scale[[i]]
      
      names(argslist.scale)[1] <- "x"
      
      names(argslist.scale)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.scale[2+j] <- dist.args[j]
        
        names(argslist.scale)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Scale Sensitivity"[[i]] <- do.call(dist.fun, argslist.scale)
      
    }
    
  }
  
  # create a data frame to hold test results in a format compatible with ggplot
  plot.res$"Scale Sensitivity" <- data.frame(seq_along(tslistx.scale),
                                                   test.results$"Scale Sensitivity")
  
  # name the columns
  colnames(plot.res$"Scale Sensitivity") <- c("x", "y")
  
  # check for NAs in test results
  if (is.na(max(test.results$"Scale Sensitivity"))) 
  {
    
    # set ymin and ymax to NA. ggplot will be told to avoid plotting these results
    ymin.scale <- NA
    
    ymax.scale <- NA
    
    # if the values returned are very small, set y axis from -0.01 to 0.01
    # this ensures that very small values will be clearly shown as near zero
    
  } else if (max(abs(test.results$"Scale Sensitivity")) < 0.01) 
  {
    
    ymin.scale <- -0.01
    
    ymax.scale <- 0.01
    
    # if the values are not very small, set y axis according to the data
    
  } else 
  {
    
    ymin.scale <- min(test.results$"Scale Sensitivity") - 0.1*min(abs(test.results$"Scale Sensitivity"))
    
    ymax.scale <- max(test.results$"Scale Sensitivity") + 0.1*max(abs(test.results$"Scale Sensitivity"))
    
  }
  
  # testing: Amplitude Sensitivity ----
  
  # see Translation Sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Amplitude Sensitivity
    for (i in seq_along(tslistx.amp)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Amplitude Sensitivity"[[i]] <- dist.fun(tslistx.amp[[i]], 
                                                            tslisty.amp[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Amplitude Sensitivity
    for (i in seq_along(tslistx.amp)) 
    {
      
      argslist.amp <- list()
      
      argslist.amp[[1]] <- tslistx.amp[[i]]
      
      argslist.amp[[2]] <- tslisty.amp[[i]]
      
      names(argslist.amp)[1] <- "x"
      
      names(argslist.amp)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.amp[2+j] <- dist.args[j]
        
        names(argslist.amp)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Amplitude Sensitivity"[[i]] <- do.call(dist.fun, argslist.amp)
      
    }
    
  }
  
  plot.res$"Amplitude Sensitivity" <- data.frame(seq_along(tslistx.amp),
                                                 test.results$"Amplitude Sensitivity")
  
  colnames(plot.res$"Amplitude Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Amplitude Sensitivity"))) 
  {
    
    ymin.amp <- NA
    
    ymax.amp <- NA
    
  } else if (max(abs(test.results$"Amplitude Sensitivity")) < 0.01) 
  {
    
    ymin.amp <- -0.01
    
    ymax.amp <- 0.01
    
  } else 
  {
    
    ymin.amp <- min(test.results$"Amplitude Sensitivity") - 0.1*min(abs(test.results$"Amplitude Sensitivity"))
    
    ymax.amp <- max(test.results$"Amplitude Sensitivity") + 0.1*max(abs(test.results$"Amplitude Sensitivity"))
    
  }  
  
  # testing: Duration Sensitivity ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Duration Sensitivity
    for (i in seq_along(tslistx.dur)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Duration Sensitivity"[[i]] <- dist.fun(tslistx.dur[[i]], 
                                                           tslisty.dur[[i]])
    }
    
  } else 
  {
    
    # loop to test each distance measure for Duration Sensitivity
    for (i in seq_along(tslistx.dur)) 
    {
      
      argslist.dur <- list()
      
      argslist.dur[[1]] <- tslistx.dur[[i]]
      
      argslist.dur[[2]] <- tslisty.dur[[i]]
      
      names(argslist.dur)[1] <- "x"
      
      names(argslist.dur)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.dur[2+j] <- dist.args[j]
        
        names(argslist.dur)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Duration Sensitivity"[[i]] <- do.call(dist.fun, argslist.dur)
      
    }
    
  }
  
  plot.res$"Duration Sensitivity" <- data.frame(seq_along(tslistx.dur),
                                                test.results$"Duration Sensitivity")
  
  colnames(plot.res$"Duration Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Duration Sensitivity"))) 
  {
    
    ymin.dur <- NA
    
    ymax.dur <- NA
    
  } else if (max(abs(test.results$"Duration Sensitivity")) < 0.01) 
  {
    
    ymin.dur <- -0.01
    
    ymax.dur <- 0.01
    
  } else 
  {
    
    ymin.dur <- min(test.results$"Duration Sensitivity") - 0.1*min(abs(test.results$"Duration Sensitivity"))
    
    ymax.dur <- max(test.results$"Duration Sensitivity") + 0.1*max(abs(test.results$"Duration Sensitivity"))
    
  }  
  
  # testing: Frequency Sensitivity ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Frequency Sensitivity
    for (i in seq_along(tslistx.freq)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Frequency Sensitivity"[[i]] <- dist.fun(tslistx.freq[[i]], 
                                                            tslisty.freq[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Frequency Sensitivity
    for (i in seq_along(tslistx.freq)) 
    {
      
      argslist.freq <- list()
      
      argslist.freq[[1]] <- tslistx.freq[[i]]
      
      argslist.freq[[2]] <- tslisty.freq[[i]]
      
      names(argslist.freq)[1] <- "x"
      
      names(argslist.freq)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.freq[2+j] <- dist.args[j]
        
        names(argslist.freq)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Frequency Sensitivity"[[i]] <- do.call(dist.fun, argslist.freq)
      
    }
    
  }
  
  plot.res$"Frequency Sensitivity" <- data.frame(seq_along(tslistx.freq),
                                                 test.results$"Frequency Sensitivity")
  
  colnames(plot.res$"Frequency Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Frequency Sensitivity"))) 
  {
    
    ymin.freq <- NA
    
    ymax.freq <- NA
    
  } else if (max(abs(test.results$"Frequency Sensitivity")) < 0.01) 
  {
    
    ymin.freq <- -0.01
    
    ymax.freq <- 0.01
    
  } else 
  {
    
    ymin.freq <- min(test.results$"Frequency Sensitivity") - 0.1*min(abs(test.results$"Frequency Sensitivity"))
    
    ymax.freq <- max(test.results$"Frequency Sensitivity") + 0.1*max(abs(test.results$"Frequency Sensitivity"))
    
  }  
  
  # testing: Noise Sensitivity ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Noise Sensitivity
    for (i in seq_along(tslistx.noise)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Noise Sensitivity"[[i]] <- dist.fun(tslistx.noise[[i]], 
                                                        tslisty.noise[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Noise Sensitivity
    for (i in seq_along(tslistx.noise)) 
    {
      
      argslist.noise <- list()
      
      argslist.noise[[1]] <- tslistx.noise[[i]]
      
      argslist.noise[[2]] <- tslisty.noise[[i]]
      
      names(argslist.noise)[1] <- "x"
      
      names(argslist.noise)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.noise[2+j] <- dist.args[j]
        
        names(argslist.noise)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Noise Sensitivity"[[i]] <- do.call(dist.fun, argslist.noise)
      
    }
    
  }
  
  plot.res$"Noise Sensitivity" <- data.frame(seq_along(tslistx.noise),
                                             test.results$"Noise Sensitivity")
  
  colnames(plot.res$"Noise Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Noise Sensitivity"))) 
  {
    
    ymin.noise <- NA
    
    ymax.noise <- NA
    
  } else if (max(abs(test.results$"Noise Sensitivity")) < 0.01) 
  {
    
    ymin.noise <- -0.01
    
    ymax.noise <- 0.01
    
  } else 
  {
    
    ymin.noise <- min(test.results$"Noise Sensitivity") - 0.1*min(abs(test.results$"Noise Sensitivity"))
    
    ymax.noise <- max(test.results$"Noise Sensitivity") + 0.1*max(abs(test.results$"Noise Sensitivity"))
    
  }  
  
  
  # testing: White Noise Sensitivity ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for White Noise Sensitivity
    for (i in seq_along(tslistx.wn)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"White Noise Sensitivity"[[i]] <- dist.fun(tslistx.wn[[i]], 
                                                              tslisty.wn[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for White Noise Sensitivity
    for (i in seq_along(tslistx.wn)) 
    {
      
      argslist.wn <- list()
      
      argslist.wn[[1]] <- tslistx.wn[[i]]
      
      argslist.wn[[2]] <- tslisty.wn[[i]]
      
      names(argslist.wn)[1] <- "x"
      
      names(argslist.wn)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.wn[2+j] <- dist.args[j]
        
        names(argslist.wn)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"White Noise Sensitivity"[[i]] <- do.call(dist.fun, argslist.wn)
      
    }
    
  }
  
  plot.res$"White Noise Sensitivity" <- data.frame(seq_along(tslistx.wn),
                                                   test.results$"White Noise Sensitivity")
  
  colnames(plot.res$"White Noise Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"White Noise Sensitivity"))) 
  {
    
    ymin.wn <- NA
    
    ymax.wn <- NA
    
  } else if (max(abs(test.results$"White Noise Sensitivity")) < 0.01) 
  {
    
    ymin.wn <- -0.01
    
    ymax.wn <- 0.01
    
  } else 
  {
    
    ymin.wn <- min(test.results$"White Noise Sensitivity") - 0.1*min(abs(test.results$"White Noise Sensitivity"))
    
    ymax.wn <- max(test.results$"White Noise Sensitivity") + 0.1*max(abs(test.results$"White Noise Sensitivity"))
    
  }  
  
  
  # testing: Biased Noise Sensitivity ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Biased Noise Sensitivity
    for (i in seq_along(tslistx.bn)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Biased Noise Sensitivity"[[i]] <- dist.fun(tslistx.bn[[i]], 
                                                               tslisty.bn[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Biased Noise Sensitivity
    for (i in seq_along(tslistx.bn)) 
    {
      
      argslist.bn <- list()
      
      argslist.bn[[1]] <- tslistx.bn[[i]]
      
      argslist.bn[[2]] <- tslisty.bn[[i]]
      
      names(argslist.bn)[1] <- "x"
      
      names(argslist.bn)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.bn[2+j] <- dist.args[j]
        
        names(argslist.bn)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Biased Noise Sensitivity"[[i]] <- do.call(dist.fun, argslist.bn)
      
    }
    
  }
  
  plot.res$"Biased Noise Sensitivity" <- data.frame(seq_along(tslistx.bn),
                                                    test.results$"Biased Noise Sensitivity")
  
  colnames(plot.res$"Biased Noise Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Biased Noise Sensitivity"))) 
  {
    
    ymin.bn <- NA
    
    ymax.bn <- NA
    
  } else if (max(abs(test.results$"Biased Noise Sensitivity")) < 0.01) 
  {
    
    ymin.bn <- -0.01
    
    ymax.bn <- 0.01
    
  } else 
  {
    
    ymin.bn <- min(test.results$"Biased Noise Sensitivity") - 0.1*min(abs(test.results$"Biased Noise Sensitivity"))
    
    ymax.bn <- max(test.results$"Biased Noise Sensitivity") + 0.1*max(abs(test.results$"Biased Noise Sensitivity"))
    
  }  
  
  
  # testing: Outlier Sensitivity ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Outlier Sensitivity
    for (i in seq_along(tslistx.out)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Outlier Sensitivity"[[i]] <- dist.fun(tslistx.out[[i]], 
                                                          tslisty.out[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Outlier Sensitivity
    for (i in seq_along(tslistx.out)) 
    {
      
      argslist.out <- list()
      
      argslist.out[[1]] <- tslistx.out[[i]]
      
      argslist.out[[2]] <- tslisty.out[[i]]
      
      names(argslist.out)[1] <- "x"
      
      names(argslist.out)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.out[2+j] <- dist.args[j]
        
        names(argslist.out)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Outlier Sensitivity"[[i]] <- do.call(dist.fun, argslist.out)
      
    }
    
  }
  
  plot.res$"Outlier Sensitivity" <- data.frame(seq_along(tslistx.out),
                                               test.results$"Outlier Sensitivity")
  
  colnames(plot.res$"Outlier Sensitivity") <- c("x", "y")
  
  if (is.na(max(test.results$"Outlier Sensitivity"))) 
  {
    
    ymin.out <- NA
    
    ymax.out <- NA
    
  } else if (max(abs(test.results$"Outlier Sensitivity")) < 0.01) 
  {
    
    ymin.out <- -0.01
    
    ymax.out <- 0.01
    
  } else 
  {
    
    ymin.out <- min(test.results$"Outlier Sensitivity") - 0.1*min(abs(test.results$"Outlier Sensitivity"))
    
    ymax.out <- max(test.results$"Outlier Sensitivity") + 0.1*max(abs(test.results$"Outlier Sensitivity"))
    
  }  
  
  
  # testing: Point Order Dependence ----
  
  # see Translation sensitivity for specific comments
  # Point Order Dependence is not plotted, so no need to check for NAs or adjust axes

  # create temporary vector to hold full results from Point Order test

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
      
    # loop to test each distance measure for Point Order Dependence
    
    point_order.temp.list <- lapply(tslistx.ord, function(x) 
      {dist.fun(x,tslisty.ord[[1]])})
    
    point_order.temp <- do.call(rbind, point_order.temp.list) 
    
    # take the mean as the final value
    test.results$"Point Order Dependence" <- mean(point_order.temp)
      
  } else 
  {
      
    # loop to test each distance measure for Point Order Dependence
    
    point_order.temp.list <- lapply(tslistx.ord, function(x)
      {argslist.ord <- list()
      
      argslist.ord[[1]] <- x
      
      argslist.ord[[2]] <- tslisty.ord[[1]]
      
      names(argslist.ord)[1] <- "x"
      
      names(argslist.ord)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.ord[2+j] <- dist.args[j]
        
        names(argslist.ord)[2+j] <- names(dist.args[j])
        
      }
      
      do.call(dist.fun, argslist.ord)
      
      })
    
    point_order.temp <- do.call(rbind, point_order.temp.list)

    # take the mean and use it as the final value
    test.results$"Point Order Dependence" <- mean(point_order.temp)
      
  }
    
  plot.res$"Point Order Dependence" <- data.frame(seq_along(test.results$"Point Order Dependence"),
                                            test.results$"Point Order Dependence")
  
  colnames(plot.res$"Point Order Dependence") <- c("x", "y")
  
  if (is.na(max(test.results$"Point Order Dependence"))) 
  {
    
    ymin.ord <- NA
    
    ymax.ord <- NA
    
  } else if (is.nan(max(test.results$"Point Order Dependence")))
  {
    
    ymin.ord <- NA
    
    ymax.ord <- NA
  
  } else if (max(abs(test.results$"Point Order Dependence")) < 0.01) 
  {
    
    ymin.ord <- -0.01
    
    ymax.ord <- 0.01
    
  } else 
  {
    
    ymin.ord <- min(test.results$"Point Order Dependence") - 0.1*min(abs(test.results$"Point Order Dependence"))
    
    ymax.ord <- max(test.results$"Point Order Dependence") + 0.1*max(abs(test.results$"Point Order Dependence"))
    
  }  
  
  # testing: Antiparallelism Bias ----
  
  # see Translation sensitivity for specific comments

  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
      
    # loop to test each distance measure for Antiparallelism Bias
    for (i in seq_along(tslistx.par)) 
    {
        
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Antiparallelism Bias"[[i]] <- dist.fun(tslistx.par[[i]], 
                                                             tslisty.par[[i]])
    }
      
  } else 
  {
      
    # loop to test each distance measure for Antiparallelism Bias
    for (i in seq_along(tslistx.par)) 
    {
        
      argslist.par <- list()
        
      argslist.par[[1]] <- tslistx.par[[i]]
        
      argslist.par[[2]] <- tslisty.par[[i]]
      
      names(argslist.par)[1] <- "x"
      
      names(argslist.par)[2] <- "y"
        
      for (j in 1:length(dist.args)) 
      {
          
        argslist.par[2+j] <- dist.args[j]
        
        names(argslist.par)[2+j] <- names(dist.args[j])
          
      }
        
      # use this line if there are extra arguments
      test.results$"Antiparallelism Bias"[[i]] <- do.call(dist.fun, argslist.par)
        
    }
      
  }
  
  plot.res$"Antiparallelism Bias" <- data.frame(seq_along(tslistx.par),
                                                  test.results$"Antiparallelism Bias")
  
  colnames(plot.res$"Antiparallelism Bias") <- c("x", "y")
  
  if (is.na(max(test.results$"Antiparallelism Bias"))) 
  {
    
    ymin.par <- NA
    
    ymax.par <- NA
    
  } else if (max(abs(test.results$"Antiparallelism Bias")) < 0.01) 
  {
    
    ymin.par <- -0.01
    
    ymax.par <- 0.01
    
  } else 
  {
    
    ymin.par <- min(test.results$"Antiparallelism Bias") - 0.1*min(abs(test.results$"Antiparallelism Bias"))
    
    ymax.par <- max(test.results$"Antiparallelism Bias") + 0.1*max(abs(test.results$"Antiparallelism Bias"))
    
  }
  
  # testing: Phase Invariance ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Phase Invariance
    for (i in seq_along(tslistx.phase)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Phase Invariance"[[i]] <- dist.fun(tslistx.phase[[i]], 
                                                       tslisty.phase[[i]])
    }
    
  } else 
  {
    
    # loop to test each distance measure for Phase Invariance
    for (i in seq_along(tslistx.phase)) 
    {
      
      argslist.phase <- list()
      
      argslist.phase[[1]] <- tslistx.phase[[i]]
      
      argslist.phase[[2]] <- tslisty.phase[[i]]
      
      names(argslist.phase)[1] <- "x"
      
      names(argslist.phase)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.phase[2+j] <- dist.args[j]
        
        names(argslist.phase)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Phase Invariance"[[i]] <- do.call(dist.fun, argslist.phase)
      
    }
    
  }
  
  plot.res$"Phase Invariance" <- data.frame(seq_along(tslistx.phase),
                                            test.results$"Phase Invariance")
  
  colnames(plot.res$"Phase Invariance") <- c("x", "y")
  
  if (is.na(max(test.results$"Phase Invariance"))) 
  {
    
    ymin.phase <- NA
    
    ymax.phase <- NA
    
  } else if (max(abs(test.results$"Phase Invariance")) < 0.01) 
  {
    
    ymin.phase <- -0.01
    
    ymax.phase <- 0.01
    
  } else 
  {
    
    ymin.phase <- min(test.results$"Phase Invariance") - 0.1*min(abs(test.results$"Phase Invariance"))
    
    ymax.phase <- max(test.results$"Phase Invariance") + 0.1*max(abs(test.results$"Phase Invariance"))
    
  } 
  
  # testing: Uniform Time Scaling Invariance ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Uniform Time Scaling Invariance
    for (i in seq_along(tslistx.uni)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Uniform Time Scaling Invariance"[[i]] <- dist.fun(tslistx.uni[[i]], 
                                                                       tslisty.uni[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Amplitude Sensitivity
    for (i in seq_along(tslistx.uni)) 
    {
      
      argslist.uni <- list()
      
      argslist.uni[[1]] <- tslistx.uni[[i]]
      
      argslist.uni[[2]] <- tslisty.uni[[i]]
      
      names(argslist.uni)[1] <- "x"
      
      names(argslist.uni)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.uni[2+j] <- dist.args[j]
        
        names(argslist.uni)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Uniform Time Scaling Invariance"[[i]] <- do.call(dist.fun, argslist.uni)
      
    }
    
  }
  
  plot.res$"Uniform Time Scaling Invariance" <- data.frame(seq_along(tslistx.uni),
                                                  test.results$"Uniform Time Scaling Invariance")
  
  colnames(plot.res$"Uniform Time Scaling Invariance") <- c("x", "y")
  
  if (is.na(max(test.results$"Uniform Time Scaling Invariance"))) 
  {
    
    ymin.uni <- NA
    
    ymax.uni <- NA
    
  } else if (max(abs(test.results$"Uniform Time Scaling Invariance")) < 0.01) 
  {
    
    ymin.uni <- -0.01
    
    ymax.uni <- 0.01
    
  } else 
  {
    
    ymin.uni <- min(test.results$"Uniform Time Scaling Invariance") - 0.1*min(abs(test.results$"Uniform Time Scaling Invariance"))
    
    ymax.uni <- max(test.results$"Uniform Time Scaling Invariance") + 0.1*max(abs(test.results$"Uniform Time Scaling Invariance"))
    
  }
  
  # testing: Warping Invariance ----
  
  # see Translation sensitivity for specific comments
  # Warping Invariance is not plotted, so no need to check for NAs or adjust axes
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Warping Invariance
    for (i in seq_along(tslistx.warp)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Warping Invariance"[[i]] <- dist.fun(tslistx.warp[[i]], 
                                                             tslisty.warp[[i]])
    }
    
  } else 
  {
    
    # loop to test each distance measure for Warping Invariance
    for (i in seq_along(tslistx.warp)) 
    {
      
      argslist.warp <- list()
      
      argslist.warp[[1]] <- tslistx.warp[[i]]
      
      argslist.warp[[2]] <- tslisty.warp[[i]]
      
      names(argslist.warp)[1] <- "x"
      
      names(argslist.warp)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.warp[2+j] <- dist.args[j]
        
        names(argslist.warp)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Warping Invariance"[[i]] <- do.call(dist.fun, argslist.warp)
      
    }
    
  }
  
  plot.res$"Warping Invariance" <- data.frame(seq_along(tslistx.warp),
                                                  test.results$"Warping Invariance")
  
  colnames(plot.res$"Warping Invariance") <- c("x", "y")
  
  # testing: Shuffling Invariance ----
  
  # see Translation sensitivity for specific comments
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Shuffling Invariance
    for (i in seq_along(tslistx.shuf)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      test.results$"Shuffling Invariance"[[i]] <- dist.fun(tslistx.shuf[[i]], 
                                                            tslisty.shuf[[i]])
      
    }
    
  } else 
  {
    
    # loop to test each distance measure for Shuffling Invariance
    for (i in seq_along(tslistx.shuf)) 
    {
      
      argslist.shuf <- list()
      
      argslist.shuf[[1]] <- tslistx.shuf[[i]]
      
      argslist.shuf[[2]] <- tslisty.shuf[[i]]
      
      names(argslist.shuf)[1] <- "x"
      
      names(argslist.shuf)[2] <- "y"
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.shuf[2+j] <- dist.args[j]
        
        names(argslist.shuf)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      test.results$"Shuffling Invariance"[[i]] <- do.call(dist.fun, argslist.shuf)
      
    }
    
  }
  
  plot.res$"Shuffling Invariance" <- data.frame(seq_along(tslistx.shuf),
                                                 test.results$"Shuffling Invariance")
  
  colnames(plot.res$"Shuffling Invariance") <- c("x", "y")
  
  if (is.na(max(test.results$"Shuffling Invariance"))) 
  {
    
    ymin.shuf <- NA
    
    ymax.shuf <- NA
    
  } else if (max(abs(test.results$"Shuffling Invariance")) < 0.01) 
  {
    
    ymin.shuf <- -0.01
    
    ymax.shuf <- 0.01
    
  } else 
  {
    
    ymin.shuf <- min(test.results$"Shuffling Invariance") - 0.1*min(abs(test.results$"Shuffling Invariance"))
    
    ymax.shuf <- max(test.results$"Shuffling Invariance") + 0.1*max(abs(test.results$"Shuffling Invariance"))
    
  }  
  
  # testing: Relative Sensitivity Ranges ----
  
  # see Translation sensitivity for specific comments
  
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


  rsr.mean <- mean(test.results$"Relative Sensitivity Ranges")
  
  for (i in seq_along(test.results$"Relative Sensitivity Ranges"))
  {
    
    test.results$"Relative Sensitivity Ranges"[[i]] <-
      test.results$"Relative Sensitivity Ranges"[[i]] / rsr.mean
    
  }
  
  
  sens <- seq(1,length(test.results$"Relative Sensitivity Ranges"))  
  
  plot.res$"Relative Sensitivity Ranges" <- data.frame(sens,
                                                  test.results$"Relative Sensitivity Ranges")
  
  colnames(plot.res$"Relative Sensitivity Ranges") <- c("x", "y")
  
  if (is.na(max(test.results$"Relative Sensitivity Ranges"))) 
  {
    
    ymin.range <- NA
    
    ymax.range <- NA
    
  } else if (is.nan(max(test.results$"Relative Sensitivity Ranges")))
  {
    
    ymin.range <- NA
    
    ymax.range <- NA
    
  } else if (max(abs(test.results$"Relative Sensitivity Ranges")) < 0.01) 
  {
    
    ymin.range <- -0.01
    
    ymax.range <- 0.01
    
  } else 
  {
    
    ymin.range <- min(test.results$"Relative Sensitivity Ranges") - 0.1*min(abs(test.results$"Relative Sensitivity Ranges"))
    
    ymax.range <- max(test.results$"Relative Sensitivity Ranges") + 0.1*max(abs(test.results$"Relative Sensitivity Ranges"))
    
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
                         seq(1, length(test.results$"Scale Sensitivity")),
                         seq(1, length(test.results$"Amplitude Sensitivity")), 
                         seq(1, length(test.results$"Duration Sensitivity")),
                         seq(1, length(test.results$"Frequency Sensitivity")),
                         seq(1, length(test.results$"Noise Sensitivity")),
                         seq(1, length(test.results$"White Noise Sensitivity")),
                         seq(1, length(test.results$"Biased Noise Sensitivity")),
                         seq(1, length(test.results$"Outlier Sensitivity")),
                         seq(1, length(test.results$"Point Order Dependence")), 
                         seq(1, length(test.results$"Antiparallelism Bias")),
                         seq(1, length(test.results$"Phase Invariance")),
                         seq(1, length(test.results$"Uniform Time Scaling Invariance")),
                         seq(1, length(test.results$"Warping Invariance")),
                         seq(1, length(test.results$"Shuffling Invariance")),
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

  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.trans)) 
  {
    
    # save ggplot of Translation sensitivity
    p1 <- ggplot(plot.res$"Translation Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.trans, ymax.trans) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Translation Sensitivity") +
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
  if (!is.na(ymin.phase)) 
  {
 
    # save ggplot of Phase Invariance
    p2 <- ggplot(plot.res$"Phase Invariance",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.phase, ymax.phase) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Phase Invariance") +
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
    
    # save ggplot of Amplitude Sensitivity
    p3 <- ggplot(plot.res$"Amplitude Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.amp, ymax.amp) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Amplitude Sensitivity") +
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

    # save ggplot of Duration Sensitivity
    p4 <- ggplot(plot.res$"Duration Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.dur, ymax.dur) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Duration Sensitivity") +
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
  if (!is.na(ymin.freq)) 
  {
    
    # save ggplot of Frequency Sensitivity
    p5 <- ggplot(plot.res$"Frequency Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.freq, ymax.freq) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Frequency Sensitivity") +
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
  if (!is.na(ymin.scale)) 
  {
    
    # save ggplot of Scale Sensitivity
    p6 <- ggplot(plot.res$"Scale Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.scale, ymax.scale) +
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
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.noise)) 
  {
    
    # save ggplot of Noise Sensitivity
    p7 <- ggplot(plot.res$"Noise Sensitivity",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.noise, ymax.noise) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Noise Sensitivity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p7 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.par)) 
  {
    
    # save ggplot of Antiparallelism Bias
    p10 <- ggplot(plot.res$"Antiparallelism Bias",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.par, ymax.par) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Antiparallelism Bias") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p10 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.ord)) 
  {
    
    # save ggplot of Point Order Dependence
    p11 <- ggplot(plot.res$"Point Order Dependence",
                 aes(x = x, 
                     y = y)) +
      geom_point(size=2) +
      ylim(ymin.ord, ymax.ord) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Point Order Dependence") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p11 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.shuf)) 
  {
    
    # save ggplot of Relative Sensitivities
    p13 <- ggplot(plot.res$"Shuffling Invariance",
                  aes(x = x, 
                      y = y)) +
      geom_point(size=2) +
      ylim(ymin.shuf, ymax.shuf) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Shuffling Invariance") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p13 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.range)) 
  {
    
    # save ggplot of Relative Sensitivities
    p14 <- ggplot(plot.res$"Relative Sensitivity Ranges",
                  aes(x = x, 
                      y = y)) +
      geom_point(size=2) +
      ylim(ymin.range, ymax.range) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Relative Sensitivity Ranges") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p14 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.wn)) 
  {
    
    # save ggplot of White Noise Sensitivity
    p15 <- ggplot(plot.res$"White Noise Sensitivity",
                  aes(x = x, 
                      y = y)) +
      geom_point(size=2) +
      ylim(ymin.wn, ymax.wn) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("White Noise Sensitivity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p15 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.bn)) 
  {
    
    # save ggplot of Biased Noise Sensitivity
    p16 <- ggplot(plot.res$"Biased Noise Sensitivity",
                  aes(x = x, 
                      y = y)) +
      geom_point(size=2) +
      ylim(ymin.bn, ymax.bn) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Biased Noise Sensitivity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p16 <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.out)) 
  {
    
    # save ggplot of Outlier Sensitivity
    p17 <- ggplot(plot.res$"Outlier Sensitivity",
                  aes(x = x, 
                      y = y)) +
      geom_point(size=2) +
      ylim(ymin.out, ymax.out) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle("Outlier Sensitivity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12))
    
  } else 
  {
    
    # save blank plot
    p17 <- ggplot() +
      theme_void()
    
  }
  
 
  # arrange the 4 saved plots into one
  p.all <- grid.arrange(p1,
                        p2,
                        p3,
                        p4,
                        p5,
                        p7,
                        p10,
                        p11,
                        p13,
                        p15,
                        p16,
                        p17,
                        ncol=4, 
                        top = textGrob(paste(print_name, " Test Results", sep=""), 
                                       gp=gpar(fontsize=16,
                                               font=4)))
  
  # write results to files ----
  
  # save working directory path to a variable
  wd <- getwd()
  
  # write the grid-arranged plot as a jpg image
  ggsave(paste(wd, "/plots/test_results_3/", print_name, "_plots.jpg", sep=""), p.all)
  
  # write the flextable as a Word document
  save_as_docx(tr_table, 
               path = paste(wd, "/tables/test_results_3/", print_name, "_table.docx", sep=""))
  
  # write the wide-form data table to an R file
  saveRDS(tr_wide, file = paste(wd, "/files/", print_name, "_dfwide.RData", sep=""))
  # send the flextable object to the main R environment  
  return(tr_table)
  
}
