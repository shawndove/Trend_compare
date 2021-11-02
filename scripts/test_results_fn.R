test.results.fn <- function(tslistx, tslisty, x, y, dist.fun, dist.args) {
  
  source("ts_testing_controlled.R")
  
  test.results <- vector()
  
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Outlier Sensitivity
    for (i in seq_along(tslistx)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      try(test.results[[i]] <- dist.fun(tslistx[[i]], tslisty[[i]]))
      
    }
    
  } else 
  {
    
    # loop to test each distance measure
    for (i in seq_along(tslistx)) 
    {
      
      argslist <- list()
      
      argslist[[1]] <- tslistx[[i]]
      
      argslist[[2]] <- tslisty[[i]]
      
      names(argslist)[1] <- x
      
      names(argslist)[2] <- y
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist[2+j] <- dist.args[j]
        
        names(argslist)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      try(test.results[[i]] <- do.call(dist.fun, argslist))
      
    }
    
  }
  
  if(length(test.results)==0)
  {
    
    test.results <- NA
    
  }
  
  plot.res <- data.frame(seq_along(tslistx), test.results)
  
  colnames(plot.res) <- c("x", "y")
  
  if (is.na(max(test.results))) 
  {
    
    ymin <- NA
    
    ymax <- NA
    
  } else if (max(abs(test.results)) < 0.01) 
  {
    
    ymin <- -0.01
    
    ymax <- 0.01
    
  } else 
  {
    
    ymin <- min(test.results) - 0.1*min(abs(test.results))
    
    ymax <- max(test.results) + 0.1*max(abs(test.results))
    
  }  
  
  return(list(test.results, plot.res, ymin, ymax))
  
}
