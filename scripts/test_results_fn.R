# function for controlled testing of distance measures

test.results.fn <- function(tslistx, 
                            tslisty, 
                            dist.fun, 
                            dist.args) 
  
{
  
  # create vector for results
  test.results <- vector()
  
  # loop to test each property
  for (i in seq_along(tslistx)) 
  {
      
    # call the distance measure function
    try(test.results[[i]] <- do.call(dist.fun, c(list(tslistx[[i]]), 
                                                 list(tslisty[[i]]), 
                                                 dist.args)))
      
  }

  # if there are no results, replace with NA
  if(length(test.results)==0)
  {
    
    test.results <- NA
    
  }
  
  # create data frame for plotting later
  plot.res <- data.frame(seq_along(tslistx), test.results)
  
  # name columns
  colnames(plot.res) <- c("x", "y")
  
  # this code sets the ylim values that will be used for ggplot later on
  
  # if all test results are NA, set ymin and ymax to NA so no plot will be made
  if (all(is.na(test.results))) 
  {
    
    ymin <- NA
    
    ymax <- NA
    
    # if max result is very small, set ymin and ymax very small 
  } else if (max(abs(test.results), na.rm=TRUE) < 0.01) 
  {
    
    ymin <- -0.01
    
    ymax <- 0.01
    
    # otherwise set ymin and ymax to slightly below and above (respectively) 
    # the range of test results
  } else 
  {
    
    ymin <- min(test.results, na.rm=TRUE) - 0.1*min(abs(test.results), na.rm=TRUE)
    
    ymax <- max(test.results, na.rm=TRUE) + 0.1*max(abs(test.results), na.rm=TRUE)
    
  }  
  
  # return results vector, plotting dataframe, and ymin and ymax values to parent function
  return(list(test.results, plot.res, ymin, ymax))
  
}
