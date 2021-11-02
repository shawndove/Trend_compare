ti.test.fn1 <- function(dist.fun, 
                       print_name, 
                       dist.args,
                       x,
                       y,
                       tpops=100000,
                       tmax=5) 
{
  
# store sequence of years for plotting purposes
#years <- seq(1,100)

# create 3 lists of random time series using pgrowth4 function
ts_list.1 <- as.matrix(pgrowth(tpops, tmax, gr_start=1, gr_mean=0, gr_sd=1))
ts_list.2 <- as.matrix(pgrowth(tpops, tmax, gr_start=1, gr_mean=0, gr_sd=0.3))
ts_list.3 <- as.matrix(pgrowth(tpops, tmax, gr_start=1, gr_mean=2, gr_sd=0.5))

#ts_list.1 <- vector(length=1000)
#ts_list.1 <- as.matrix(do.call(rbind, lapply(ts_list.1, function(x) sample(1:1000, 100))))
#ts_list.2 <- vector(length=1000)
#ts_list.2 <- as.matrix(do.call(rbind, lapply(ts_list.2, function(x) sample(1:1000, 100))))
#ts_list.3 <- vector(length=1000)
#ts_list.3 <- as.matrix(do.call(rbind, lapply(ts_list.3, function(x) sample(1:1000, 100))))

# create a vector to hold the results of each triangle inequality test
# at the end we will check for any failures to determine the final pass/fail result
ti_results <- vector()
ti_full_results <- list()
nn_temp_results <- vector()
rcounter <- c(1,2,3)

for (h in 1:nrow(ts_list.1)) {
  
  # take a single time series from each list and put each one into a vector
  ts_vec.1 <- ts_list.1[h,]
  ts_vec.2 <- ts_list.2[h,]
  ts_vec.3 <- ts_list.3[h,]
  
  # define ordered sets of time series to be compared, list x vs list y
  tslistx.ti <- list(ts_vec.2,
                     ts_vec.3,
                     ts_vec.3)
  
  tslisty.ti <- list(ts_vec.1,
                     ts_vec.2,
                     ts_vec.1)
  
  # create an empty vector to hold results of time series comparisons
  ti_temp_results <- vector()
  
  # check if any additional arguments are specified for the distance measure
  if (!is.list(dist.args)) 
  {
    
    # loop to test each distance measure for Triangle Inequality
    for (i in seq_along(tslistx.ti)) 
    {
      
      # use this line to call the distance measure fn if there are no extra arguments
      ti_temp_results[i] <- dist.fun(tslistx.ti[[i]], 
                                tslisty.ti[[i]])
    }
    
  } else 
  {
    
    # loop to test each distance measure
    for (i in seq_along(tslistx.ti)) 
    {
      
      argslist.ti <- list()
      
      argslist.ti[[1]] <- tslistx.ti[[i]]
      names(argslist.ti)[1] <- x
      
      argslist.ti[[2]] <- tslisty.ti[[i]]
      names(argslist.ti)[2] <- y
      
      for (j in 1:length(dist.args)) 
      {
        
        argslist.ti[2+j] <- dist.args[j]
        names(argslist.ti)[2+j] <- names(dist.args[j])
        
      }
      
      # use this line if there are extra arguments
      ti_temp_results[i] <- do.call(dist.fun, argslist.ti)
      
    }
    
  }
  
  if(any(is.nan(ti_temp_results))) {
    
    ti_results[h] <- NA
    
    nn_temp_results[rcounter] <- NA
    
    rcounter <- rcounter + 3
    
  } else {
  
    ti_full_results[[h]] <- ti_temp_results
  
    nn_temp_results[rcounter] <- ifelse(any(ti_temp_results < 0), FALSE, TRUE)
  
    rcounter <- rcounter + 3
  
    # save the largest result as the longest side of a triangle
    ti_longside <- max(ti_temp_results)
  
    # save the other two results as the shorter sides of the triangle
    ti_othersides <- ti_temp_results[-grep(max(ti_temp_results), ti_temp_results)]
  
    # check whether the max result is unique (if so, there should be 2 othersides)
    # if it is not unique, the two longest sides are of equal length
    # the dm passes the triangle inequality test by default
    # because the two longest sides are equal, so one side cannot be longer than
    # the other two combined
    if(length(ti_othersides)<2) {
    
      # return true
      ti_results[h] <- TRUE
    
    } else {
      
      # if it is unique, subtract the two shorter sides from the longside
      temp_result <- ti_longside - sum(ti_othersides)
    
      # if the difference is greater than 0, the dm fails the test, return FALSE
      # if it is less than or equal to 0, return TRUE
      # note that 0.0001 has been used instead of 0, as apparent rounding errors 
      # were leading to occasional false negatives
      ti_results[h] <- ifelse(temp_result > 0.0001, FALSE, TRUE)
    
    }
  
  }
  
}

# check whether the dm passed all triangle inequality tests
# if so return TRUE, if not return FALSE
final_ti_result <- all(ti_results[!is.na(ti_results)])

final_nn_result <- all(nn_temp_results[!is.na(nn_temp_results)])
#final_nn_result <- ifelse(!all(nn_temp_results)==TRUE, FALSE, TRUE)

ti_full_results[[tpops+1]] <- ti_results
ti_full_results[[tpops+2]] <- final_ti_result
ti_full_results[[tpops+3]] <- ts_list.1
ti_full_results[[tpops+4]] <- ts_list.2
ti_full_results[[tpops+5]] <- ts_list.3

nn_full_results <- list(nn_temp_results, final_nn_result)

# save working directory path to a variable
wd <- getwd()

# save full results as RData file
saveRDS(ti_full_results, file = paste(wd, "/files/ti_results_temp/", print_name, "_tifull.RData", sep=""))
saveRDS(nn_full_results, file = paste(wd, "/files/nn_results_temp/", print_name, "_nnfull.RData", sep=""))

final_results <- list(final_ti_result, final_nn_result)

#return(final_ti_result)
return(final_results)

}
