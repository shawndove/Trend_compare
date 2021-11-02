### Standard Deviation Distance

# function, with x and y as time series, and alpha as moving average window length
sddist <- function(x, y, alpha) {
  
  if(class(try(
    if (! is.numeric(x) | ! is.numeric(y)) {
      stop('The series must be numeric.', call.=FALSE)
    }
  ))=="try-error") {return(NA)} 
  if(class(try(
    if (! is.vector(x) | ! is.vector(y)) {
      stop('The series must be univariate vectors', call.=FALSE)
    }
  ))=="try-error") {return(NA)} 
  if(class(try(
    if (length(x) < 1 | length(y) < 1) {
      stop('The series must have at least one point.', call.=FALSE)
    }
  ))=="try-error") {return(NA)}
  if(class(try(
    if (length(x) != length(y)) {
      stop('Both series must have the same length.', call.=FALSE)
    }
  ))=="try-error") {return(NA)}
  if(class(try(
    if (any(is.na(x)) | any(is.na(y))) {
      stop('There are missing values in the series.', call.=FALSE)
    }
  ))=="try-error") {return(NA)} 
  if (! missing(alpha)) {
    if(class(try(
      if (round(alpha) != alpha) {
        stop('alpha must be an integer value', call.=FALSE)
      }
    ))=="try-error") {return(NA)}
    if(class(try(
      if(alpha <= 1) {
        stop("alpha must be greater than 1")
      }
    ))=="try-error") {return(NA)}
  }
  
  
  # take difference of two time series
  z = x - y
  
  # set limits for moving average
  dist <- alpha - 1
  
  # create vector to hold standard deviation values
  t <- vector()
  
  # set variable n as the length of differece between the time series
  n <- length(z)
  
  # if window size is less than the length of the time series...
  if (alpha < length(x)) {
    
    # loop to get moving average of the standard deviation
    # using the sample moving average function
    for (i in 1:(n-dist)) {
      
      t[i] <- sd(z[i:(i+dist)])
      
    }
    
  # if window size is the full length of the time series
  } else {
    
    # get the population standard deviation
    t <- sqrt((n-1)/n) * sd(z)

  }

  
  # take the mean of the standard deviation values
  dev_val <- mean(t)
  
  # return the mean
  return(dev_val)
}


####


