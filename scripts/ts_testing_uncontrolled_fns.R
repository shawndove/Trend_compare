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
  
  set.seed(23) # ensures the sampling will be consistent each time
  
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
  
  set.seed(23) # ensures the sampling will be consistent each time

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
  
  set.seed(23) # ensures the same time point will be sampled each time
  
  if (length(t) <= 3) {
    
    stop("time series must have a length of at least 4")
    
  }
  
  rand <- sample(seq_along(t)[2:(length(t)-1)], 1)
  
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
  
  set.seed(23) # ensures the same time point will be sampled each time
  
  if (round(q)!=q) {
    
    stop("Input for q must be a whole number.")
  }
  
  rand <- sample(seq(1, length(t)), 1)
  
  u <- c(t[1:rand], rep(t[rand], (q-1)), t[rand:length(t)])
  
  return(u)
  
}

# deletion invariance??
ts_local_delete <- function(t, q) {
  
  set.seed(23) # ensures the same time points will be sampled each time
  
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

