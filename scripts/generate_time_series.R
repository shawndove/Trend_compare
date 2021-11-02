
pgrowth <- function(tpops, tmax, gr_start, gr_mean, gr_sd) {
  
  t_dist <- rnorm(10000, gr_mean, gr_sd)
  
  all_pops <- matrix(NA, ncol=tmax, nrow=tpops)
  
  t <- vector()
  
  for (i in 1:tpops) {
    
    t[1] <- gr_start
    
    for (j in 2:tmax) {
      
      t[j] <- t[j-1] + sample(t_dist, 1)
      
    }
    
    all_pops[i,] <- t
    
  }
  
  all_pops <- as.data.frame(all_pops)
  colnames(all_pops) <- c(1:(ncol(all_pops)))
  return(all_pops)
}

