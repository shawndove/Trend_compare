# Function to test bird data time series with distance measures
# and print the raw results to Word tables and jpg images
# Shawn Dove (s.dove@ucl.ac.uk) - December, 2020

get.dm.plot.birds1 <- function(dist.fun, 
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
  
  # create a list of results with an element for each set of trends
  bird.results <- list()
  bird.results$"Unsmoothed" <- vector()
  bird.results$"Smoothed" <- vector()

  # create a list to hold plots of some results
  plot.res <- list()
  
  # create vectors to hold species names
  unsm.species <- vector()
  sm.species <- vector()
  
  # unsmoothed time series ----
  
  # loop to compare each pair of time series
  for (i in seq_along(trends.unsm.res.list)) 
  {
      
    # call the distance measure function
    bird.results$"Unsmoothed"[[i]] <- do.call(dist.fun, c(list(trends.unsm.res.list[[i]]$imputed), 
                                                          list(trends.unsm.cf.list[[i]]$imputed),
                                                          dist.args))
      
    # get species name for each pair of time series
    unsm.species[i] <- as.character(trends.unsm.res.list[[i]]$species[1])
  }

  # create a data frame to hold results in a format compatible with ggplot
  plot.res$"Unsmoothed" <- data.frame(seq_along(trends.unsm.res.list),
                                      bird.results$"Unsmoothed",
                                      unsm.species)
  
  # name the columns
  colnames(plot.res$"Unsmoothed") <- c("x", "y", "Species")
  
  plot.res$Unsmoothed$Species <- factor(plot.res$Unsmoothed$Species, levels = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"))
  
  # check for NAs in test results
  if (is.na(max(bird.results$"Unsmoothed"))) 
  {
    
    # set ymin and ymax to NA. ggplot will be told to avoid plotting these results
    ymin.unsm <- NA
    
    ymax.unsm <- NA
    
    # if the values returned are very small, set y axis from -0.01 to 0.01
    # this ensures that very small values will be clearly shown as near zero
    
  } else if (max(abs(bird.results$"Unsmoothed")) < 0.01) 
  {
    
    ymin.unsm <- -0.01
    
    ymax.unsm <- 0.01
    
    # if the values are not very small, set y axis according to the data
    
  } else 
  {
    
    ymin.unsm <- min(bird.results$"Unsmoothed") - 0.1*min(abs(bird.results$"Unsmoothed"))
    
    ymax.unsm <- max(bird.results$"Unsmoothed") + 0.1*max(abs(bird.results$"Unsmoothed"))
    
  }
  
  # smoothed time series ----
  
  # loop to compare each pair of time series
  for (i in seq_along(trends.sm.res.list)) 
  {
      
    # call the distance measure function
    bird.results$"Smoothed"[[i]] <- do.call(dist.fun, c(list(trends.sm.res.list[[i]]$sm_fixed), 
                                                        list(trends.sm.cf.list[[i]]$sm_fixed),
                                                        dist.args))
      
    # get species name for each pair of time series
    sm.species[i] <- as.character(trends.sm.res.list[[i]]$species[1])
  }
    
  # create a data frame to hold results in a format compatible with ggplot
  plot.res$"Smoothed" <- data.frame(seq_along(trends.sm.res.list),
                                      bird.results$"Smoothed",
                                      sm.species)
  
  # name the columns
  colnames(plot.res$"Smoothed") <- c("x", "y", "Species")
  
  plot.res$Smoothed$Species <- factor(plot.res$Smoothed$Species,levels = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"))
  
  # check for NAs in test results
  if (is.na(max(bird.results$"Smoothed"))) 
  {
    
    # set ymin and ymax to NA. ggplot will be told to avoid plotting these results
    ymin.sm <- NA
    
    ymax.sm <- NA
    
    # if the values returned are very small, set y axis from -0.01 to 0.01
    # this ensures that very small values will be clearly shown as near zero
    
  } else if (max(abs(bird.results$"Smoothed")) < 0.01) 
  {
    
    ymin.sm <- -0.01
    
    ymax.sm <- 0.01
    
    # if the values are not very small, set y axis according to the data
    
  } else 
  {
    
    ymin.sm <- min(bird.results$"Smoothed") - 0.1*min(abs(bird.results$"Smoothed"))
    
    ymax.sm <- max(bird.results$"Smoothed") + 0.1*max(abs(bird.results$"Smoothed"))
    
  }  
  

  # create results plots ----
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.unsm)) 
  {
    
    # save ggplot of unsmoothed results
    p1.birds <- ggplot(plot.res$"Unsmoothed",
                 aes(x = x, 
                     y = y,
                     colour = Species,
                     shape = Species)) +
      geom_point(size=4) +
      ylim(ymin.unsm, ymax.unsm) +
      xlim(0, 6) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle(paste(print_name)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    
  } else 
  {
    
    # save blank plot
    p1.birds <- ggplot() +
      theme_void()
    
  }
  
  # check for NA in ymin value
  # if NA, then plot will be blank
  if (!is.na(ymin.sm)) 
  {
    
    # save ggplot of smoothed results
    p2.birds <- ggplot(plot.res$"Smoothed",
                 aes(x = x, 
                     y = y,
                     colour = Species,
                     shape = Species)) +
      geom_point(size=4) +
      ylim(ymin.sm, ymax.sm) +
      xlim(0, 6) +
      theme(legend.title = element_text(color="blue")) +
      ggtitle(paste(print_name)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    
  } else 
  {
    
    # save blank plot
    p2.birds <- ggplot() +
      theme_void()
    
  }
  
  # return the ggplot objects
  return(list(p1.birds, p2.birds))
  
}
