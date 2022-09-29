# Function to test bird data time series with distance measures
# and print the raw results to Word tables and jpg images
# Shawn Dove (s.dove@ucl.ac.uk) - December, 2020

get.simp.unsmoothed.result.birds <- function(dist.fun, 
                                 print_name, 
                                 dist.args)
{
  
  # prepare variables ----
  
  # create a list of results with an element for each set of trends
  bird.results <- vector()
  

  # create a list to hold plots of some results
  plot.res <- list()
  
  # create vectors to hold species names
  unsm.species <- vector()
 
  # unsmoothed time series ----
  
  # loop to compare each pair of time series
  for (i in seq_along(trends.unsm.res.list)) 
  {
    
    # call the distance measure function
    bird.results[[i]] <- do.call(dist.fun, c(list(trends.unsm.res.list[[i]]$imputed), 
                                             list(trends.unsm.cf.list[[i]]$imputed), 
                                             dist.args))
    
    # get species name for each pair of time series
    unsm.species[i] <- as.character(trends.unsm.res.list[[i]]$species[1])
  }
  
  # create a data frame to hold results in a format compatible with ggplot
  plot.res <- data.frame(bird.results,
                         unsm.species,
                         print_name)
  
  # name the columns
  colnames(plot.res) <- c("y", "Species", "DM")
  
  plot.res$Species <- factor(plot.res$Species, levels = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"))
  
  return(plot.res)
  
}

# Function to test bird data time series with distance measures
# and print the raw results to Word tables and jpg images
# Shawn Dove (s.dove@ucl.ac.uk) - December, 2020

get.simp.smoothed.result.birds <- function(dist.fun, 
                                             print_name, 
                                             dist.args)
{
  
  # prepare variables ----
  
  # create a list of results with an element for each set of trends
  bird.results <- vector()

  # create a list to hold plots of some results
  plot.res <- list()
  
  # create vectors to hold species names
  sm.species <- vector()
  
  # smoothed time series ----
  
  # loop to compare each pair of time series
  for (i in seq_along(trends.sm.res.list)) 
  {
    
    # call the distance measure function
    bird.results[[i]] <- do.call(dist.fun, c(list(trends.sm.res.list[[i]]$sm_fixed), 
                                                        list(trends.sm.cf.list[[i]]$sm_fixed),
                                                        dist.args))
    
    # get species name for each pair of time series
    sm.species[i] <- as.character(trends.sm.res.list[[i]]$species[1])
  }
  
  # create a data frame to hold results in a format compatible with ggplot
  plot.res <- data.frame(bird.results,
                         sm.species,
                         print_name)
  
  # name the columns
  colnames(plot.res) <- c("y", "Species", "DM")
  
  plot.res$Species <- factor(plot.res$Species,levels = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"))
  
  return(plot.res)
  
}