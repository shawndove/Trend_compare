# Function to test bird data time series with distance measures
# and print the raw results to Word tables and jpg images
# Shawn Dove (s.dove@ucl.ac.uk) - December, 2020

get.dm.result.birds1 <- function(dist.fun, 
                                print_name, 
                                dist.args)
{
  
  # packages ----
  
  library(ggplot2)
  library(patchwork)
  library(ggpubr)
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
  
  # create results table ----
  
  # turn results list into a long-form data frame
  tr_dataframe <- ldply(bird.results, 
                        .fun = data.frame)
  
  # name columns
  colnames(tr_dataframe) <- c("Smoothing", 
                              "Species")
  
  # add a column to define row-column structure for conversion to wide-form
  tr_dataframe$Time <- c(seq(1, length(trends.unsm.res.list)), 
                         seq(1, length(trends.sm.res.list)))

  # convert to wide-form data frame with one row for each desirable property
  tr_wide <- reshape(tr_dataframe, 
                     timevar = "Species", 
                     idvar = "Smoothing", 
                     direction = "wide")
  
  #colnames(tr_wide[2:6]) <- unsm.species
  #colnames(tr_wide[7:11]) <- sm.species
  
  # create a flextable from the wide-form data frame
  tr_table_birds <- flextable(tr_wide)
  
  # change the number of decimal places to display based on the size of the largest
  # value in the results. this ensures results will be displayed as precisely as
  # possible without the table becoming too wide
  if (max(tr_wide[,2:length(tr_wide)], na.rm=TRUE) < 10) 
  {
    
    tr_table_birds <- colformat_num(tr_table_birds, big.mark=",", digits = 4, na_str = "")
    
  } else if (max(tr_wide[,2:length(tr_wide)], na.rm=TRUE) < 100) 
  {
    
    tr_table_birds <- colformat_num(tr_table_birds, big.mark=",", digits = 3, na_str = "")
    
  } else if (max(tr_wide[,2:length(tr_wide)], na.rm=TRUE) < 1000) 
  {
    
    tr_table_birds <- colformat_num(tr_table_birds, big.mark=",", digits = 2, na_str = "")
    
    # number of decimal places displayed will never be less than 2
  } else 
  {
    
    tr_table_birds <- colformat_num(tr_table_birds, big.mark=",", digits = 2, na_str = "")
    
  }
  
  # this line adds a blank prefix and suffix to fix an issue where the first column 
  # of the table, containing the names of the desirable properties, was not left-justified
  # when exported to Word, although it displayed correctly as a flextable within R.
  tr_table_birds <- colformat_char(tr_table_birds, j=1, prefix="",suffix="")
  
  # minimize the cell padding to reduce table width
  tr_table_birds <- padding(tr_table_birds, padding.left = 0.1, padding.right = 0.1, part = "all")
  
  # set font size larger for results values if none are greater than 1000
  # and smaller if any values are greater than 1000. this allows retention of 2 
  # decimal places without making the table too wide when some results values are very large
  if (max(tr_wide[,2:length(tr_wide)], na.rm=TRUE) < 1000) 
  {
    
    tr_table_birds <- fontsize(tr_table_birds, size = 9, part = "body")
    
  } else 
  {
    
    tr_table_birds <- fontsize(tr_table_birds, size = 7, part = "body")
    
  }
  
  # set font size for column headers
  tr_table_birds <- fontsize(tr_table_birds, size = 10, part = "header")
  
  # center column headers
  tr_table_birds <- align(tr_table_birds, align="center", part="header")
  
  # left-justify the body of the table
  tr_table_birds <- align(tr_table_birds, align="left", part="body")
  
  # add a title for the table, including abbreviated name of distance measure
  tr_table_birds <- add_header_row(tr_table_birds, 
                             top=TRUE, 
                             values=paste(print_name, 
                                          "Raw Bird Results", 
                                          separator=""),
                             colwidths=ncol(tr_wide))
  
  tr_table_birds <- autofit(tr_table_birds)
  
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
      geom_point(size=6) +
      ylim(ymin.unsm, ymax.unsm) +
      ggtitle("Unsmoothed") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,
                                      size = 16),
            plot.margin = margin(0.5,1,0.5,1, "cm"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 16),
            legend.title = element_blank(),
            legend.text = element_text(size = 16,
                                       margin = margin(t = 5, 
                                                       b = 5, 
                                                       unit = "pt")))
    
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
      geom_point(size=6) +
      ylim(ymin.sm, ymax.sm) +
      ggtitle("Smoothed") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,
                                      size = 16),
            plot.margin = margin(0.5,1,0.5,1, "cm"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 16),
            legend.title = element_blank(),
            legend.text = element_text(size = 16,
                                       margin = margin(t = 5, 
                                                       b = 5, 
                                                       unit = "pt")))
    
  } else 
  {
    
    # save blank plot
    p2.birds <- ggplot() +
      theme_void()
    
  }
  
  # arrange the saved plots into one
  p.all.birds <- ggarrange(p1.birds,
                           p2.birds,
                           ncol = 2,
                           nrow = 1,
                           legend = "right",
                           common.legend = TRUE)+
    theme(plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"))
  
  p.all.birds <- annotate_figure(p.all.birds,
                                 top = text_grob(paste("Wading Bird Rankings: ", 
                                                      print_name, 
                                                      sep = ""),
                                                size = 20,
                                                hjust = 0.7),
                                 left = text_grob("Dissimilarity Value",
                                                  rot = 90,
                                                  size = 18,
                                                  vjust = 1.3),
                                 bottom = text_grob("Species",
                                                    size = 18,
                                                    vjust = 0,
                                                    hjust = 1.5))
  
  # write results to files ----

  # create directories if needed
  if(!dir.exists("plots/")) {dir.create("plots/")}
  if(!dir.exists("tables/")) {dir.create("tables/")}
  if(!dir.exists("plots/bird_results/")) {dir.create("plots/bird_results/")}
  if(!dir.exists("tables/bird_results/")) {dir.create("tables/bird_results/")}
  
  # write the grid-arranged plot as a jpg image
  ggsave(filename = paste(print_name, "_plot.tiff", sep=""), 
         path = "plots/bird_results/", 
         plot = p.all.birds,
         width = 10000,
         height = 6000,
         units = "px",
         device = "tiff",
         dpi = 1000,
         compression = "lzw")
  
  # write the flextable as a Word document
  save_as_docx(tr_table_birds, 
               path = paste("tables/bird_results/", print_name, "_table.docx", sep=""))
  
  # send the flextable object to the main R environment  
  return(tr_table_birds)
  
}
