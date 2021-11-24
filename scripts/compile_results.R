# Test distance measures and compile the raw results in a Word document
# Shawn Dove (s.dove@ucl.ac.uk) - November, 2020

# Packages ----

library(officer)
library(TSdist)
library(philentropy)
library(flextable)
library(ggplot2)
library(ggpubr)

# Load distance measures ----

source("scripts/select_distance_measures.R")

# Load controlled time series ----

source("scripts/ts_controlled.R")

# Load bird trends ----

source("scripts/bird_trends.R")

# Load functions ----

source("scripts/triangle_inequality_test.R")
source("scripts/triangle_inequality_test_nnonly.R")
source("scripts/get.dm.result.R")
source("scripts/get.dm.result.birds.R")
source("scripts/get.dm.plots.birds.R")

# Produce test results ----

ti_table <- list()
ti_table2 <- list()
ti_nn_table <- list()
ti_nn_table2 <- list()

# loop to call testing function on each distance measure and write
# plots and tables of all results to the above docx object

# test for triangle inequality and non-negative value handling
# this function generates negative values
for(i in seq_along(dist.fnlist1)) {
  
  ti_table[[i]] <- ti.test.fn1(dist.fun=dist.fnlist1[[i]], 
                               dist.nameslist1[i], 
                               dist.argslist1[[i]])
  
}

for(i in seq_along(dist.fnlist2)) {
  
  ti_table2[[i]] <- ti.test.fn1(dist.fun=dist.fnlist2[[i]], 
                                dist.nameslist2[i], 
                                dist.argslist2[[i]])
  
}

# test for triangle inequality and non-negative value handling
# this function does not generate negative values
for(i in seq_along(dist.fnlist1)) {

  ti_nn_table[[i]] <- ti.test.nn.fn1(dist.fun=dist.fnlist1[[i]], 
                               dist.nameslist1[i], 
                               dist.argslist1[[i]])
  
}
for(i in seq_along(dist.fnlist2)) {
  
  ti_nn_table2[[i]] <- ti.test.nn.fn1(dist.fun=dist.fnlist2[[i]], 
                                dist.nameslist2[i], 
                                dist.argslist2[[i]])
  
}

# create lists to temporarily hold results tables
temp_table <- list()
temp_table_birds <- list()

# create a docx object to write results to
temp_doc <- read_docx()


# loop to call testing function on each distance measure and write
# plots and tables of all results to the above docx object
for(i in seq_along(dist.fnlist1)) {

  # call testing function and store table of raw results in temp list
  # the testing function also generates plots and tables for each
  # distance measure, which are stored as jpg images and Word documents respectively
  temp_table[[i]] <- get.dm.result1(dist.fun=dist.fnlist1[[i]], 
                                   dist.nameslist1[i], 
                                   dist.argslist1[[i]],
                                   tsx.list,
                                   tsy.list)
  
  # repeat for bird data
  temp_table_birds[[i]] <- get.dm.result.birds1(dist.fun=dist.fnlist1[[i]], 
                                               dist.nameslist1[i], 
                                               dist.argslist1[[i]])
  
  # add test results table to Word document
  #body_add_flextable(temp_doc, 
  #                   temp_table[[i]])
  
  # add a blank space after the test results table
  #body_add_par(temp_doc, "")

  # variable to store location of image that contains plots of test results
  #img.file.test <- paste("plots/test_results/", 
  #                  dist.nameslist1[i], 
   #                 "_plots.jpg", 
   #                 sep="")
  
  # add plot to Word document
  #body_add_img(temp_doc, 
  #             src=img.file.test, 
  #             height = 5, 
  #             width = 6)
  
  # add a page break after the plot
  #body_add_break(temp_doc)
  
  # add birds results table to Word document
  #body_add_flextable(temp_doc, 
  #                   temp_table_birds[[i]])
  
  # add a blank space after the birds results table
  #body_add_par(temp_doc, "")
  
  # variable to store location of image that contains plots of birds results
  #img.file.bird <- paste("plots/bird_results", 
  #                  dist.nameslist1[i], 
  #                  "_plots_fixed.jpg", 
  #                  sep="")
  
  # add plot to Word document
  #body_add_img(temp_doc, 
  #             src=img.file.bird, 
  #             height = 5, 
  #             width = 6)
  
  # add a page break after the plot
  #body_add_break(temp_doc)
  
}

for(i in seq_along(dist.fnlist2)) {
  
  # call testing function and store table of raw results in temp list
  # the testing function also generates plots and tables for each
  # distance measure, which are stored as jpg images and Word documents respectively

#  temp_table[[i]] <- get.dm.result1(dist.fun=dist.fnlist2[[i]], 
#                                    dist.nameslist2[i], 
#                                    dist.argslist2[[i]],
#                                    tsx.list,
#                                    tsy.list)

  # repeat for bird data
  temp_table_birds[[i]] <- get.dm.result.birds1(dist.fun=dist.fnlist2[[i]], 
                                               dist.nameslist2[i], 
                                               dist.argslist2[[i]])
  
  # add test results table to Word document
  #body_add_flextable(temp_doc, 
  #                   temp_table[[i]])
  
  # add a blank space after the test results table
  #body_add_par(temp_doc, "")
  
  # variable to store location of image that contains plots of test results
  #img.file.test <- paste("plots/controlled_results/", 
  #                  dist.nameslist2[i], 
  #                 "_plots.jpg", 
  #                 sep="")
  
  # add plot to Word document
  #body_add_img(temp_doc, 
  #             src=img.file.test, 
  #             height = 5, 
  #             width = 6)
  
  # add a page break after the plot
  #body_add_break(temp_doc)
  
  # add birds results table to Word document
  #body_add_flextable(temp_doc, 
  #                   temp_table_birds[[i]])
  
  # add a blank space after the birds results table
  #body_add_par(temp_doc, "")
  
  # variable to store location of image that contains plots of birds results
  #img.file.bird <- paste("plots/bird_results/", 
  #                  dist.nameslist2[i], 
  #                  "_plots_fixed.jpg", 
  #                  sep="")
  
  # add plot to Word document
  #body_add_img(temp_doc, 
  #             src=img.file.bird, 
  #             height = 5, 
  #             width = 6)
  
  # add a page break after the plot
  #body_add_break(temp_doc)
  
}

# write docx object to a Word file
#print(temp_doc,
#      target="All_results_fixed_4.docx")

tableslist <- create.results.table(c(dist.nameslist1,dist.nameslist2))
tableslist[[1]] <- theme_zebra(tableslist[[1]])
tableslist[[2]] <- theme_zebra(tableslist[[2]])
tableslist[[3]] <- theme_zebra(tableslist[[3]])

temp_doc <- read_docx()
body_add_flextable(temp_doc, 
                   tableslist[[1]])
body_add_break(temp_doc)
body_add_flextable(temp_doc, 
                   tableslist[[2]])
body_add_break(temp_doc)
body_add_flextable(temp_doc, 
                   tableslist[[3]])
body_add_break(temp_doc)
print(temp_doc,
      target="tables/Results_tables_1.docx")

