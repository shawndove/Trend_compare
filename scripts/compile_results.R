# Test distance measures and compile the results in a Word document

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
# warnings about negative values being adjusted can be ignored
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

# loop to call testing function on each distance measure and write
# plots and tables of all results to the above docx object
for(i in seq_along(c(dist.fnlist1, dist.fnlist2))) {

  # call testing function and store table of raw results in temp list
  # the testing function also generates plots and tables for each
  # distance measure, which are stored as jpg images and Word documents respectively
  temp_table[[i]] <- get.dm.result1(dist.fun=c(dist.fnlist1, dist.fnlist2)[[i]], 
                                   c(dist.nameslist1, dist.nameslist2)[i], 
                                   c(dist.argslist1, dist.argslist2)[[i]],
                                   tsx.list,
                                   tsy.list)
  
  # repeat for bird data
  temp_table_birds[[i]] <- get.dm.result.birds1(dist.fun=c(dist.fnlist1, dist.fnlist2)[[i]], 
                                               c(dist.nameslist1, dist.nameslist2)[i], 
                                               c(dist.argslist1, dist.argslist2)[[i]])
  
}

# combine lists of distance measures names
dist.names <- c(dist.nameslist1, dist.nameslist2)

# sort alphabetically
dist.names <- sort(dist.names)

# create a docx object to write results to
temp_doc <- read_docx()

# add a blank space after the test results table
body_add_par(temp_doc, "")

# create list of distance measures that gave results for warping and time scaling
height8_list <- c("DTW", 
                  "TAM", 
                  "ERP", 
                  "EDR", 
                  "Piccolo", 
                  "NCD", 
                  "CDM")

# add controlled results plots to document
for (i in seq_along(dist.names)) {

  # variable to store location of image that contains plots of test results
  img.file.test <- paste("plots/controlled_results/", 
                         dist.names[i], 
                         "_plots.tiff", 
                         sep="")
  
  # add blank space before the plot to center
  body_add_par(temp_doc, "")
  body_add_par(temp_doc, "")
  body_add_par(temp_doc, "")
  body_add_par(temp_doc, "")
  
  if (dist.names[i] %in% height8_list) {
    
    # add plot to Word document
    body_add_img(temp_doc, 
                 src=img.file.test,
                 width = 6,
                 height = 8)
    
  } else {
    
    # add plot to Word document
    body_add_img(temp_doc, 
                 src=img.file.test,
                 width = 6,
                 height = 6)
    
  }
  
  # add a page break after the plot
  body_add_break(temp_doc)
  
}

# create number sequence that will arrange tables in alphabetical order
temp_seq <- c(12, 19, 16, 20, 21, 8, 4, 22, 18, 23, 24, 25, 5, 10, 9, 1, 11, 
              26, 3, 15, 27, 28, 29, 30, 31, 32, 33, 34, 2, 7, 13, 14, 35, 36, 
              37, 38, 39, 17, 6, 40, 41, 42)

# add controlled results tables to document
for (i in temp_seq) {
  
  # add blank space before the test results table
  body_add_par(temp_doc, "")
  body_add_par(temp_doc, "")
  body_add_par(temp_doc, "")
  body_add_par(temp_doc, "")
  
  # add test results table to Word document
  body_add_flextable(temp_doc, 
                     temp_table[[i]])
  
  # add a page break after the table
  body_add_break(temp_doc)
}

# add bird results plots to document
for (i in seq_along(dist.names)) {
  
  # variable to store location of image that contains plots of test results
  img.file.test <- paste("plots/bird_results/", 
                         dist.names[i], 
                         "_plot.tiff", 
                         sep="")
  
  # add blank space before the plot to center
  body_add_par(temp_doc, "")
  body_add_par(temp_doc, "")
  
  # add plot to Word document
  body_add_img(temp_doc, 
               src=img.file.test,
               width = 6,
               height = 3.6)
  
  # add blank space after the plot
  body_add_par(temp_doc, "")
  body_add_par(temp_doc, "")
  
  # add a page break after every second plot
  if ((i %% 2) == 0) {body_add_break(temp_doc)}
  
}

# create directory for output if needed
if(!dir.exists("docs/")) {dir.create("docs/")}

# write docx object to a Word file
print(temp_doc,
      target="docs/controlled_and_bird_results.docx")

