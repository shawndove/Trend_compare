# Test distance measures and compile the raw results in a Word document
# Shawn Dove (s.dove@ucl.ac.uk) - November, 2020

# Packages ----

library(officer)
library(TSdist)
#library(philentropy)
#library(statip)

# set working directory
# this is where the Word document will be stored
setwd("c:/R_projects/Trend_compare")

# Define distance measures ----

# list of functions of all distance measures to be tested
dist.fnlist <- list(EuclideanDistance,
                    ManhattanDistance,
                    InfNormDistance,
                    MinkowskiDistance,
                    DissimDistance,
                    CIDDistance,
                    FrechetDistance,
                    DTWDistance,
                    TAMDistance,
                    philentropy::sorensen,
                    NCDDistance,
                    CDMDistance,
                    ERPDistance,
                    LCSSDistance,
                    EDRDistance,
                    FourierDistance,
                    ACFDistance,
                    PACFDistance,
                    PerDistance,
                    IntPerDistance,
                    ARPicDistance,
                    PDCDistance,
                    MindistSaxDistance,
                    STSDistance,
                    CCorDistance,
                    CortDistance,
                    SpecGLKDistance,
                    SpecLLRDistance,
                    statip::hellinger,
                    devdist)

# names to use for each distance measure in test results output
# must be short or table and graph titles will be too long
dist.nameslist <- c("Euclidean",
                    "Manhattan",
                    "Chebyshev",
                    "Minkowski",
                    "Dissim",
                    "CID",
                    "Frechet",
                    "DTW",
                    "TAM",
                    "Sorensen",
                    "NCD",
                    "CDM",
                    "ERP",
                    "LCSS",
                    "EDR",
                    "Fourier",
                    "ACF",
                    "PACF",
                    "Per",
                    "IntPer",
                    "Piccolo",
                    "PDC",
                    "SAX",
                    "STS",
                    "CCor",
                    "Cort",
                    "GLK",
                    "LLR",
                    "Hellinger",
                    "MyDist")

# extra arguments to apply to distance measures
# to apply multiple arguments to one measure, enclose with c()
dist.argslist <- list("NULL",
                   "NULL",
                   "NULL",
                   p=3,
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   testNA=FALSE,
                   "NULL",
                   "NULL",
                   g=0,
                   epsilon=0.1,
                   epsilon=0.1,
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   w=20,
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   "NULL",
                   alpha=10)

# Produce test results ----

# create a list to temporarily hold results tables
temp_table <- list()

# create a docx object to write results to
temp_doc <- read_docx()

# loop to call testing function on each distance measure and write
# plots and tables of all results to the above docx object
for(i in seq_along(dist.fnlist)) {
  
  # call testing function and store table of raw results in temp list
  # the testing function also generates plots and tables for each
  # distance measure, which are stored as jpg images and Word documents respectively
  temp_table[[i]] <- get.dm.result(dist.fun=dist.fnlist[[i]], 
                                   dist.nameslist[i], 
                                   dist.argslist[i])
  
  # add table to Word document
  body_add_flextable(temp_doc, 
                     temp_table[[i]])
  
  # add a blank space after the table
  body_add_par(temp_doc, "")

  # variable to store location of image that contains plots of test results
  img.file <- paste("plots/test_results/", 
                    dist.nameslist[i], 
                    "_plots.jpg", 
                    sep="")
  
  # add plot to Word document
  body_add_img(temp_doc, 
               src=img.file, 
               height = 5, 
               width = 6)
  
  # add a page break after the plot
  body_add_break(temp_doc)
  
}

# write docx object to a Word file
print(temp_doc,
      target="All_results.docx")

