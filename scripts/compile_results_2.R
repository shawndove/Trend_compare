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
                   # FrechetDistance,
                    DTWDistance,
                    TAMDistance,
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
                    #statip::hellinger,
                    sdddist,
                    philentropy::additive_symm_chi_sq,
                    philentropy::avg,
                    philentropy::bhattacharyya,
                    philentropy::canberra,
                    philentropy::clark_sq,
                    philentropy::cosine_dist,
                    philentropy::czekanowski,
                    philentropy::dice_dist,
                    philentropy::divergence_sq,
                    philentropy::fidelity,
                    philentropy::gower,
                    philentropy::harmonic_mean_dist,
                    philentropy::inner_product,
                    philentropy::intersection_dist,
                    philentropy::jaccard,
                    philentropy::jeffreys,
                    philentropy::jensen_difference,
                    philentropy::jensen_shannon,
                    philentropy::kulczynski_d,
                    philentropy::kullback_leibler_distance,
                    philentropy::kumar_hassebrook,
                    philentropy::kumar_johnson,
                    philentropy::k_divergence,
                    philentropy::lorentzian,
                    philentropy::matusita,
                    philentropy::motyka,
                    philentropy::neyman_chi_sq,
                    philentropy::pearson_chi_sq,
                    philentropy::prob_symm_chi_sq,
                    philentropy::ruzicka,
                    philentropy::soergel,
                    philentropy::squared_chi_sq,
                    philentropy::squared_chord,
                    philentropy::squared_euclidean,
                    philentropy::taneja,
                    philentropy::tanimoto,
                    philentropy::topsoe,
                    philentropy::wave_hedges)

# names to use for each distance measure in test results output
# must be short or table and graph titles will be too long
dist.nameslist <- c("Euclidean",
                    "Manhattan",
                    "Chebyshev",
                    "Minkowski",
                    "Dissim",
                    "CID",
                   # "Frechet",
                    "DTW",
                    "TAM",
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
                    #"Hellinger",
                    "SDD",
                    "Additive",
                    "AVG",
                    "Bhat",
                    "Canb",
                    "Clark",
                    "Cosine",
                    "Czek",
                    "Dice",
                    "Diverge",
                    "Fidelity",
                    "Gower",
                    "Harm",
                    "InnerP",
                    "Intersect",
                    "Jaccard",
                    "Jeffreys",
                    "Jensen",
                    "JenShan",
                    "Kulcz",
                    "Kullback",
                    "KumarHass",
                    "KumarJohnson",
                    "KDiv",
                    "Lorentz",
                    "Matusita",
                    "Motyka",
                    "Neyman",
                    "Pearson",
                    "ProbSymm",
                    "Ruzicka",
                    "Soergel",
                    "SqChi",
                    "SqChord",
                    "SqEuclid",
                    "Taneja",
                    "Tanimoto",
                    "Topsoe",
                    "WaveHedges")

# extra arguments to apply to distance measures
# all non-null arguments must be enclosed with list()
dist.argslist <- list("NULL", #Euclidean
                   "NULL", #Manhattan
                   "NULL", #Chebyshev
                   list(p=3), #Minkowski
                   "NULL", #Dissim
                   "NULL", #CID
                  # "NULL", #Frechet
                   "NULL", #DTW
                   "NULL", #TAM
                   "NULL", #NCD
                   "NULL", #CDM
                   list(g=0), #ERP
                   list(epsilon=0.1), #LCSS
                   list(epsilon=0.1), #EDR
                   "NULL", #Fourier
                   "NULL", #ACF
                   "NULL", #PACF
                   "NULL", #Per
                   "NULL", #IntPer
                   "NULL", #Piccolo
                   "NULL", #PDC
                   list(w=20), #SAX
                   "NULL", #STS
                   "NULL", #CCor
                   "NULL", #Cort
                   "NULL", #GLK
                   "NULL", #LLR
                   #"NULL", #Hellinger
                   list(alpha=10), #MyDist
                   list(testNA=FALSE), #Additive
                   list(testNA=FALSE), #AVG
                   list(testNA=FALSE, unit="log"), #Bhat
                   list(testNA=FALSE), #Canb
                   list(testNA=FALSE), #Clark
                   list(testNA=FALSE), #Cosine
                   list(testNA=FALSE), #Czek
                   list(testNA=FALSE), #Dice
                   list(testNA=FALSE), #Diverge
                   list(testNA=FALSE), #Fidelity
                   list(testNA=FALSE), #Gower
                   list(testNA=FALSE), #Harm
                   list(testNA=FALSE), #InnerP
                   list(testNA=FALSE), #Intersect
                   list(testNA=FALSE), #Jaccard
                   list(testNA=FALSE, unit="log"), #Jeffreys
                   list(testNA=FALSE, unit="log"), #Jensen
                   list(testNA=FALSE, unit="log"), #JenShan
                   list(testNA=FALSE), #Kulcz
                   list(testNA=FALSE, unit="log"), #Kullback
                   list(testNA=FALSE), #KumarHass
                   list(testNA=FALSE), #KumarJohson
                   list(testNA=FALSE, unit="log"), #KDiv
                   list(testNA=FALSE, unit="log"), #Lorentz
                   list(testNA=FALSE), #Matusita
                   list(testNA=FALSE), #Motyka
                   list(testNA=FALSE), #Neyman
                   list(testNA=FALSE), #Pearson
                   list(testNA=FALSE), #ProbSymm
                   list(testNA=FALSE), #Ruzicka
                   list(testNA=FALSE), #Soergel
                   list(testNA=FALSE), #SqChi
                   list(testNA=FALSE), #SqChord
                   list(testNA=FALSE), #SqEuclid
                   list(testNA=FALSE, unit="log"), #Taneja
                   list(testNA=FALSE), #Tanimoto
                   list(testNA=FALSE, unit="log"), #Topsoe
                   list(testNA=FALSE)) #WaveHedges

# Produce test results ----

# create lists to temporarily hold results tables
temp_table <- list()
temp_table_birds <- list()

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
                                   dist.argslist[[i]])
  
  # repeat for bird data
  temp_table_birds[[i]] <- get.dm.result.birds(dist.fun=dist.fnlist[[i]], 
                                               dist.nameslist[i], 
                                               dist.argslist[[i]])
  
  # add test results table to Word document
  body_add_flextable(temp_doc, 
                     temp_table[[i]])
  
  # add a blank space after the test results table
  body_add_par(temp_doc, "")

  # variable to store location of image that contains plots of test results
  img.file.test <- paste("plots/test_results/", 
                    dist.nameslist[i], 
                    "_plots.jpg", 
                    sep="")
  
  # add plot to Word document
  body_add_img(temp_doc, 
               src=img.file.test, 
               height = 5, 
               width = 6)
  
  # add a page break after the plot
  body_add_break(temp_doc)
  
  # add birds results table to Word document
  body_add_flextable(temp_doc, 
                     temp_table_birds[[i]])
  
  # add a blank space after the birds results table
  body_add_par(temp_doc, "")
  
  # variable to store location of image that contains plots of birds results
  img.file.bird <- paste("plots/bird_results/2/", 
                    dist.nameslist[i], 
                    "_plots_fixed.jpg", 
                    sep="")
  
  # add plot to Word document
  body_add_img(temp_doc, 
               src=img.file.bird, 
               height = 5, 
               width = 6)
  
  # add a page break after the plot
  body_add_break(temp_doc)
  
}

# write docx object to a Word file
print(temp_doc,
      target="All_results_fixed.docx")

