# Test distance measures and compile the raw results in a Word document
# Shawn Dove (s.dove@ucl.ac.uk) - November, 2020

# Packages ----

library(officer)
library(TSdist)
library(philentropy)
library(flextable)
library(ggplot2)
library(ggpubr)
#library(statip)

# set working directory
# this is where the Word document will be stored
setwd("c:/R_projects/Trend_compare")

# Define distance measures ----

# list of functions of all distance measures to be tested
dist.fnlist1 <- list(EuclideanDistance,
                    ManhattanDistance,
                    InfNormDistance,
                   # MinkowskiDistance,
                   # DissimDistance,
                    CIDDistance,
                   # FrechetDistance,
                    DTWDistance,
                    TAMDistance,
                    NCDDistance,
                    CDMDistance,
                    ERPDistance,
                   # LCSSDistance,
                    EDRDistance,
                    FourierDistance,
                    ACFDistance,
                    PACFDistance,
                    PerDistance,
                    IntPerDistance,
                    ARPicDistance,
                   # PDCDistance,
                   # MindistSaxDistance,
                    STSDistance,
                   # CCorDistance,
                    CortDistance#,
                   # SpecGLKDistance,
                   # SpecLLRDistance,
                   # sddist
                   )

dist.fnlist2 <- list(additive_symm_chi_sq,
                    avg,
                   # bhattacharyya,
                    canberra,
                    clark_sq,
                  #  cosine_dist,
                    czekanowski,
                    dice_dist,
                    divergence_sq,
                  #  fidelity,
                    gower,
                  #  harmonic_mean_dist,
                  #  inner_product,
                  #  intersection_dist,
                    jaccard,
                    jeffreys,
                    jensen_difference,
                  #  jensen_shannon,
                    kulczynski_d,
                    kullback_leibler_distance,
                  #  kumar_hassebrook,
                    kumar_johnson,
                    k_divergence,
                    lorentzian,
                  #  matusita,
                  #  motyka,
                  #  neyman_chi_sq,
                  #  pearson_chi_sq,
                    prob_symm_chi_sq,
                  #  ruzicka,
                    soergel,
                    squared_chi_sq,
                    squared_chord,
                    squared_euclidean,
                    taneja,
                  #  tanimoto,
                    topsoe,
                    wave_hedges)

# names to use for each distance measure in test results output
# must be short or table and graph titles will be too long
dist.nameslist1 <- c("Euclidean",
                    "Manhattan",
                    "Chebyshev",
                   # "Minkowski",
                   # "Dissim",
                    "CID",
                   # "Frechet",
                    "DTW",
                    "TAM",
                    "NCD",
                    "CDM",
                    "ERP",
                   # "LCSS",
                    "EDR",
                    "Fourier",
                    "ACF",
                    "PACF",
                    "Per",
                    "IntPer",
                    "Piccolo",
                   # "PDC",
                   # "SAX",
                    "STS",
                   # "CCor",
                    "Cort"#,
                    #"GLK",
                    #"LLR",
                    #"SDD"
                   )

dist.nameslist2 <- c("Additive",
                    "AVG",
                   # "Bhat",
                    "Canb",
                    "Clark",
                   # "Cosine",
                    "Czek",
                    "Dice",
                    "Diverge",
                   # "Fidelity",
                    "Gower",
                   # "Harm",
                   # "InnerP",
                   # "Intersect",
                    "Jaccard",
                    "Jeffreys",
                    "Jensen",
                   # "JenShan",
                    "Kulcz",
                    "Kullback",
                   # "KumarHass",
                    "KumarJohnson",
                    "KDiv",
                    "Lorentz",
                   # "Matusita",
                   # "Motyka",
                   # "Neyman",
                   # "Pearson",
                    "ProbSymm",
                   # "Ruzicka",
                    "Soergel",
                    "SqChi",
                    "SqChord",
                    "SqEuclid",
                    "Taneja",
                   # "Tanimoto",
                    "Topsoe",
                    "WaveHedges")

# extra arguments to apply to distance measures
# all non-null arguments must be enclosed with list()
dist.argslist1 <- list("NULL", #Euclidean
                   "NULL", #Manhattan
                   "NULL", #Chebyshev
                  # list(p=3), #Minkowski
                  # "NULL", #Dissim
                   "NULL", #CID
                   "NULL", #list(window.type="sakoechiba", window.size=60), #DTW
                   "NULL", #TAM
                   "NULL", #NCD
                   "NULL", #CDM
                   list(g=0), #ERP
                  # list(epsilon=0.1), #LCSS
                   list(epsilon=0), #EDR
                   "NULL", #Fourier
                   "NULL", #ACF
                   "NULL", #PACF
                   "NULL", #Per
                   "NULL", #IntPer
                   "NULL", #Piccolo
                  # "NULL", #PDC
                  # list(w=5), #SAX
                   "NULL", #STS
                  # "NULL", #CCor
                   "NULL"#, #Cort
                   #"NULL", #GLK
                   #"NULL", #LLR
                   #"NULL", #Hellinger
                   #list(alpha=10) #SDD
                  )
                  
dist.argslist2 <- list(list(testNA=FALSE), #Additive
                   list(testNA=FALSE), #AVG
                  # list(testNA=FALSE, unit="log"), #Bhat
                   list(testNA=FALSE), #Canb
                   list(testNA=FALSE), #Clark
                  # list(testNA=FALSE), #Cosine
                   list(testNA=FALSE), #Czek
                   list(testNA=FALSE), #Dice
                   list(testNA=FALSE), #Diverge
                  # list(testNA=FALSE), #Fidelity
                   list(testNA=FALSE), #Gower
                  # list(testNA=FALSE), #Harm
                  # list(testNA=FALSE), #InnerP
                  # list(testNA=FALSE), #Intersect
                   list(testNA=FALSE), #Jaccard
                   list(testNA=FALSE, unit="log"), #Jeffreys
                   list(testNA=FALSE, unit="log"), #Jensen
                  # list(testNA=FALSE, unit="log"), #JenShan
                   list(testNA=FALSE), #Kulcz
                   list(testNA=FALSE, unit="log"), #Kullback
                  # list(testNA=FALSE), #KumarHass
                   list(testNA=FALSE), #KumarJohson
                   list(testNA=FALSE, unit="log"), #KDiv
                   list(testNA=FALSE, unit="log"), #Lorentz
                  # list(testNA=FALSE), #Matusita
                  # list(testNA=FALSE), #Motyka
                  # list(testNA=FALSE), #Neyman
                  # list(testNA=FALSE), #Pearson
                   list(testNA=FALSE), #ProbSymm
                  # list(testNA=FALSE), #Ruzicka
                   list(testNA=FALSE), #Soergel
                   list(testNA=FALSE), #SqChi
                   list(testNA=FALSE), #SqChord
                   list(testNA=FALSE), #SqEuclid
                   list(testNA=FALSE, unit="log"), #Taneja
                  # list(testNA=FALSE), #Tanimoto
                   list(testNA=FALSE, unit="log"), #Topsoe
                   list(testNA=FALSE)) #WaveHedges

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
                               dist.argslist1[[i]],
                               "x",
                               "y")
}
for(i in seq_along(dist.fnlist2)) {
  ti_table2[[i]] <- ti.test.fn1(dist.fun=dist.fnlist2[[i]], 
                                dist.nameslist2[i], 
                                dist.argslist2[[i]],
                                "P",
                                "Q")
}

# test for triangle inequality and non-negative value handling
# this function does not generate negative values
for(i in seq_along(dist.fnlist1)) {

  ti_nn_table[[i]] <- ti.test.nn.fn1(dist.fun=dist.fnlist1[[i]], 
                               dist.nameslist1[i], 
                               dist.argslist1[[i]],
                               "x",
                               "y")
}
for(i in seq_along(dist.fnlist2)) {
  ti_nn_table2[[i]] <- ti.test.nn.fn1(dist.fun=dist.fnlist2[[i]], 
                                dist.nameslist2[i], 
                                dist.argslist2[[i]],
                                "P",
                                "Q")
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
                                   tsy.list,
                                   "x",
                                   "y")
  
  # test for triangle inequality
#  ti_table[[i]] <- ti.test.fn1(dist.fun=dist.fnlist1[[i]], 
#                dist.nameslist1[i], 
#                dist.argslist1[[i]])
  
  # repeat for bird data
#  temp_table_birds[[i]] <- get.dm.result.birds1(dist.fun=dist.fnlist1[[i]], 
#                                               dist.nameslist1[i], 
#                                               dist.argslist1[[i]])
  
  # add test results table to Word document
  #body_add_flextable(temp_doc, 
  #                   temp_table[[i]])
  
  # add a blank space after the test results table
  #body_add_par(temp_doc, "")

  # variable to store location of image that contains plots of test results
  #img.file.test <- paste("plots/test_results_2/", 
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
  #img.file.bird <- paste("plots/bird_results/2/", 
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

  temp_table[[i]] <- get.dm.result1(dist.fun=dist.fnlist2[[i]], 
                                    dist.nameslist2[i], 
                                    dist.argslist2[[i]],
                                    tsx.list,
                                    tsy.list,
                                    "P",
                                    "Q")
  
  # test for triangle inequality
#    ti_table[[i]] <- ti.test.fn2(dist.fun=dist.fnlist2[[i]], 
#                  dist.nameslist2[i], 
#                  dist.argslist2[[i]])

  # repeat for bird data
#  temp_table_birds[[i]] <- get.dm.result.birds2(dist.fun=dist.fnlist2[[i]], 
#                                               dist.nameslist2[i], 
#                                               dist.argslist2[[i]])
  
  # add test results table to Word document
  #body_add_flextable(temp_doc, 
  #                   temp_table[[i]])
  
  # add a blank space after the test results table
  #body_add_par(temp_doc, "")
  
  # variable to store location of image that contains plots of test results
  #img.file.test <- paste("plots/test_results_2/", 
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
  #img.file.bird <- paste("plots/bird_results/2/", 
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
      target="Results_tables_1.docx")



#--

library(TSdist)
library(philentropy)

dist.fnlist.usplots2 <- list(k_divergence, kullback_leibler_distance)
dist.nameslist.usplots2 <- c("KDiv", "Kullback")
dist.argslist.usplots2 <- list(list(testNA=FALSE, unit="log"), list(testNA=FALSE, unit="log"))

unsmoothed.plots2 <- list()
for (i in seq_along(dist.fnlist.usplots2)) {
  
  unsmoothed.plots2[[i]] <- get.dm.plot.birds2(dist.fnlist.usplots2[[i]], dist.nameslist.usplots2[i], dist.argslist.usplots2[[i]])[[1]]
  
}

sj_results <- data.frame("x" = c(3,1,4,2,5), "Percent Improvement" = c(108, 57, 36, 33, 0), "T-Test" = c(9.4, 7.6, 4.0, 5.4, -0.3), "Species" = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"))

sj_results$Species <- factor(sj_results$Species, levels = c("Redshank", "Lapwing", "Snipe", "Curlew", "Yellow Wagtail"))

ymin.pi <- min(sj_results$Percent.Improvement) - 0.1*min(abs(sj_results$Percent.Improvement))

ymax.pi <- max(sj_results$Percent.Improvement) + 0.1*max(abs(sj_results$Percent.Improvement))

ymin.tt <- min(sj_results$T.Test) - 0.1*min(abs(sj_results$T.Test))

ymax.tt <- max(sj_results$T.Test) + 0.1*max(abs(sj_results$T.Test))

pi.birdplot <- ggplot(sj_results,
                   aes(x = x, 
                       y = Percent.Improvement,
                       colour = Species,
                       shape = Species)) +
  geom_point(size=4) +
  ylim(ymin.pi, ymax.pi) +
  xlim(0, 6) +
  theme(legend.title = element_text(color="blue")) +
  ggtitle(paste("% Improvement")) +
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

tt.birdplot <- ggplot(sj_results,
                      aes(x = x, 
                          y = T.Test,
                          colour = Species,
                          shape = Species)) +
  geom_point(size=4) +
  ylim(ymin.tt, ymax.tt) +
  xlim(0, 6) +
  theme(legend.title = element_text(color="blue")) +
  ggtitle(paste("T-Test")) +
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

unsmoothed.plots2[[3]] <- pi.birdplot
unsmoothed.plots2[[4]] <- tt.birdplot

gg_unsmoothed <- ggarrange(unsmoothed.plots2[[1]], unsmoothed.plots2[[2]], 
                           unsmoothed.plots2[[3]], unsmoothed.plots2[[4]],
                           ncol = 4, nrow = 1, legend=c("right"), common.legend = TRUE)
gg_unsmoothed <- annotate_figure(gg_unsmoothed, left = "Distance Value")
ggsave(paste(wd, "/figures/unsmoothed_plots.tiff", sep=""), gg_unsmoothed, width=7480, height=2494, units="px", dpi=1000, scale=1)

## Smoothed bird result plots --

dist.fnlist.plots <- list(EuclideanDistance, ERPDistance, ManhattanDistance)
dist.nameslist.plots <- c("Euclidean", "ERP", "Manhattan")
dist.argslist.plots <- list("NULL", list(g=0), "NULL")

dist.fnlist.plots2 <- list(avg, gower, lorentzian, squared_euclidean)
dist.nameslist.plots2 <- c("AVG", "Gower", "Lorentz", "Sq. Euclid")
dist.argslist.plots2 <- list(list(testNA=FALSE), list(testNA=FALSE), list(testNA=FALSE, unit="log"), list(testNA=FALSE))

smoothed.plots <- list()
smoothed.plots2 <- list()
for (i in seq_along(dist.fnlist.plots)) {
  
  smoothed.plots[[i]] <- get.dm.plot.birds1(dist.fnlist.plots[[i]], dist.nameslist.plots[i], dist.argslist.plots[[i]])[[2]]
  
}
for (i in seq_along(dist.fnlist.plots2)) {
  
  smoothed.plots2[[i]] <- get.dm.plot.birds2(dist.fnlist.plots2[[i]], dist.nameslist.plots2[i], dist.argslist.plots2[[i]])[[2]]

}

smoothed.plots[[4]] <- pi.birdplot
#smoothed.plots2[[5]] <- tt.birdplot

library(ggpubr)

gg_smoothed <- ggarrange(smoothed.plots[[1]], smoothed.plots[[2]], smoothed.plots[[3]], smoothed.plots[[4]], 
                         smoothed.plots2[[1]], smoothed.plots2[[2]], smoothed.plots2[[3]], smoothed.plots2[[4]],
                         ncol = 4, nrow = 2, legend=c("right"), common.legend = TRUE)
gg_smoothed <- annotate_figure(gg_smoothed, left = "Distance Value")
ggsave(paste(wd, "/figures/smoothed_plots.tiff", sep=""), gg_smoothed, width=7480, height=4986, unit="px", dpi=1000, scale=1)

