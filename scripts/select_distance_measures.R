# Load libraries ----

library(TSdist)
library(philentropy)

# Define distance measures ----

# list of functions of all distance measures to be tested
dist.fnlist1 <- list(EuclideanDistance,
                     ManhattanDistance,
                     InfNormDistance,
                     CIDDistance,
                     DTWDistance,
                     TAMDistance,
                     NCDDistance,
                     CDMDistance,
                     ERPDistance,
                     EDRDistance,
                     FourierDistance,
                     ACFDistance,
                     PACFDistance,
                     PerDistance,
                     IntPerDistance,
                     ARPicDistance,
                     STSDistance,
                     CortDistance
)

dist.fnlist2 <- list(additive_symm_chi_sq,
                     avg,
                     canberra,
                     clark_sq,
                     czekanowski,
                     dice_dist,
                     divergence_sq,
                     gower,
                     jaccard,
                     jeffreys,
                     jensen_difference,
                     kulczynski_d,
                     kullback_leibler_distance,
                     kumar_johnson,
                     k_divergence,
                     lorentzian,
                     prob_symm_chi_sq,
                     soergel,
                     squared_chi_sq,
                     squared_chord,
                     squared_euclidean,
                     taneja,
                     topsoe,
                     wave_hedges
)

# names to use for each distance measure in test results output
# must be short or table and graph titles will be too long
dist.nameslist1 <- c("Euclidean",
                     "Manhattan",
                     "Chebyshev",
                     "CID",
                     "DTW",
                     "TAM",
                     "NCD",
                     "CDM",
                     "ERP",
                     "EDR",
                     "Fourier",
                     "ACF",
                     "PACF",
                     "Per",
                     "IntPer",
                     "Piccolo",
                     "STS",
                     "Cort"
)

dist.nameslist2 <- c("Additive",
                     "AVG",
                     "Canb",
                     "Clark",
                     "Czek",
                     "Dice",
                     "Diverge",
                     "Gower",
                     "Jaccard",
                     "Jeffreys",
                     "Jensen",
                     "Kulcz",
                     "Kullback",
                     "KumarJohnson",
                     "KDiv",
                     "Lorentz",
                     "ProbSymm",
                     "Soergel",
                     "SqChi",
                     "SqChord",
                     "SqEuclid",
                     "Taneja",
                     "Topsoe",
                     "WaveHedges"
)

# extra arguments to apply to distance measures
# all non-null arguments must be enclosed with list()
dist.argslist1 <- list(NULL, #Euclidean
                       NULL, #Manhattan
                       NULL, #Chebyshev
                       NULL, #CID
                       NULL, #DTW
                       NULL, #TAM
                       NULL, #NCD
                       NULL, #CDM
                       list(g=0), #ERP
                       list(epsilon=0.1), #EDR
                       NULL, #Fourier
                       NULL, #ACF
                       NULL, #PACF
                       NULL, #Per
                       NULL, #IntPer
                       NULL, #Piccolo
                       NULL, #STS
                       NULL #Cort
)

dist.argslist2 <- list(list(testNA=FALSE), #Additive
                       list(testNA=FALSE), #AVG
                       list(testNA=FALSE), #Canb
                       list(testNA=FALSE), #Clark
                       list(testNA=FALSE), #Czek
                       list(testNA=FALSE), #Dice
                       list(testNA=FALSE), #Diverge
                       list(testNA=FALSE), #Gower
                       list(testNA=FALSE), #Jaccard
                       list(testNA=FALSE, unit="log"), #Jeffreys
                       list(testNA=FALSE, unit="log"), #Jensen
                       list(testNA=FALSE), #Kulcz
                       list(testNA=FALSE, unit="log"), #Kullback
                       list(testNA=FALSE), #KumarJohson
                       list(testNA=FALSE, unit="log"), #KDiv
                       list(testNA=FALSE, unit="log"), #Lorentz
                       list(testNA=FALSE), #ProbSymm
                       list(testNA=FALSE), #Soergel
                       list(testNA=FALSE), #SqChi
                       list(testNA=FALSE), #SqChord
                       list(testNA=FALSE), #SqEuclid
                       list(testNA=FALSE, unit="log"), #Taneja
                       list(testNA=FALSE, unit="log"), #Topsoe
                       list(testNA=FALSE) #WaveHedges
)