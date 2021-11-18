library(TSdist)
library(philentropy)

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