dist.results <- function(tslistx, tslisty) {
  
  library(TSdist)
  library(philentropy)
  library(statip)
  # run metrics on all time series pairs
  
  # loop to create list of time series pairs
  
  res.ACF <- vector()
  res.ARLP <- vector()
  res.ARM <- vector()
  res.ARM.p <- vector()
  res.ARP <- vector()
  res.CC <- vector()
  res.CDM <- vector()
  res.CID <- vector()
  res.Cor <- vector()
  res.Cort <- vector()
  res.Dissim <- vector()
  res.DTW <- vector()
  res.EDR <- vector()
  res.ERP <- vector()
  res.Euclid <- vector()
  res.Fourier <- vector()
  res.Frechet <- vector()
  res.Hellinger <- vector()
  res.Inf <- vector()
  res.Int <- vector()
  res.LBK <- vector()
  res.LCSS <- vector()
  res.LP <- vector()
  res.Man <- vector()
  res.Mindist <- vector()
  res.Minkow <- vector()
  res.mydist <- vector()
  res.NCD <- vector()
  res.PACF <- vector()
  res.PDC <- vector()
  res.Per <- vector()
  res.Pred <- vector()
  res.Sorensen <- vector()
  res.SpecGLK <- vector()
  res.SpecISD <- vector()
  res.SpecLLR <- vector()
  res.STS <- vector()
  res.TAM <- vector()
  res.Tquest <- vector()

  for (i in seq_along(tslisty)) {
    
    res.ACF[i] <- (ACFDistance(tslistx[[i]], tslisty[[i]]))
    res.ARLP[i] <- (ARLPCCepsDistance(tslistx[[i]], tslisty[[i]]))
    res.ARM[i] <- (ARMahDistance(tslistx[[i]], tslisty[[i]])$statistic)
    res.ARM.p[i] <- (ARMahDistance(tslistx[[i]], tslisty[[i]])$p_value)
    res.ARP[i] <- (ARPicDistance(tslistx[[i]], tslisty[[i]]))
    res.CC[i] <- (CCorDistance(tslistx[[i]], tslisty[[i]]))
    res.CDM[i] <- (CDMDistance(tslistx[[i]], tslisty[[i]]))
    res.CID[i] <- (CIDDistance(tslistx[[i]], tslisty[[i]]))
    res.Cor[i] <- (CorDistance(tslistx[[i]], tslisty[[i]]))
    res.Cort[i] <- (CortDistance(tslistx[[i]], tslisty[[i]]))
    res.Dissim[i] <- (DissimDistance(tslistx[[i]], tslisty[[i]]))
    res.DTW[i] <- (DTWDistance(tslistx[[i]], tslisty[[i]]))
    res.EDR[i] <- (EDRDistance(tslistx[[i]], tslisty[[i]], 0.1, 30))
    res.ERP[i] <- (ERPDistance(tslistx[[i]], tslisty[[i]], 0, 30))
    res.Euclid[i] <- (EuclideanDistance(tslistx[[i]], tslisty[[i]]))
    res.Fourier[i] <- (FourierDistance(tslistx[[i]], tslisty[[i]]))
    res.Frechet[i] <- (FrechetDistance(tslistx[[i]], tslisty[[i]]))
    res.Inf[i] <- (InfNormDistance(tslistx[[i]], tslisty[[i]]))
    res.Int[i] <- (IntPerDistance(tslistx[[i]], tslisty[[i]]))
    res.LBK[i] <- (LBKeoghDistance(tslistx[[i]], tslisty[[i]], w=3))
    res.LCSS[i] <- (LCSSDistance(tslistx[[i]], tslisty[[i]], 0.1, 30))
    res.LP[i] <- (LPDistance(tslistx[[i]], tslisty[[i]]))
    res.Man[i] <- (ManhattanDistance(tslistx[[i]], tslisty[[i]]))
    res.Mindist[i] <- (MindistSaxDistance(tslistx[[i]], tslisty[[i]], 20))
    res.Minkow[i] <- (MinkowskiDistance(tslistx[[i]], tslisty[[i]], 3))
    res.NCD[i] <- (NCDDistance(tslistx[[i]], tslisty[[i]]))
    res.PACF[i] <- (PACFDistance(tslistx[[i]], tslisty[[i]]))
    res.PDC[i] <- (PDCDistance(tslistx[[i]], tslisty[[i]]))
    res.Per[i] <- (PerDistance(tslistx[[i]], tslisty[[i]]))
    #res.Pred[i] <- (PredDistance(tslistx[[i]], tslisty[[i]], 2))
    res.SpecGLK[i] <- (SpecGLKDistance(tslistx[[i]], tslisty[[i]]))
    #res.SpecISD[i] <- (SpecISDDistance(tslistx[[i]], tslisty[[i]]))
    res.SpecLLR[i] <- (SpecLLRDistance(tslistx[[i]], tslisty[[i]]))
    res.STS[i] <- (STSDistance(tslistx[[i]], tslisty[[i]]))
    res.TAM[i] <- (TAMDistance(tslistx[[i]], tslisty[[i]]))
    #res.Tquest[i] <- (TquestDistance(tslistx[[i]], tslisty[[i]], tau=2.5))
    res.Sorensen[i] <- (sorensen(tslistx[[i]], tslisty[[i]], testNA=FALSE))
    #res.Hellinger[i] <- (hellinger(tslistx[[i]], tslisty[[i]]))
    res.mydist[i] <- (devdist(tslistx[[i]], tslisty[[i]], 10))

  }
  
  res.temp <- list(res.ACF,res.ARLP,res.ARM,res.ARM.p,res.ARP,res.CC,res.CDM,
                   res.CID,res.Cor,res.Cort,res.Dissim,res.DTW,res.DTW,res.EDR,
                   res.ERP,res.Euclid,res.Fourier,res.Frechet,res.Inf,res.Int,
                   res.LBK,res.LCSS,res.LP,res.Man,res.Mindist,res.Minkow,
                   res.NCD,res.PACF,res.PDC,res.Per,res.Pred,res.SpecGLK,
                   res.SpecISD,res.SpecLLR,res.STS,res.TAM,res.Tquest,
                   res.Sorensen,res.Hellinger,res.mydist)
  
  return(res.temp)
  
}
