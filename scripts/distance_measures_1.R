### Comparing similarity measures
### code copied from Cleasby, 2019

library(TSdist) # package which implements the distances (except Nearest Neighbour), separately from the Cleasby code
library(RANN) # package which implements Nearest Neighbour Distance
library(mgcv) # package for GAMs

# create time series to compare

# identical

a1 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b1 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

c1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

d1 <- seq(from=1, to=10, by=0.1)

x1 <- as.vector(predict(gam(a1~s(c1)), newdata=data.frame(c1=d1)))

y1 <- as.vector(predict(gam(b1~s(c1)), newdata=data.frame(c1=d1)))

# y-axis shift

a2 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b2 <- c(3, 3, 3, 4, 4, 4, 3, 3, 3, 3)

x2 <- as.vector(predict(gam(a2~s(c1)), newdata=data.frame(c1=d1)))

y2 <- as.vector(predict(gam(b2~s(c1)), newdata=data.frame(c1=d1)))

# x-axis shift

a3 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b3 <- c(2, 2, 2, 2, 2, 3, 3, 3, 2, 2)

x3 <- as.vector(predict(gam(a3~s(c1)), newdata=data.frame(c1=d1)))

y3 <- as.vector(predict(gam(b3~s(c1)), newdata=data.frame(c1=d1)))

# single deviation

a4 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b4 <- c(2, 2, 2, 3, 3, 3, 2.5, 2.5, 2.5, 2.5)

x4 <- as.vector(predict(gam(a4~s(c1)), newdata=data.frame(c1=d1)))

y4 <- as.vector(predict(gam(b4~s(c1)), newdata=data.frame(c1=d1)))

# single outlier (short double deviation)

a5 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b5 <- c(2, 2, 2, 3, 3, 3, 2, 1.5, 1.5, 2)

x5 <- as.vector(predict(gam(a5~s(c1)), newdata=data.frame(c1=d1)))

y5 <- as.vector(predict(gam(b5~s(c1)), newdata=data.frame(c1=d1)))

# plateau (long double deviation)

a6 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b6 <- c(2, 2, 2, 3, 3, 2, 1, 0, 0, 2)

x6 <- as.vector(predict(gam(a6~s(c1)), newdata=data.frame(c1=d1)))

y6 <- as.vector(predict(gam(b6~s(c1)), newdata=data.frame(c1=d1)))

# rotational shift

a7 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b7 <- c(2, 2.4, 2.8, 4.2, 4.6, 5, 4.4, 4.8, 5.2, 5.6)

x7 <- as.vector(predict(gam(a7~s(c1)), newdata=data.frame(c1=d1)))

y7 <- as.vector(predict(gam(b7~s(c1)), newdata=data.frame(c1=d1)))

# y-axis shift, but with same starting value (single deviation at start)

a8 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b8 <- c(2, 2.5, 2.5, 3.5, 3.5, 3.5, 2.5, 2.5, 2.5, 2.5)

x8 <- as.vector(predict(gam(a8~s(c1)), newdata=data.frame(c1=d1)))

y8 <- as.vector(predict(gam(b8~s(c1)), newdata=data.frame(c1=d1)))

# random, frequent deviations

a9 <- c(2, 2, 2, 3, 3, 3, 2, 2, 2, 2)

b9 <- c(2, 1, 1, 1, 4, 4, 4, 4, 1, 1)

x9 <- as.vector(predict(gam(a9~s(c1)), newdata=data.frame(c1=d1)))

y9 <- as.vector(predict(gam(b9~s(c1)), newdata=data.frame(c1=d1)))


# run metrics on all time series pairs

# loop to create list of time series pairs

tslistx <- list(x1,x2,x3,x4,x5,x6,x7,x8,x9)

tslisty <- list(y1,y2,y3,y4,y5,y6,y7,y8,y9)

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
res.Inf <- vector()
res.Int <- vector()
res.LBK <- vector()
res.LCSS <- vector()
res.LP <- vector()
res.Man <- vector()
res.Mindist <- vector()
res.Minkow <- vector()
res.NCD <- vector()
res.PACF <- vector()
res.PDC <- vector()
res.Per <- vector()
res.Pred <- vector()
res.SpecGLK <- vector()
res.SpecISD <- vector()
res.SpecLLR <- vector()
res.STS <- vector()
res.TAM <- vector()
res.Tquest <- vector()
res.Wav <- vector()



for (i in 1:length(tslisty)) {
  
  res.ACF[i] <- (ACFDistance(tslistx[[i]], tslisty[[i]]))
  #res.ARLP[i] <- (ARLPCCepsDistance(tslistx[[i]], tslisty[[i]]))
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
  res.EDR[i] <- (EDRDistance(tslistx[[i]], tslisty[[i]], 0))
  res.ERP[i] <- (ERPDistance(tslistx[[i]], tslisty[[i]], 0))
  res.Euclid[i] <- (EuclideanDistance(tslistx[[i]], tslisty[[i]]))
  res.Fourier[i] <- (FourierDistance(tslistx[[i]], tslisty[[i]]))
  #res.Frechet[i] <- (FrechetDistance(tslistx[[i]], tslisty[[i]]))
  res.Inf[i] <- (InfNormDistance(tslistx[[i]], tslisty[[i]]))
  res.Int[i] <- (IntPerDistance(tslistx[[i]], tslisty[[i]]))
  #res.LBK[i] <- (LBKeoghDistance(tslistx[[i]], tslisty[[i]], w=2))
  res.LCSS[i] <- (LCSSDistance(tslistx[[i]], tslisty[[i]], 0))
  res.LP[i] <- (LPDistance(tslistx[[i]], tslisty[[i]]))
  res.Man[i] <- (ManhattanDistance(tslistx[[i]], tslisty[[i]]))
  res.Mindist[i] <- (MindistSaxDistance(tslistx[[i]], tslisty[[i]], 4))
  res.Minkow[i] <- (MinkowskiDistance(tslistx[[i]], tslisty[[i]], 1))
  res.NCD[i] <- (NCDDistance(tslistx[[i]], tslisty[[i]]))
  res.PACF[i] <- (PACFDistance(tslistx[[i]], tslisty[[i]]))
  res.PDC[i] <- (PDCDistance(tslistx[[i]], tslisty[[i]]))
  res.Per[i] <- (PerDistance(tslistx[[i]], tslisty[[i]]))
  #res.Pred[i] <- (PredDistance(tslistx[[i]], tslisty[[i]], 2))
  #res.SpecGLK[i] <- (SpecGLKDistance(tslistx[[i]], tslisty[[i]]))
  #res.SpecISD[i] <- (SpecISDDistance(tslistx[[i]], tslisty[[i]]))
  #res.SpecLLR[i] <- (SpecLLRDistance(tslistx[[i]], tslisty[[i]]))
  res.STS[i] <- (STSDistance(tslistx[[i]], tslisty[[i]]))
  res.TAM[i] <- (TAMDistance(tslistx[[i]], tslisty[[i]]))
  res.Tquest[i] <- (TquestDistance(tslistx[[i]], tslisty[[i]], tau=0))
  res.Wav[i] <- (WavDistance(tslistx[[i]], tslisty[[i]]))

}
res.dev <- vector()
for (i in 1:length(tslisty)) {
  
  res.dev[i] <- (devdist(tslistx[[i]], tslisty[[i]], 20))
  
}

# metrics

ACFDistance(x,y)

ARLPCCepsDistance(x,y)

ARMahDistance(x,y)

ARPicDistance(x,y)





# plot time series

plot(x,type="l",col="red")

lines(y,col="blue")

### Dynamic Time Warping

# set up distance matrix

distances <- array(dim = c(length(y), length(x)), 0)

# fill distance matrix with Euclidean distance between all point pairs

for (i in 1:length(y)) {
  
  for(j in 1:length(x)) {
    
    distances[i, j] <- (x[j] - y[i]) ^ 2
    
  }
  
}

# view distance matrix

colfunc <- colorRampPalette(c("snow", "gold", "orange", "firebrick"))

image(1:ncol(distances), 1:nrow(distances), t(distances), col = colfunc(20), xlab = "x", ylab = "y", las = 1)

for (a in 1:ncol(distances)) {
  
  for (b in 1:nrow(distances)) {
    
    text(a, b, distances[b,a])
    
  }
}

# identify warping path

# set up cost matrix

costs <- array(dim = c(length(y), length(x)), 0)

for (i in 2:length(x)) {
  
  costs[1, i] <- distances[1, i] + costs[1, i-1]
  
}

for (i in 1:length(y)) {
  
  for (j in 2:length(x)) {
    
    costs[i, j] <- min(costs[i-1, j-1], costs[i-1, j], costs[i, j-1]) + distances[i, j]
    
  }
  
}

# examine cost matrix

image(1:ncol(costs), 1:nrow(costs), t(costs), col = colfunc(20), xlab = "x", ylab = "y", las = 1)

for (a in 1:ncol(costs)){
  
  for (b in 1:nrow(costs)){
    
    text(a, b, costs[b, a])
    
  }
  
}   

# find optimal path

i <- length(y)

j <- length(x)

WPath <- cbind(j, i) ## Warping path starts at top corner

# step backward to identify path

while (i > 1 & j > 1){ 
  
  if (i==1) {
    
    j <- j - 1
    
  } else if (j==1){  
    
    i <- i - 1
    
  } else if(costs[i-1, j] == min(costs[i-1, j-1], costs[i-1, j], costs[i, j-1])) {
    
    i <- i -1
    
  } else if(costs[i, j-1] == min(costs[i-1, j-1], costs[i-1, j], costs[i, j-1])) {
    
    j <- j - 1
    
  } else {
    
    i <- i - 1
    
    j <- j - 1
    
  }
  
  WPath <- append(WPath, c(j, i))
  
}

#convert warp path to matrix

WPath <- matrix(WPath, ncol = 2, byrow = TRUE)

#Visualize warping path on cost matrix

image(1:ncol(costs), 1:nrow(costs), t(costs), col = colfunc(20), xlab = "x", ylab = "y", las = 1)

for (a in 1:ncol(costs)){
  
  for (b in 1:nrow(costs)){
    
    text(a, b, costs[b, a])
    
  }
  
}   

points(WPath[,1], WPath[,2], type = 'l')

#Extract DTW from Warping Path

WCost <- 0

for(i in 1:length(y)) {
  
  WCost <- WCost + distances[WPath[i, 2], WPath[i, 1]]
  
  print(WCost)
  
}

WCost ## DTW = 2

### LCSS

# find the length of the time series

dist_i <- length(x)

dist_j <- length(y)

# create array for storing LCSS values

l <- array(dim = c(dist_i+1, dist_j+1), 0)

# build l by counting matches

for (i in 1:dist_i+1) {
  
  for (j in 1:dist_j+1) {
    
    if (i == 1 | j == 1) {
      
      l[i, j] <- 0

    } else if (x[i-1] == y[j-1]) {
      
      l[i, j] <- l[i-1, j-1] + 1
      
    } else {
      
      l[i, j] <- max(l[i-1, j], l[i, j-1])
      
    }
    
  }
  
}

max(l)

### My Method

# function, with x and y as time series, and alpha as moving average window length
devdist <- function(x, y, alpha) {
  
  # take difference of two time series
  z = x - y
  
  # set limits for moving average
  dist <- alpha - 1
  
  # create vector to hold standard deviation values
  t <- vector()
  
  # loop to get moving average of the standard deviation
  for (i in 1:(length(z)-dist)) {
    
    t[i] <- sd(z[i:(i+dist)])
    
  }
  
  # take the mean of the standard deviation values
  dev_val <- mean(t)
  
  # return the mean
  return(dev_val)
}


####


