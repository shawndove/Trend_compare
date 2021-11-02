chinatown_train <- read.csv(file="c:/R_projects/Trend_compare/UCR_datasets/Chinatown_TRAIN.tsv", sep="")
c_train_noclass <- chinatown_train[,2:length(chinatown_train)]
c_train_noclass2 <- t(apply(c_train_noclass, 1, normalize))
#c_train_noclass2.5 <- matrix(NA, nrow=nrow(c_train_noclass2), ncol=ncol(c_train_noclass2))
#for (i in 1:nrow(c_train_noclass2)){
#c_train_noclass2.5[i,] <- loess(c_train_noclass2[i,] ~ seq(1,ncol(c_train_noclass2)), span = 0.4)$fitted
#}
c_train_noclass3 <- c_train_noclass2 + 0.0001
plot(seq(1:length(c_train_noclass)),c_train_noclass3[18,], type="l")
c_train_classes <- chinatown_train[,1]

chinatown_test <- read.csv(file="c:/R_projects/Trend_compare/UCR_datasets/Chinatown_TEST.tsv", sep="")
c_test_noclass <- chinatown_test[,2:length(chinatown_test)]
c_test_noclass2 <- t(apply(c_test_noclass, 1, normalize))
#c_test_noclass2.5 <- matrix(NA, nrow=nrow(c_test_noclass2), ncol=ncol(c_test_noclass2))
#for (i in 1:nrow(c_test_noclass2)){
#  c_test_noclass2.5[i,] <- loess(c_test_noclass2[i,] ~ seq(1,ncol(c_test_noclass2)), span = 0.4)$fitted
#}
c_test_noclass3 <- c_test_noclass2 + 0.0001
plot(seq(1:length(c_test_noclass)),c_test_noclass3[105,], type="l")
c_test_classes <- chinatown_test[,1]

yoga_train <- read.csv(file="c:/R_projects/Trend_compare/UCR_datasets/Yoga/Yoga_TRAIN.tsv", sep="")
y_train_noclass <- yoga_train[,2:length(yoga_train)]
plot(seq(1:length(y_train_noclass)), y_train_noclass[5,], type="l")
y_train_classes <- yoga_train[,1]

yoga_test <- read.csv(file="c:/R_projects/Trend_compare/UCR_datasets/Yoga/Yoga_TEST.tsv", sep="")
y_test_noclass <- yoga_test[,2:length(yoga_test)]
plot(seq(1:length(y_test_noclass)), y_test_noclass[8,], type="l")
y_test_classes <- yoga_test[,1]

synth_control_train <- read.csv(file="c:/R_projects/Trend_compare/UCR_datasets/SyntheticControl/SyntheticControl_TRAIN.tsv", sep="")
#synth_control_train <- synth_control_train[synth_control_train[,1]!=2,]
sc_train_noclass <- synth_control_train[,2:length(synth_control_train)]
#sc_train_noclass <- data.Normalization(sc_train_noclass, type="n4", normalization="row") + 0.0001
#for (i in 1:nrow(sc_train_noclass)){
#  sc_train_noclass[i,] <- loess(as.numeric(sc_train_noclass[i,]) ~ seq(1,ncol(sc_train_noclass)), span = 0.1)$fitted
#}
plot(seq(1:length(sc_train_noclass)), sc_train_noclass[263,], type="l")
sc_train_classes <- synth_control_train[,1]

synth_control_test <- read.csv(file="c:/R_projects/Trend_compare/UCR_datasets/SyntheticControl/SyntheticControl_TEST.tsv", sep="")
#synth_control_test <- synth_control_test[synth_control_test[,1]!=2,]
synth_control_test <- sample_n(synth_control_test, 299, replace=FALSE)
sc_test_noclass <- synth_control_test[,2:length(synth_control_test)]
library(clusterSim)
sc_test_noclass <- data.Normalization(sc_test_noclass, type="n4", normalization="row") + 0.0001
#for (i in 1:nrow(sc_test_noclass)){
#  sc_test_noclass[i,] <- loess(as.numeric(sc_test_noclass[i,]) ~ seq(1,ncol(sc_test_noclass)), span = 0.1)$fitted
#}
plot(seq(1:length(sc_test_noclass)), sc_test_noclass[5,], type="l")
sc_test_classes <- synth_control_test[,1]


import_dataset <- function(dataset, wd) {
  
  library(ggplot2)
  library(grid)
  library(gridExtra)
  
  print(paste(wd, dataset, "/", dataset, "_TRAIN.tsv", sep = ""))
  
  data.train.all <- tryCatch(read.csv(file=paste(wd, dataset, "/", dataset, "_TRAIN.tsv", sep = ""), sep = ""),
                       error = function(e) {print("Please check that you have correctly input the path where your datasets are stored.")})
  
  colnames(data.train.all) <- NULL
  
  rownames(data.train.all) <- NULL
  
  data.train.noclass <- data.train.all[,2:length(data.train.all)]
  
  data.train.classes <- data.train.all[,1]
  
  data.test.all <- tryCatch(read.csv(file=paste(wd, dataset, "/", dataset, "_TEST.tsv", sep = ""), sep = ""),
                       error = function(e) {print("Please check that you have correctly input the path where your datasets are stored.")})
  
  colnames(data.test.all) <- NULL
  
  rownames(data.test.all) <- NULL
  
  data.test.noclass <- data.test.all[,2:length(data.test.all)]
  
  data.test.classes <- data.test.all[,1]
  
  p <- list()
  
  for (i in seq_along(unique(data.train.classes))) {
    
    class_i <- data.train.noclass[data.train.all[,1]==i,]
    
    rownames(class_i) <- NULL
    
    if (nrow(class_i) >= 4) {
      
      examples <- data.frame(x1 = t(class_i[1,]), 
                             x2 = t(class_i[2,]), 
                             x3 = t(class_i[3,]), 
                             x4 = t(class_i[4,]), 
                             time=seq(1, nrow(t(class_i))))
      
      colnames(examples) <- c("x1", "x2", "x3", "x4", "time")
      
    } else if (nrow(class_i) == 3) {
      
      examples <- data.frame(x1 = t(class_i[1,]), 
                             x2 = t(class_i[2,]), 
                             x3 = t(class_i[3,]), 
                             time=seq(1, nrow(t(class_i))))
      
      colnames(examples) <- c("x1", "x2", "x3", "time")
      
    } else if (nrow(class_i) == 2) {
      
      examples <- data.frame(x1 = t(class_i[1,]), 
                             x2 = t(class_i[2,]), 
                             time=seq(1, nrow(t(class_i))))
      
      colnames(examples) <- c("x1", "x2", "time")
      
    } else {
      
      examples <- data.frame(x1 = t(class_i[1,]), 
                             time=seq(1, nrow(t(class_i))))
      
      colnames(examples) <- c("x1", "time")

    }
      
    plot1 <- ggplot(examples, 
                    aes(x = time, y = x1)) +
      geom_line()
    
    if (length(examples) > 2) {
      
      plot2 <- ggplot(examples, 
                      aes(x = time, y = x2)) +
        geom_line()
      
    } 
    
    if (length(examples) > 3) {
      
      
      plot3 <- ggplot(examples, 
                      aes(x = time, y = x3)) +
        geom_line()
      
    } 
    
    if (length(examples) > 4) {
      
      
      plot4 <- ggplot(examples, 
                      aes(x = time, y = x4)) +
        geom_line()

    }
    
    if (length(examples) == 2) {
      
      p[[i]] <- grid.arrange(plot1, ncol=1,
                   top = textGrob(paste(dataset, ":", "\n", "Class ", i, " Examples", sep="")))
      
    } else if (length(examples) == 3) {

      p[[i]] <- grid.arrange(plot1, plot2, ncol=2,
                   top = textGrob(paste(dataset, ":", "\n", "Class ", i, " Examples", sep="")))
      
    } else if (length(examples) == 4) {
      
      p[[i]] <- grid.arrange(plot1, plot2, plot3, ncol=2,
                   top = textGrob(paste(dataset, ":", "\n", "Class ", i, " Examples", sep="")))
      
    } else if (length(examples) > 4) {
      
      p[[i]] <- grid.arrange(plot1, plot2, plot3, plot4, ncol=2,
                   top = textGrob(paste(dataset, ":", "\n", "Class ", i, " Examples", sep="")))
      
    }
    
  #  print(p[[i]])

  }
  
  return(list(data.train.noclass, data.train.classes, data.test.noclass, data.test.classes))
  
} 


onenn <- function(train, test, class, distance, arguments, type){
  n.test <- nrow(test)
  n.train <- nrow(train)
  ddist <- matrix(NA, nrow = n.test, ncol = n.train)
  cl.train <- vector()
  #ddist <- NULL
  for(i in 1:n.test) {
    for(j in 1:n.train) {
      xmat <- rbind(as.numeric(test[i,]), as.numeric(train[j,])) #we make a 2 row matrix combining the current test and train rows
      if (is.list(arguments)){
        args. <- list()
        args.[[1]] <- xmat[1,]
        names(args.)[[1]] <- ifelse(type==1, "P", "x")
        args.[[2]] <- xmat[2,]
        names(args.)[[2]] <- ifelse(type==1, "Q", "y")
        for (k in 1:length(arguments)) 
        {
          
          args.[2+k] <- arguments[k]
          
          names(args.)[2+k] <- names(arguments[k])
          
        }
        ddist[i, j] <- do.call(distance, args.)  #then we calculate the distance and append it to the ddist vector.
      } else {
        ddist[i, j] <- distance(xmat[1,], xmat[2,])  #then we calculate the distance and append it to the ddist vector.
      }
    }
    neigh <- which.min(ddist[i,])
    cl.train[i] <- class[neigh]
    #neigh[i, ] <- sort(ddist)[2:(k + 1)] 
  }
  return(cl.train)
}

errorslist_ct <- list()
#distlist <- c(dist.fnlist1, dist.fnlist2)
#argslist <- c(dist.argslist1, dist.argslist2)
#nameslist <- c(dist.nameslist1, dist.nameslist2)
for (i in 1:length(c(dist.fnlist1, dist.fnlist2))){
  type <- ifelse(i > length(dist.fnlist1), 1, 0)
  class_result <- onenn(c_train_noclass3, 
                        c_test_noclass3, 
                        c_train_classes, 
                        c(dist.fnlist1,dist.fnlist2)[[i]], 
                        c(dist.argslist1,dist.argslist2)[[i]],
                        type)
  error_count <- sum(class_result!=c_test_classes)
  error_pct <- (error_count / length(class_result)) * 100
  print(c(dist.nameslist1,dist.nameslist2)[[i]])
  print(error_pct)
  errorslist_ct[i] <- error_pct
}

errorslist_yoga <- list()
#distlist <- c(dist.fnlist1, dist.fnlist2)
#argslist <- c(dist.argslist1, dist.argslist2)
#nameslist <- c(dist.nameslist1, dist.nameslist2)
for (i in 19:length(dist.fnlist1)){
  # type <- ifelse(i > length(dist.fnlist1), 1, 0)
  class_result <- onenn(y_train_noclass, 
                        y_test_noclass[1:100,], 
                        y_train_classes, 
                        dist.fnlist1[[i]], 
                        dist.argslist1[[i]],
                        type=0)
  error_count <- sum(class_result!=y_test_classes[1:100])
  error_pct <- (error_count / length(class_result)) * 100
  print(dist.nameslist1[[i]])
  print(error_pct)
  errorslist_yoga[i] <- error_pct
}

errorslist_sc <- list()
#distlist <- c(dist.fnlist1, dist.fnlist2)
#argslist <- c(dist.argslist1, dist.argslist2)
#nameslist <- c(dist.nameslist1, dist.nameslist2)
for (i in 5:length(dist.fnlist1)){
  # type <- ifelse(i > length(dist.fnlist1), 1, 0)
  class_result <- onenn(sc_train_noclass, 
                        sc_test_noclass[1:50,], 
                        sc_train_classes, 
                        dist.fnlist1[[i]], 
                        dist.argslist1[[i]],
                        type=0)
  error_count <- sum(class_result!=sc_test_classes[1:50])
  error_pct <- (error_count / length(class_result)) * 100
  print(dist.nameslist1[[i]])
  print(error_pct)
  errorslist_sc[i] <- error_pct
}


