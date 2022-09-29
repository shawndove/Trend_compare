# load libraries ----

library(reshape2)
library(ggplot2)

# load extra functions ----

# function to get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# function to get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# function to reorder correlation matrix
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# load distance measures ----

source("scripts/select_distance_measures.R")


# get names for all distance measures
dist.names <- c(dist.nameslist1, dist.nameslist2)

# create a list to hold results
temp_levels <- list()

# loop along names
for (i in seq_along(dist.names)) {
  
  # save distance measure name to a vector
  print_name <- dist.names[i]
  
  # load wide format test results from saved file
  temp_levels1 <- readRDS(file = paste("files/controlled_results/", print_name, "_dfwide.RData", sep=""))
  
  # get the relative sensitivity range results
  temp_levels[[i]] <- temp_levels1[17,]
  
}

# convert list to data frame, name the rows
levels <- do.call(rbind, temp_levels)
rownames(levels) <- dist.names
levels <- as.data.frame(as.matrix(levels[,-1])) # remove test name
#colnames(levels <- c()) # name the results

# add a type column for distance measure families
levels$Type <- c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", "Elastic", "Elastic", 
             "Feature", "Feature",
             "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
             "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon's_Entropy", "Shannon's_Entropy",
             "Intersection", "Shannon's_Entropy", "Other", "Shannon's_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
             "Squared_L2", "Other", "Shannon's_Entropy", "Intersection")

# convert to factor
levels$Type <- as.factor(levels$Type)

# reorder type column
levels$Type <- factor(levels$Type, levels=c("Compression", "Model", "Feature", "Other", "Elastic", "Intersection", 
                                            "Inner_Product", "Fidelity", "Shannon's_Entropy", "Squared_L2", "L1", "Minkowski"),
                           labels=c("Compression-Based", "Model-Based", "Feature-Based", "Other Shape-Based", "Elastic", "Intersection Family",  
                                    "Inner Product Family", "Fidelity Family", "Shannon's Entropy Family", "Squared L2 Family", "L1 Family", "Minkowski Family"))

# reorder according to type
levels <- levels[order(levels$Type, decreasing=FALSE),]

# hierarchical clustering
cluster.dm <- hclust(dist(levels[,1:7], method="euclidean"), method="average")

# Pearson's correlation heatmap
cluster.cormat <- round(cor(t(levels[,1:7]), method = "pearson"), 2)
#melted_cormat <- melt(cluster.cormat, na.rm = TRUE)
# reorder correlation matrix
#reorder.cormat <- reorder_cormat(cluster.cormat)

# get upper triangle of correlation matrix and reorganize
lower_tri <- get_lower_tri(cluster.cormat)
melted_cormat <- melt(lower_tri, na.rm = TRUE)

#plot heatmap
ggplot(data = melted_cormat, aes(Type2, Type1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "steelblue2", high = "darkorange", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   size = 10, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_fixed()

ggsave(filename="pearson_correlation.tiff",
       plot=last_plot(),
       dpi=1000,
       height=8000,
       width=8000,
       units="px",
       compression="lzw")

familyID <- setNames(levels$Type, rownames(levels))
melted_cormat$Type1 <- familyID[melted_cormat$Var1]
melted_cormat$Type2 <- familyID[melted_cormat$Var2]
within_groups_cormat <- melted_cormat[melted_cormat$Type1==melted_cormat$Type2,]
within_groups_cormat <- within_groups_cormat[within_groups_cormat$Var1!=within_groups_cormat$Var2,]
between_groups_cormat <- melted_cormat[melted_cormat$Type1!=melted_cormat$Type2,]

wgc <- within_groups_cormat %>% 
  group_by(Type1) %>%
  summarise_at(vars(value), list(mean = mean))

bgc <- between_groups_cormat %>%
  group_by(Type1) %>%
  summarise_at(vars(value), list(mean = mean))
