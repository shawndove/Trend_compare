# load libraries ----

library(tidyr)
library(ggplot2)

# load distance measures ----

source("scripts/select_distance_measures.R")

# metric results ----

# get names for all distance measures
dist.names <- c(dist.nameslist1, dist.nameslist2)

# create a list to hold results
interpreted.results <- list()

# loop along names of distance measures
for (i in seq_along(dist.names)) {
  
  # save distance measure name to a vector
  print_name <- dist.names[[i]]
  
  # create a vector to hold metric test results
  metric_tr <- vector()
  
  # load uniqueness and symmetry results from saved file
  temp <- readRDS(file = paste("files/controlled_results/", print_name, "_dfwide.RData", sep=""))
  
  # load triangle inequality test results from saved file
  temp_ti <- readRDS(file = paste("files/ti_results/", print_name, "_tifull.RData", sep=""))
  ti_l <- length(temp_ti)
  
  # load non-negativity test results from saved file
  temp_nn <- readRDS(file = paste("files/nn_results/", print_name, "_nnfull.RData", sep=""))
  
  # 0 for uniqueness is a pass, any other value is a fail
  interpreted.results$"Uniqueness"[i] <- ifelse(temp[1,2] == 0, "Yes", "No")
  metric_tr[1] <- ifelse(temp[1,2] == 0, TRUE, FALSE)
  
  # if the two results are identical, symmetry is a pass, otherwise fail
  interpreted.results$"Symmetry"[i] <- ifelse(temp[2,3] == temp[2,2], "Yes", "No")
  metric_tr[2] <- ifelse(temp[2,3] == temp[2,2], TRUE, FALSE)
  
  # TRUE for non-negativity is a pass, FALSE is a fail
  interpreted.results$"Non-negativity"[i] <- ifelse(temp_nn[[2]] == TRUE, "Yes", "No")
  metric_tr[3] <- ifelse(temp_nn[[2]] == TRUE, TRUE, FALSE)
  
  # TRUE for triangle inequality is a pass, FALSE is a fail
  interpreted.results$"Triangle Inequality"[i] <- ifelse(temp_ti[[ti_l-3]] == TRUE, "Yes", "No")
  metric_tr[4] <- ifelse(temp_ti[[ti_l-3]] == TRUE, TRUE, FALSE)
  
  # passing all tests means full metric, failing only triangle inequality means semi metric, failng any others means non-metric
  interpreted.results$"Metric"[i] <- ifelse(all(metric_tr), "Full", ifelse(all(metric_tr[1:3]), "Semi", "Non"))
  
}

# convert to a data frame and add a names column
interpreted.results.1 <- do.call(rbind, interpreted.results)
interpreted.results.2 <- as.data.frame(t(interpreted.results.1))
interpreted.results.2$"Distance Measure" <- dist.names

# copy to new data frame
mr <- interpreted.results.2

# add a type column for distance measure families
mr$Type <- c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", "Elastic", "Elastic", 
             "Feature", "Feature",
             "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
             "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon's_Entropy", "Shannon's_Entropy",
             "Intersection", "Shannon's_Entropy", "Other", "Shannon's_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
             "Squared_L2", "Other", "Shannon's_Entropy", "Intersection")

# convert to factor
mr$Type <- as.factor(mr$Type)

# convert to long format and remove NaNs
metric_long <- tidyr::gather(mr, Test, Value, 1:5, factor_key=TRUE)
metric_long <- metric_long[!is.nan(metric_long$Value),]

# reorder type column
metric_long$Type <- factor(metric_long$Type, levels=c("Minkowski", "Intersection", "L1", "Elastic",  "Squared_L2", "Other", 
                                                      "Shannon's_Entropy", "Feature", "Fidelity", "Model", "Inner_Product", "Compression"),
                           labels=c("Minkowski Family", "Intersection Family", "L1 Family", "Elastic", "Squared L2 Family", 
                                    "Other Shape-Based","Shannon's Entropy Family", "Feature-Based", "Fidelity Family", "Model-Based", 
                                    "Inner Product Family", "Compression-Based"))

# add/fix column names
names(metric_long) <- c("DM", "Type", "Test", "Value")

# add symbols to certain distance measure names
metric_long$DM[metric_long$DM=="KumarJohnson"]<-"Kumar"
metric_long$DM[metric_long$DM=="Additive"]<-"*Additive"
metric_long$DM[metric_long$DM=="Canb"]<-"*Canb"
metric_long$DM[metric_long$DM=="Clark"]<-"*Clark"
metric_long$DM[metric_long$DM=="Czek"]<-"*Czek"
metric_long$DM[metric_long$DM=="EDR"]<-"\u2020EDR"
metric_long$DM[metric_long$DM=="Kulcz"]<-"*Kulcz"
metric_long$DM[metric_long$DM=="ProbSymm"]<-"*ProbSymm"
metric_long$DM[metric_long$DM=="Soergel"]<-"*Soergel"
metric_long$DM[metric_long$DM=="SqChi"]<-"*SqChi"
metric_long$DM[metric_long$DM=="WaveHedges"]<-"*Wave"

# convert value column to factor
metric_long$Value <- factor(metric_long$Value)

# add starred column and define which distance measures are starred
metric_long$Starred <- rep("No", times=210)
metric_long$Starred[metric_long$DM=="*Soergel" | 
                      metric_long$DM=="*Clark" | 
                      metric_long$DM=="*Canb" | 
                      metric_long$DM=="*Wave" |
                      metric_long$DM=="*Additive" | 
                      metric_long$DM=="*Czek" | 
                      metric_long$DM=="*Kulcz" | 
                      metric_long$DM=="*ProbSymm" |
                      metric_long$DM=="*SqChi"] <- "Yes"

# add dagger column and define which distance measures have dagger symbol
metric_long$Dagger <- rep("No", times=210)
metric_long$Dagger[metric_long$Dagger=="\u2020EDR"] <- "Yes"

# reorder distance measures according to type
ml_ordered <- metric_long[order(metric_long$Type),]
ml_ordered <- ml_ordered[ml_ordered$Test=="Uniqueness",]

# make the plot
pmall <- ggplot(metric_long, aes(x = Test, 
                                 y = DM,
                                 shape=Value, 
                                 colour=Value,
                                 label=Value)) +
  geom_point(size=4) +
  geom_text(data=metric_long[metric_long$Value=="Semi" | metric_long$Value=="Full" | metric_long$Value=="Non" | metric_long$Value=="Hemi",], 
            aes(label=Value), color="black", size=4, fontface = "bold") +
  scale_shape_manual(values=c("\U025A3", "\u274E", "\U025A3", "\U025A3", "\u2611")) +
  scale_colour_manual(values=c("white", "dark red", "white", "white", "dark green")) +
  scale_x_discrete(labels=c("Uniqueness", "Symmetry", "Non-Negativity", "Triangle Inequality", "Metric Status"))+
  facet_wrap(Type~., scales="free_y", ncol=2) +
  ggtitle(paste("Metric Test Results")) +
  labs(caption = "\n *These distances respond differently when inputs are constrained to non-negative real numbers. As we included
negative values in our tests, our results for these measures may differ from others (e.g. Kocher and Savoy, 2017).
\n \u2020This distance is a full metric when the threshold value (epsilon) is set at 0.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=14, angle=90, vjust=0.2, hjust=1, 
                                   face=c("plain", "plain", "plain", "plain", "bold",
                                          "plain", "plain", "plain", "plain", "bold")),
        axis.text.y = element_text(size=14),
        legend.position = "none",
        strip.text.x = element_text(size=12),
        plot.caption.position = "plot",
        plot.caption = element_text(size=10, hjust=0))

# get the grob data
pmall.grob <- ggplotGrob(pmall)

# adjust the heights of the facets
pmall.grob$heights[8] = unit(3, "null")
pmall.grob$heights[13] = unit(4, "null")
pmall.grob$heights[18] = unit(6, "null")
pmall.grob$heights[23] = unit(5, "null")
pmall.grob$heights[28] = unit(1, "null")
pmall.grob$heights[33] = unit(2, "null")

# adjust the colours of the distance measure names
pmall.grob$grobs[[44]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
pmall.grob$grobs[[45]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark blue", "dark blue", "grey30", "grey30")
pmall.grob$grobs[[46]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark blue", "dark blue", "dark blue", "dark blue", "grey30", "grey30")
pmall.grob$grobs[[47]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
pmall.grob$grobs[[48]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
pmall.grob$grobs[[49]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
pmall.grob$grobs[[38]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark blue", "dark blue", "dark blue")
pmall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark red", "grey30", "grey30", "grey30")
pmall.grob$grobs[[40]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
pmall.grob$grobs[[41]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
pmall.grob$grobs[[42]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
pmall.grob$grobs[[43]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"

# bold the coloured names
pmall.grob$grobs[[45]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 2, 1, 1))
pmall.grob$grobs[[46]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 2, 2, 2, 1, 1))
pmall.grob$grobs[[38]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 2, 2))
pmall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <- 
  as.integer(c(2, 1, 1, 1))

# draw the adjusted plot
grid::grid.newpage()
grid::grid.draw(pmall.grob)

# save the adjusted plot
if(!dir.exists("figures/")) {dir.create("figures/")}
ggsave(paste("figures/metric_test_results.tiff", sep=""), 
       pmall.grob, width=7480, height=10255, units="px", dpi=1000, scale=1, compression="lzw")
#ggsave(paste("figures/metric_test_results.tiff", sep=""), 
 #       pmall.grob, width=3740, height=5128, units="px", dpi=1000, scale=2, compression="lzw")
       
       

# sensitivity results ----

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

# function get results for a given property, round them, remove negative values, 
# NAs and NaNs, then bin them into 5 defined sensitivity levels
levels_fn <- function(levels_df) {
  
  levels_df[,2] <- round(levels_df[,2], digits=3)
  
  levels_df <- levels_df[levels_df[,2] >= 0,]
  
  levels_df <- levels_df[!is.na(levels_df[,2]),]
  
  levels_df <- levels_df[!is.nan(levels_df[,2]),]
  
  levels_df$Levels <- cut(levels_df[,2], 
                          breaks=c(0,0.2,0.7,1.3,2.5,20), 
                          right = FALSE,
                          ordered_result = TRUE,
                          labels = c("Very Low", "Low", "Med", "High", "Very High"))
  
  return(levels_df)
  
}

# create list for binned results
levels_binned <- list()

# get binned sensitivity results for Translation Sensitivity
for (i in 2:(length(levels))) {
  
  levels_temp <- levels[,c(1,i)]
  levels_binned[[i-1]] <- levels_fn(levels_temp)
  
}

# create list for interpreted results
interpreted.results <- list()

# loop along tests
for (j in 3:9) {
  
  # create a temporary vector to hold results for that test
  ir_temp <- vector()
  
  # loop along distance measure names
  for (i in seq_along(dist.names)) {
    
    # put name of distance measure into vector
    print_name <- dist.names[[i]]
    
    # load wide format test results
    temp <- readRDS(file = paste("files/controlled_results/", print_name, "_dfwide.RData", sep=""))
    
    # interpret and classify results
    ir_temp[i] <-
      ifelse(all(abs(temp[j,2:6]) < 0.000001), "Inv",
             ifelse((max(temp[j,2:6]) - min(temp[j,2:6])) < 0.000001, "Very Low", 
                    ifelse((temp[j,6] > temp[j,5] & temp[j,5] > temp[j,4] & temp[j,4] > temp[j,3] & temp[j,3] > temp[j,2]) |
                             (temp[j,6] < temp[j,5] & temp[j,5] < temp[j,4] & temp[j,4] < temp[j,3] & temp[j,3] < temp[j,2]),
                           as.character(levels_binned[[j-2]][print_name,]$Levels), "Unpredictable")))
    
  }
  
  # put results into list
  interpreted.results[[j-2]] <- ir_temp
  
}

interpreted.results.1 <- do.call(rbind, interpreted.results)
interpreted.results.2 <- as.data.frame(t(interpreted.results.1))

for (i in 1:ncol(interpreted.results.2)) {
  
  interpreted.results.2[,i] <- factor(interpreted.results.2[,i],
                                      levels=c("Unpredictable", "Inv", "Very Low", "Low", "Med", "High", "Very High"), 
                                      labels=c("Unpredictable", "Invariant", "Very Low", "Low", "Medium", "High", "Very High"))
  
}

interpreted.results.2$"Distance Measure" <- dist.names

rsr <- do.call(rbind, temp_levels)
rsr$DM <- dist.names
names(rsr) <- c("Test",temp_levels1$Test[c(3:9)],"DM")

rsr$Type <- c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", 
              "Elastic", "Elastic", "Feature", "Feature",
              "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
              "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon's_Entropy", "Shannon's_Entropy",
              "Intersection", "Shannon's_Entropy", "Other", "Shannon's_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
              "Squared_L2", "Other", "Shannon's_Entropy", "Intersection")
rsr$Type <- as.factor(rsr$Type)

test <- rsr[,2:10]

test_long <- tidyr::gather(test, Test, Value, 1:7, factor_key=TRUE)

test_long$Value[is.nan(test_long$Value)] <- 0

test_long$Level <- c(interpreted.results.2[,1], interpreted.results.2[,2], interpreted.results.2[,3],
                     interpreted.results.2[,4], interpreted.results.2[,5], interpreted.results.2[,6],
                     interpreted.results.2[,7])

#test_long$Level <- factor(test_long$Level, 
#                          levels=c(1,2,3,4,5,6,7), 
#                          labels=c("Unpredictable","Invariant", "Very Low", "Low", "Medium", "High", "Very High"))

test_long$Type <- factor(test_long$Type, levels=c("Minkowski", "Intersection", "L1", "Elastic",  "Squared_L2", "Other", 
                                                  "Shannon's_Entropy", "Feature", "Fidelity", "Model", "Inner_Product", "Compression"),
                         labels=c("Minkowski Family", "Intersection Family", "L1 Family", "Elastic", "Squared L2 Family", 
                                  "Other Shape-Based","Shannon's Entropy Family", "Feature-Based", "Fidelity Family", "Model-Based", 
                                  "Inner Product Family", "Compression-Based"))

test_long$DM[test_long$DM=="KumarJohnson"]<-"Kumar"
test_long$DM[test_long$DM=="WaveHedges"]<-"Wave"
test_long$DM[test_long$DM=="EDR"]<-"*EDR"

test_long$Value[test_long$Level=="Unpredictable"] <- -1

cbPalette <- c("gray56", "snow", "steelblue1", "lightsteelblue1", "gray88", "#E6BF86", "#FFA500")

ptall <-ggplot(test_long, aes(x = Test, 
                              y = DM,
                              label = round(Value, 1), fill=Level)) +
  geom_point(colour="black", size=8, shape=22) +
  geom_text(colour="black", size=3.5, fontface = "bold") +
  scale_fill_manual(values=cbPalette, guide = guide_legend()) +
  scale_x_discrete(labels=c("Translation", "Amplitude", "Duration", "Frequency", 
                            "White Noise", "Biased Noise", "Outlier"))+
  facet_wrap(Type~., scales="free_y", ncol=2) +
  labs(caption = "\n\n\n\n\n Sensitivity Ranges: Very Low: < 0.2, Low: 0.2 - 0.7, Medium: 0.7 - 1.3, High: 1.3 - 2.5, Very High: > 2.5.
\n *The results for EDR strongly depended on the threshold setting, epsilon. Here, it was set to 0.1.") +
  ggtitle(paste("Sensitivity Test Results")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=14, angle=90, vjust=0.2, hjust=1),
        axis.text.y = element_text(size=14),
        legend.position = c(0.5, -0.24),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.key.size = unit(0, 'cm'),
        strip.text.x = element_text(size=12),
        plot.caption.position = "plot",
        plot.caption = element_text(size=10, hjust=0)) +
  guides(fill = guide_legend(label.position = "bottom", label.hjust = 0.4, label.vjust = 0.4, nrow = 1,
                             label.theme = element_text(size=12, angle = 0)))

ptall.grob <- ggplotGrob(ptall)

ptall.grob$heights[8] = unit(3, "null")
ptall.grob$heights[13] = unit(4, "null")
ptall.grob$heights[18] = unit(6, "null")
ptall.grob$heights[23] = unit(5, "null")
ptall.grob$heights[28] = unit(1, "null")
ptall.grob$heights[33] = unit(2, "null")

ptall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark blue", "grey30", "grey30", "grey30")
ptall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <-
  as.integer(c(2, 1, 1, 1))

grid::grid.newpage()
grid::grid.draw(ptall.grob)

if(!dir.exists("figures/")) {dir.create("figures/")}
ggsave(paste("figures/sens_test_results.tiff", sep=""), 
       ptall.grob, width=7480, height=10255, units="px", dpi=1000, scale=1, compression="lzw")

# other properties ----

# create a list to hold results
interpreted.results <- list()

# loop along names of distance measures
for (i in seq_along(dist.names)) {
  
  # put distance measure name into a vector
  print_name <- dist.names[[i]]
  
  # load wide format test results
  temp <- readRDS(file = paste("files/controlled_results/", print_name, "_dfwide.RData", sep=""))
  
  # interpret and classify antiparallelism bias results
  interpreted.results$"Antiparallelism Bias"[i] <- 
    ifelse(temp[10,3] == temp[10,2], "Neutral", 
           ifelse(temp[10,3] > temp[10,2], "Positive", "Negative"))
  
  # interpret and classify non-positive value handling results
  interpreted.results$"Non-positive Value Handling"[i] <-
    ifelse(!is.na(temp[14,3]) & !is.nan(temp[14,3]), 
           ifelse(!is.na(temp[14,4]) & !is.nan(temp[14,4]), "All", "Zeros Only"), 
           ifelse(!is.na(temp[14,4]) & !is.nan(temp[14,4]), "Negative Only", "None"))
  
  # interpret and classify results for phase invariance
  interpreted.results$"Phase Invariance"[i] <-
    ifelse(all(abs(temp[11,2:6]) < 0.000001), "Invariant", 
           ifelse((max(diff(as.vector(as.matrix(temp[11,2:6])))) / mean(diff(as.vector(as.matrix(temp[11,2:6])))) > 2), "Unpredictable",
                  ifelse((temp[11,6] > temp[11,5] & temp[11,5] > temp[11,4] & temp[11,4] > temp[11,3] & temp[11,3] > temp[11,2]) |
                           (temp[11,6] < temp[11,5] & temp[11,5] < temp[11,4] & temp[11,4] < temp[11,3] & temp[11,3] < temp[11,2]) , "Sensitive", 
                         ifelse((max(temp[11,2:6]) - min(temp[11,2:6])) < 0.000001, "Insensitive", "Unpredictable"))))
  
  # interpret and classify results for uniform time scaling invariance
  interpreted.results$"Uniform Time Scaling Invariance"[i] <-
    ifelse(all(is.na(temp[12,2:6])), "NA", 
           ifelse(all(abs(temp[12,2:6]) < 0.000001), "Invariant", 
                  ifelse((max(diff(as.vector(as.matrix(temp[12,2:6])))) / mean(diff(as.vector(as.matrix(temp[12,2:6])))) > 2), "Unpredictable",
                         ifelse((temp[12,6] > temp[12,5] & temp[12,5] > temp[12,4] & temp[12,4] > temp[12,3] & temp[12,3] > temp[12,2]) |
                                  (temp[12,6] < temp[12,6] & temp[12,5] < temp[12,4] & temp[12,4] < temp[12,3] & temp[12,3] < temp[12,2]), "Sensitive", 
                                ifelse((max(temp[12,2:6]) - min(temp[12,2:6])) < 0.000001, "Insensitive", "Unpredictable")))))
  
  # interpret and classify results for warping invariance
  interpreted.results$"Warping Invariance"[i] <-
    ifelse(all(is.na(temp[13,2:6])), "NA", 
           ifelse(all(abs(temp[13,2:6]) < 0.000001), "Invariant",
                  ifelse(max(diff(as.vector(as.matrix(temp[13,2:6])))) / mean(diff(as.vector(as.matrix(temp[13,2:6])))) > 2, "Unpredictable",
                         ifelse((temp[13,6] > temp[13,5] & temp[13,5] > temp[13,4] & temp[13,4] > temp[13,3] & temp[13,3] > temp[13,2]) |
                                  (temp[13,6] < temp[13,5] & temp[13,5] < temp[13,4] & temp[13,4] < temp[13,3] & temp[13,3] < temp[13,2]), "Sensitive", 
                                ifelse((max(temp[13,2:6]) - min(temp[13,2:6])) < 0.000001, "Insensitive", "Unpredictable")))))
  
}

# convert to data frame
interpreted.results.1 <- do.call(rbind, interpreted.results)
interpreted.results.2 <- as.data.frame(t(interpreted.results.1))

# convert results to factors
for (i in 1:ncol(interpreted.results.2)) {
  
  interpreted.results.2[,i] <- factor(interpreted.results.2[,i],
                                      levels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros Only", "Negative Only", "Sensitive", "Invariant", "Insensitive", "Unpredictable", "NA"), 
                                      labels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros Only", "Negative Only", "Sensitive", "Invariant", "Insensitive", "Unpredictable", "NA"))
  
}

# add distance measure names
interpreted.results.2$"Distance Measure" <- dist.names

# copy to new data frame
or <- interpreted.results.2

# create type column with distance measure families and make it a factor
or$Type <- c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", 
             "Elastic", "Elastic", "Feature", "Feature",
             "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
             "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon's_Entropy", "Shannon's_Entropy",
             "Intersection", "Shannon's_Entropy", "Other", "Shannon's_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
             "Squared_L2", "Other", "Shannon's_Entropy", "Intersection")
or$Type <- as.factor(or$Type)

# convert to long format
other_long <- tidyr::gather(or, Test, Value, 1:5, factor_key=TRUE)

# convert type column to factor again
other_long$Type <- factor(other_long$Type, levels=c("Minkowski", "Intersection", "L1", "Elastic",  "Squared_L2", "Other", 
                                                    "Shannon's_Entropy", "Feature", "Fidelity", "Model", "Inner_Product", "Compression"),
                          labels=c("Minkowski Family", "Intersection Family", "L1 Family", "Elastic", "Squared L2 Family", 
                                   "Other Shape-Based","Shannon's Entropy Family", "Feature-Based", "Fidelity Family", "Model-Based", 
                                   "Inner Product Family", "Compression-Based"))

# name columns
names(other_long) <- c("DM", "Type", "Test", "Value")

# shorten names of some distance measures, or add symbols
other_long$DM[other_long$DM=="KumarJohnson"]<-"Kumar"
other_long$DM[other_long$DM=="WaveHedges"]<-"Wave"
other_long$DM[other_long$DM=="EDR"]<-"*EDR"

# convert value column to factor
other_long$Value <- factor(other_long$Value,
                           levels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros Only", "Sensitive", "Invariant", "Insensitive", "Unpredictable", "NA"), 
                           labels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros", "Sens", "Inv", "Ins", "Unp", "n/a"))

# associate stored images with value categories
emoji_pic <- data.frame(
  Value = c("Neutral", "Positive", "Negative", 
            "All", "None", "Zeros",
            "Sens", "Inv", "Ins", "Unp", "n/a"),
  emoji_link = c("figure_images/u29B6-c.png",
                 "figure_images/u2295-c2.png",
                 "figure_images/u2296-c2.png",
                 "figure_images/black-circle_26ab.png",
                 "figure_images/black-circle_26ab.png",
                 "figure_images/black-circle_26ab.png",
                 "figure_images/black-circle_26ab.png",
                 "figure_images/black-circle_26ab.png",
                 "figure_images/black-circle_26ab.png",
                 "figure_images/black-circle_26ab.png",
                 "figure_images/black-circle_26ab.png"))

# create a function to link them in ggplot
func_link_to_img <- function(x, size = 16) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

# load additional library dplyr
library(dplyr)

# add image links to results data frame
other_long <- left_join(emoji_pic, other_long, by = c("Value"="Value"))

other_long <- other_long[complete.cases(other_long),]

# convert value column to factor again
other_long$Value <- factor(other_long$Value,
                           levels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros", "Sens", "Inv", "Ins", "Unp", "n/a"), 
                           labels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros", "Sens", "Inv", "Ins", "Unp", "n/a"))

# load additional libraries for plotting
library(png)
library(ggtext)
library(grid)

# Get images for plot legend
img_pos <- readPNG("figure_images/u2295-c2.png")
img_neg <- readPNG("figure_images/u2296-c2.png")
img_neut <- readPNG("figure_images/u29B6-c.png")

# make the plot
poall <- ggplot(other_long, aes(x = Test, 
                                y = DM,
                                shape=Value, 
                                colour=Value,
                                label=Value)) +
  geom_text(data=other_long[other_long$Value=="All" | 
                              other_long$Value=="None" | 
                              other_long$Value=="Zeros" |
                              other_long$Value=="Sens" |
                              other_long$Value=="Inv" |
                              other_long$Value=="Ins" |
                              other_long$Value=="Unp" |
                              other_long$Value=="n/a",], 
            aes(label=Value, colour=Value), size=4, fontface = "bold", show.legend=FALSE) +
  scale_colour_manual(values=c("black", "black", "black", 
                               "black", "dark blue", "darkgoldenrod4",
                               "purple4", "dark green", "brown1", "deepskyblue4", "dark red"), 
                      #limits=c("Positive", "Negative", "Neutral"),
                      name="Antiparallelism Bias",
                      breaks=c("Neutral", "Positive", "Negative", "All", "None", 
                               "Zeros", "Sens", "Inv", "Ins", "Unp", "n/a")) +
  geom_richtext(data = other_long[other_long$Value=="Positive" | 
                                    other_long$Value=="Negative" |
                                    other_long$Value=="Neutral",], 
                aes(label=func_link_to_img(emoji_link)),
                fill= NA,label.color = NA, show.legend=NA) + # remove background and outline
  scale_x_discrete(labels=c("Antiparallelism", "Non-Positive \n Value Handling", "Phase Inv.", "Uniform Time \n Scaling Inv.", "Warping Inv."))+
  facet_wrap(Type~., scales="free_y", ncol=2) +
  ggtitle(paste("Time-Based Invariances & Other Test Results")) +
  labs(caption = "\n Sens = Sensitive, Ins = Insensitive, Inv = Invariant, Unp = Unpredictable
\n *For this distance measure, results differ depending on the threshold value, epsilon. 
  Here, epsilon was set to 0.1.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=14, angle=90, vjust=0.5, hjust=1),
        axis.text.y = element_text(size=14),
        legend.position = c(1.08, -0.25),
        strip.text.x = element_text(size=12),
        plot.caption.position = "plot",
        plot.caption = element_text(size=10, hjust=0),
        legend.direction = "horizontal")+
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.02, 
                               label.position = "bottom", label.hjust = 0.4, label.vjust = 0.4, 
                               nrow = 1, title.theme = element_text(size=12),
                               label.theme = element_text(size=10, angle = 0)))

# get the grob data
poall.grob <- ggplotGrob(poall)

# adjust the heights of the facets
poall.grob$heights[8] = unit(3, "null")
poall.grob$heights[13] = unit(4, "null")
poall.grob$heights[18] = unit(6, "null")
poall.grob$heights[23] = unit(5, "null")
poall.grob$heights[28] = unit(1, "null")
poall.grob$heights[33] = unit(2, "null")

# adjust colours
poall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- 
  c("dark blue", "grey30", "grey30", "grey30")
# add bold
poall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$font <-
  as.integer(c(2, 1, 1, 1))


# save plot as tiff file
if(!dir.exists("figures/")) {dir.create("figures/")}
tiff(filename="figures/other_.tiff", width=7480, height=10255,
     units="px", res=1000, pointsize = 10, compression = "lzw")

grid::grid.newpage()
grid::grid.draw(poall.grob)


# search for legend keys using regular expressions
Tree = as.character(current.vpTree())
pos = gregexpr("\\[key.*?\\]", Tree)
match = unlist(regmatches(Tree, pos))

match = gsub("^\\[(key.*?)\\]$", "\\1", match) # remove square brackets
match = match[!grepl("bg", match)]  # removes matches containing bg
match

# Change the legend keys to the images
downViewport(match[9])
grid.rect(gp=gpar(col = NA, fill = "white"))
grid.raster(img_neg, interpolate=FALSE)
upViewport(0)

downViewport(match[5])
grid.rect(gp=gpar(col = NA, fill = "white"))
grid.raster(img_pos, interpolate=FALSE)
upViewport(0)

downViewport(match[6])
grid.rect(gp=gpar(col = NA, fill = "white"))
grid.raster(img_neut, interpolate=FALSE)
upViewport(0)

dev.off()

