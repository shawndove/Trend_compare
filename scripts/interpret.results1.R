  #metric results ----
  library(tidyr)
  library(ggplot2)

  dist.names <- c(dist.nameslist1, dist.nameslist2)
  setwd("C:/R_projects/Trend_compare/")
  
  interpreted.results <- list()

  for (i in seq_along(dist.names)) {
    
    print_name <- dist.names[[i]]
    
    metric_tr <- vector()
    
    temp <- readRDS(file = paste(wd, "/files/wide_temp/", print_name, "_dfwide.RData", sep=""))
    
    temp_ti <- readRDS(file = paste(wd, "/files/ti_results_temp/", print_name, "_tifull.RData", sep=""))
    ti_l <- length(temp_ti)
    
    temp_nn <- readRDS(file = paste(wd, "/files/nn_results_temp/", print_name, "_nnfull.RData", sep=""))
    
    interpreted.results$"Uniqueness"[i] <- ifelse(temp[1,2] == 0, "Yes", "No")
    metric_tr[1] <- ifelse(temp[1,2] == 0, TRUE, FALSE)
    
    interpreted.results$"Symmetry"[i] <- ifelse(temp[2,3] == temp[2,2], "Yes", "No")
    metric_tr[2] <- ifelse(temp[2,3] == temp[2,2], TRUE, FALSE)
    
    interpreted.results$"Non-negativity"[i] <- ifelse(temp_nn[[2]] == TRUE, "Yes", "No")
    metric_tr[3] <- ifelse(temp_nn[[2]] == TRUE, TRUE, FALSE)
    
    interpreted.results$"Triangle Inequality"[i] <- ifelse(temp_ti[[ti_l-3]] == TRUE, "Yes", "No")
    metric_tr[4] <- ifelse(temp_ti[[ti_l-3]] == TRUE, TRUE, FALSE)
    
    interpreted.results$"Metric"[i] <- ifelse(all(metric_tr), "Full", ifelse(all(metric_tr[1:3]), "Semi", "Non"))
    
  }
  
  interpreted.results.1 <- do.call(rbind, interpreted.results)
  interpreted.results.2 <- as.data.frame(t(interpreted.results.1))
  interpreted.results.2$"Distance Measure" <- dist.names
  
  mr <- interpreted.results.2
  
  mr$Type <- c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", "Elastic", "Elastic", 
               "Feature", "Feature",
                "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
                "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon_Entropy", "Shannon_Entropy",
                "Intersection", "Shannon_Entropy", "Other", "Shannon_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
                "Squared_L2", "Other", "Shannon_Entropy", "Intersection")
  mr$Type <- as.factor(mr$Type)
  
  metric_long <- tidyr::gather(mr, Test, Value, 1:5, factor_key=TRUE)
  metric_long <- metric_long[!is.nan(metric_long$Value),]
  
  metric_long$Type <- factor(metric_long$Type, levels=c("Minkowski", "Intersection", "L1", "Elastic",  "Squared_L2", "Other", 
                                                    "Shannon_Entropy", "Feature", "Fidelity", "Model", "Inner_Product", "Compression"),
                           labels=c("Minkowski Family", "Intersection Family", "L1 Family", "Elastic", "Squared L2 Family", 
                                    "Other Shape-Based","Shannon Entropy Family", "Feature-Based", "Fidelity Family", "Model-Based", 
                                    "Inner Product Family", "Compression-Based"))
  names(metric_long) <- c("DM", "Type", "Test", "Value")
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
  metric_long$Value <- factor(metric_long$Value)
  metric_long$Starred <- rep("No", times=215)
  metric_long$Starred[metric_long$DM=="*Soergel" | 
                        metric_long$DM=="*Clark" | 
                        metric_long$DM=="*Canb" | 
                        metric_long$DM=="*Wave" |
                        metric_long$DM=="*Additive" | 
                        metric_long$DM=="*Czek" | 
                        metric_long$DM=="*Kulcz" | 
                        metric_long$DM=="*ProbSymm" |
                        metric_long$DM=="*SqChi"] <- "Yes"
  
  metric_long$Dagger <- rep("No", times=215)
  metric_long$Dagger[metric_long$Dagger=="\u2020EDR"] <- "Yes"
  
  ml_ordered <- metric_long[order(metric_long$Type),]
  ml_ordered <- ml_ordered[ml_ordered$Test=="Uniqueness",]

  # make the plot
  pmall <- ggplot(metric_long, aes(x = Test, 
                                y = DM,
                                shape=Value, 
                                colour=Value,
                                label=Value)) +
    geom_point(size=4) +
    geom_text(data=metric_long[metric_long$Value=="Semi" | metric_long$Value=="Full" | metric_long$Value=="Non",], 
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
  pmall.grob$grobs[[45]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- c("dark blue", "dark blue", "grey30", "grey30")
  pmall.grob$grobs[[46]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- c("dark blue", "dark blue", "dark blue", "dark blue", "grey30", "grey30")
  pmall.grob$grobs[[47]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
  pmall.grob$grobs[[48]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
  pmall.grob$grobs[[49]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
  pmall.grob$grobs[[38]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- c("dark blue", "dark blue", "dark blue")
  pmall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- c("dark red", "grey30", "grey30", "grey30")
  pmall.grob$grobs[[40]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
  pmall.grob$grobs[[41]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
  pmall.grob$grobs[[42]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
  pmall.grob$grobs[[43]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- "grey30"
  
  # draw the adjusted plot
  grid::grid.newpage()
  grid::grid.draw(pmall.grob)
  
  # save the adjusted plot
  ggsave(paste(wd, "/figures/metric_test_results.tiff", sep=""), 
         pmall.grob, width=7480, height=10255, units="px", dpi=1000, scale=1, compression="lzw")
  
  
  # sensitivity results ----
  
temp_levels <- list()
wd <- getwd()
for (i in seq_along(dist.names)) {
  print_name <- dist.names[i]
  
  temp_levels1 <- readRDS(file = paste(wd, "/files/wide_temp/", print_name, "_dfwide.RData", sep=""))
  
  temp_levels[[i]] <- temp_levels1[20,]
  
}

levels <- do.call(rbind, temp_levels)
rownames(levels) <- dist.names
levels_1 <- levels[,1:2]
levels_1[,2] <- round(levels_1[,2], digits=3)
levels_1 <- levels_1[levels_1[,2] >= 0,]
levels_1 <- levels_1[!is.na(levels_1[,2]),]
levels_1 <- levels_1[!is.nan(levels_1[,2]),]
levels_1$Levels <- cut(levels_1[,2], breaks=c(0,0.2,0.7,1.3,2.5,20), right=FALSE, ordered_result=TRUE, labels=c("Very Low", "Low", "Med", "High", "Very High"))

levels_2 <- levels[,c(1,3)]
levels_2[,2] <- round(levels_2[,2], digits=3)
levels_2 <- levels_2[levels_2[,2] >= 0,]
levels_2 <- levels_2[!is.na(levels_2[,2]),]
levels_2 <- levels_2[!is.nan(levels_2[,2]),]
levels_2$Levels <- cut(levels_2[,2], breaks=c(0,0.2,0.7,1.3,2.5,20), right=FALSE, ordered_result=TRUE, labels=c("Very Low", "Low", "Med", "High", "Very High"))

levels_3 <- levels[,c(1,4)]
levels_3[,2] <- round(levels_3[,2], digits=3)
levels_3 <- levels_3[levels_3[,2] >= 0,]
levels_3 <- levels_3[!is.na(levels_3[,2]),]
levels_3 <- levels_3[!is.nan(levels_3[,2]),]
levels_3$Levels <- cut(levels_3[,2], breaks=c(0,0.2,0.7,1.3,2.5,20), right=FALSE, ordered_result=TRUE, labels=c("Very Low", "Low", "Med", "High", "Very High"))

levels_4 <- levels[,c(1,5)]
levels_4[,2] <- round(levels_4[,2], digits=3)
levels_4 <- levels_4[levels_4[,2] >= 0,]
levels_4 <- levels_4[!is.na(levels_4[,2]),]
levels_4 <- levels_4[!is.nan(levels_4[,2]),]
levels_4$Levels <- cut(levels_4[,2], breaks=c(0,0.2,0.7,1.3,2.5,20), right=FALSE, ordered_result=TRUE, labels=c("Very Low", "Low", "Med", "High", "Very High"))

levels_5 <- levels[,c(1,6)]
levels_5[,2] <- round(levels_5[,2], digits=3)
levels_5 <- levels_5[levels_5[,2] >= 0,]
levels_5 <- levels_5[!is.na(levels_5[,2]),]
levels_5 <- levels_5[!is.nan(levels_5[,2]),]
levels_5$Levels <- cut(levels_5[,2], breaks=c(0,0.2,0.7,1.3,2.5,20), right=FALSE, ordered_result=TRUE, labels=c("Very Low", "Low", "Med", "High", "Very High"))

levels_6 <- levels[,c(1,7)]
levels_6[,2] <- round(levels_6[,2], digits=3)
levels_6 <- levels_6[levels_6[,2] >= 0,]
levels_6 <- levels_6[!is.na(levels_6[,2]),]
levels_6 <- levels_6[!is.nan(levels_6[,2]),]
levels_6$Levels <- cut(levels_6[,2], breaks=c(0,0.2,0.7,1.3,2.5,20), right=FALSE, ordered_result=TRUE, labels=c("Very Low", "Low", "Med", "High", "Very High"))

levels_7 <- levels[,c(1,8)]
levels_7[,2] <- round(levels_7[,2], digits=3)
levels_7 <- levels_7[levels_7[,2] >= 0,]
levels_7 <- levels_7[!is.na(levels_7[,2]),]
levels_7 <- levels_7[!is.nan(levels_7[,2]),]
levels_7$Levels <- cut(levels_7[,2], breaks=c(0,0.2,0.7,1.3,2.5,20), right=FALSE, ordered_result=TRUE, labels=c("Very Low", "Low", "Med", "High", "Very High"))

interpreted.results <- list()

for (i in seq_along(dist.names)) {
  
  print_name <- dist.names[[i]]
  
  metric_tr <- vector()
  
  temp <- readRDS(file = paste(wd, "/files/wide_temp/", print_name, "_dfwide.RData", sep=""))
  
  interpreted.results$"Translation Sensitivity"[i] <-
    ifelse(all(abs(temp[3,2:5]) < 0.000001), "Inv",
           ifelse((max(temp[3,2:5]) - min(temp[3,2:5])) < 0.000001, "Very Low", 
           ifelse((temp[3,5] > temp[3,4] & temp[3,4] > temp[3,3] & temp[3,3] > temp[3,2]) |
             (temp[3,5] < temp[3,4] & temp[3,4] < temp[3,3] & temp[3,3] < temp[3,2]),
                  as.character(levels_1[print_name,]$Levels), "Unpredictable")))
  
  interpreted.results$"Amplitude Sensitivity"[i] <-
    ifelse(all(abs(temp[4,2:5]) < 0.000001), "Inv",
           ifelse((max(temp[4,2:5]) - min(temp[4,2:5])) < 0.000001, "Very Low", 
           ifelse((temp[4,5] > temp[4,4] & temp[4,4] > temp[4,3] & temp[4,3] > temp[4,2]) |
                    (temp[4,5] < temp[4,4] & temp[4,4] < temp[4,3] & temp[4,3] < temp[4,2]),
                  as.character(levels_2[print_name,]$Levels), "Unpredictable")))
  
  interpreted.results$"Duration Sensitivity"[i] <-
    ifelse(all(abs(temp[5,2:5]) < 0.000001), "Inv",
           ifelse((max(temp[5,2:5]) - min(temp[5,2:5])) < 0.000001, "Very Low", 
           ifelse((temp[5,5] > temp[5,4] & temp[5,4] > temp[5,3] & temp[5,3] > temp[5,2]) |
                    (temp[5,5] < temp[5,4] & temp[5,4] < temp[5,3] & temp[5,3] < temp[5,2]),
                  as.character(levels_3[print_name,]$Levels), "Unpredictable")))
  
  interpreted.results$"Frequency Sensitivity"[i] <-
    ifelse(all(abs(temp[6,2:5]) < 0.000001), "Inv",
           ifelse((max(temp[6,2:5]) - min(temp[6,2:5])) < 0.000001, "Very Low",
           ifelse((temp[6,5] > temp[6,4] & temp[6,4] > temp[6,3] & temp[6,3] > temp[6,2]) |
                    (temp[6,5] < temp[6,4] & temp[6,4] < temp[6,3] & temp[6,3] < temp[6,2]),
                  as.character(levels_4[print_name,]$Levels), "Unpredictable")))
  
  interpreted.results$"White Noise Sensitivity"[i] <-
    ifelse(all(abs(temp[9,2:5]) < 0.000001), "Inv",
           ifelse((max(temp[9,2:5]) - min(temp[9,2:5])) < 0.000001, "Very Low", 
           ifelse((temp[9,5] > temp[9,4] & temp[9,4] > temp[9,3] & temp[9,3] > temp[9,2]) |
                    (temp[9,5] < temp[9,4] & temp[9,4] < temp[9,3] & temp[9,3] < temp[9,2]),
                  as.character(levels_5[print_name,]$Levels), "Unpredictable")))
  
  interpreted.results$"Biased Noise Sensitivity"[i] <-
    ifelse(all(abs(temp[10,2:5]) < 0.000001), "Inv",
           ifelse((max(temp[10,2:5]) - min(temp[10,2:5])) < 0.000001, "Very Low", 
           ifelse((temp[10,5] > temp[10,4] & temp[10,4] > temp[10,3] & temp[10,3] > temp[10,2]) |
                    (temp[10,5] < temp[10,4] & temp[10,4] < temp[10,3] & temp[10,3] < temp[10,2]),
                  as.character(levels_6[print_name,]$Levels), "Unpredictable")))
  
  interpreted.results$"Outlier Sensitivity"[i] <-
    ifelse(all(abs(temp[11,2:5]) < 0.000001), "Inv",
           ifelse((max(temp[11,2:5]) - min(temp[11,2:5])) < 0.000001, "Very Low", 
           ifelse((temp[11,5] > temp[11,4] & temp[11,4] > temp[11,3] & temp[11,3] > temp[11,2]) |
                    (temp[11,5] < temp[11,4] & temp[11,4] < temp[11,3] & temp[11,3] < temp[11,2]),
                  as.character(levels_7[print_name,]$Levels), "Unpredictable")))
  
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
names(rsr) <- c("Test",temp_levels1$Test[c(3:6,9:11)],"DM")

rsr$Type <- c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", 
              "Elastic", "Elastic", "Feature", "Feature",
              "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
              "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon_Entropy", "Shannon_Entropy",
              "Intersection", "Shannon_Entropy", "Other", "Shannon_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
              "Squared_L2", "Other", "Shannon_Entropy", "Intersection")
rsr$Type <- as.factor(rsr$Type)

test <- rsr[,2:10]

test_long <- tidyr::gather(test, Test, Value, 1:7, factor_key=TRUE)

test_long$Value[is.nan(test_long$Value)] <- 0

test_long$Level <- c(interpreted.results.2[,1], interpreted.results.2[,2], interpreted.results.2[,3],
                     interpreted.results.2[,4], interpreted.results.2[,5], interpreted.results.2[,6],
                     interpreted.results.2[,7])

#test_long$Value[test_long$DM=="NCD" | test_long$DM=="CDM"] <- c(0, 0, -1, -1, -1, -1, -1, -1, -1, -1,
#                                                                -1, -1, -1, -1)

#test_long$Level[test_long$DM=="NCD" | test_long$DM=="CDM"] <- c(2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#                                                                0, 0)

# TAM appeared from our tests to be invariant to white noise, but further testing revealed this to be 
# incorrect at higher values.
test_long$Level[test_long$DM=="TAM" & test_long$Test=="White Noise Sensitivity"] <- "Unpredictable"

test_long$Level <- factor(test_long$Level, 
                          levels=c(1,2,3,4,5,6, 7), 
                          labels=c("Unpredictable","Invariant", "Very Low", "Low", "Medium", "High", "Very High"))

test_long$Type <- factor(test_long$Type, levels=c("Minkowski", "Intersection", "L1", "Elastic",  "Squared_L2", "Other", 
                                                  "Shannon_Entropy", "Feature", "Fidelity", "Model", "Inner_Product", "Compression"),
                         labels=c("Minkowski Family", "Intersection Family", "L1 Family", "Elastic", "Squared L2 Family", 
                                  "Other Shape-Based","Shannon Entropy Family", "Feature-Based", "Fidelity Family", "Model-Based", 
                                  "Inner Product Family", "Compression-Based"))

test_long$DM[test_long$DM=="KumarJohnson"]<-"Kumar"
test_long$DM[test_long$DM=="WaveHedges"]<-"Wave"
test_long$DM[test_long$DM=="EDR"]<-"*EDR"
#test_long$DM[test_long$DM=="NCD"]<-"\u2020NCD"
#test_long$DM[test_long$DM=="CDM"]<-"\u2020CDM"

test_long$Value[test_long$Level=="Unpredictable"] <- -1

#test_long$Point <- rep("Yes", times=301)
#test_long$Point[test_long$DM=="NCD" | test_long$DM=="CDM"] <- "No"
#test_long$Point[7] <- "Yes"
#test_long$Point[8] <- "Yes"
#test_long$Point <- factor(test_long$Point, levels=c("Yes", "No"), labels=c("Yes", "No"))

#cbPalette <- c("snow", scales::seq_gradient_pal("skyblue", "orange", "Lab")(c(0, 0.2, 0.4, 0.7, 1)))
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
\n *The results for EDR strongly depended on the threshold setting, epsilon. Here, it was set to 0.") +
  ggtitle(paste("Sensitivity Test Results")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=14, angle=90, vjust=0.2, hjust=1),
        axis.text.y = element_text(size=14),
        #legend.text = element_text(size=12),
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

ptall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- c("dark blue", "grey30", "grey30", "grey30")

grid::grid.newpage()
grid::grid.draw(ptall.grob)
        
ggsave(paste(wd, "/figures/sens_test_results.tiff", sep=""), 
       ptall.grob, width=7480, height=10255, units="px", dpi=1000, scale=1, compression="lzw")

# other properties ----

interpreted.results <- list()

for (i in seq_along(dist.names)) {
  
  print_name <- dist.names[[i]]
  
  metric_tr <- vector()
  
  temp <- readRDS(file = paste(wd, "/files/wide_temp/", print_name, "_dfwide.RData", sep=""))

  interpreted.results$"Antiparallelism Bias"[i] <- 
    ifelse(temp[12,3] == temp[12,2], "Neutral", 
           ifelse(temp[12,3] > temp[12,2], "Positive", "Negative"))
  
  interpreted.results$"Non-positive Value Handling"[i] <-
    ifelse(!is.na(temp[17,3]) & !is.nan(temp[17,3]), 
           ifelse(!is.na(temp[17,4]) & !is.nan(temp[17,4]), "All", "Zeros Only"), 
           ifelse(!is.na(temp[17,4]) & !is.nan(temp[17,4]), "Negative Only", "None"))
  
  interpreted.results$"Phase Invariance"[i] <-
    ifelse(all(abs(temp[13,2:5]) < 0.000001), "Invariant", 
           ifelse((temp[13,5] > temp[13,4] & temp[13,4] > temp[13,3] & temp[13,3] > temp[13,2]) |
                  (temp[13,5] < temp[13,4] & temp[13,4] < temp[13,3] & temp[13,3] < temp[13,2]) , "Sensitive", 
                  ifelse((max(temp[13,2:5]) - min(temp[13,2:5])) < 0.000001, "Insensitive", "Unpredictable")))
  
  interpreted.results$"Uniform Time Scaling Invariance"[i] <-
    ifelse(all(is.na(temp[14,2:5])), "NA", 
           ifelse(all(abs(temp[14,2:5]) < 0.000001), "Invariant", 
                  ifelse((temp[14,5] > temp[14,4] & temp[14,4] > temp[14,3] & temp[14,3] > temp[14,2]) |
                         (temp[14,5] < temp[14,4] & temp[14,4] < temp[14,3] & temp[14,3] < temp[14,2]), "Sensitive", 
                         ifelse((max(temp[14,2:5]) - min(temp[14,2:5])) < 0.000001, "Insensitive", "Unpredictable"))))
  
  interpreted.results$"Warping Invariance"[i] <-
    ifelse(all(is.na(temp[15,2:5])), "NA", 
           ifelse(all(abs(temp[15,2:5]) < 0.000001), "Invariant", 
                  ifelse((temp[15,5] > temp[15,4] & temp[15,4] > temp[15,3] & temp[15,3] > temp[15,2]) |
                         (temp[15,5] < temp[15,4] & temp[15,4] < temp[15,3] & temp[15,3] < temp[15,2]), "Sensitive", 
                         ifelse((max(temp[15,2:5]) - min(temp[15,2:5])) < 0.000001, "Insensitive", "Unpredictable"))))
  
}

interpreted.results.1 <- do.call(rbind, interpreted.results)
interpreted.results.2 <- as.data.frame(t(interpreted.results.1))

for (i in 1:ncol(interpreted.results.2)) {
  
  interpreted.results.2[,i] <- factor(interpreted.results.2[,i],
                                      levels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros Only", "Negative Only", "Sensitive", "Invariant", "Insensitive", "Unpredictable", "NA"), 
                                      labels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros Only", "Negative Only", "Sensitive", "Invariant", "Insensitive", "Unpredictable", "NA"))
  
}

interpreted.results.2$"Distance Measure" <- dist.names

or <- interpreted.results.2

or$Type <- c("Minkowski", "Minkowski", "Minkowski", "Other", "Elastic", "Elastic", "Compression", "Compression", 
             "Elastic", "Elastic", "Feature", "Feature",
             "Feature", "Feature", "Feature", "Model", "Other", "Other", "Squared_L2", "Other", "L1", "Squared_L2",
             "Intersection", "Inner_Product", "Squared_L2", "L1", "Inner_Product", "Shannon_Entropy", "Shannon_Entropy",
             "Intersection", "Shannon_Entropy", "Other", "Shannon_Entropy", "L1", "Squared_L2", "L1", "Squared_L2", "Fidelity",
             "Squared_L2", "Other", "Shannon_Entropy", "Intersection")
or$Type <- as.factor(or$Type)

other_long <- tidyr::gather(or, Test, Value, 1:5, factor_key=TRUE)

other_long$Type <- factor(other_long$Type, levels=c("Minkowski", "Intersection", "L1", "Elastic",  "Squared_L2", "Other", 
                                                      "Shannon_Entropy", "Feature", "Fidelity", "Model", "Inner_Product", "Compression"),
                           labels=c("Minkowski Family", "Intersection Family", "L1 Family", "Elastic", "Squared L2 Family", 
                                    "Other Shape-Based","Shannon Entropy Family", "Feature-Based", "Fidelity Family", "Model-Based", 
                                    "Inner Product Family", "Compression-Based"))

names(other_long) <- c("DM", "Type", "Test", "Value")

other_long$DM[other_long$DM=="KumarJohnson"]<-"Kumar"
other_long$DM[other_long$DM=="WaveHedges"]<-"Wave"
other_long$DM[other_long$DM=="EDR"]<-"*EDR"

other_long$Value <- factor(other_long$Value,
                            levels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros Only", "Sensitive", "Invariant", "Insensitive", "Unpredictable", "NA"), 
                            labels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros", "Sens", "Inv", "Ins", "Unp", "n/a"))

emoji_pic<-data.frame(
  Value = c("Neutral", "Positive", "Negative", 
            "All", "None", "Zeros",
            "Sens", "Inv", "Ins", "Unp", "n/a"),
  emoji_link = c("c:/R_projects/Trend_compare/figures/u29B6-c.png",
                 "c:/R_projects/Trend_compare/figures/u2295-c2.png",
                 "c:/R_projects/Trend_compare/figures/u2296-c2.png",
                 "c:/R_projects/Trend_compare/figures/black-circle_26ab.png",
                 "c:/R_projects/Trend_compare/figures/black-circle_26ab.png",
                 "c:/R_projects/Trend_compare/figures/black-circle_26ab.png",
                 "c:/R_projects/Trend_compare/figures/black-circle_26ab.png",
                 "c:/R_projects/Trend_compare/figures/black-circle_26ab.png",
                 "c:/R_projects/Trend_compare/figures/black-circle_26ab.png",
                 "c:/R_projects/Trend_compare/figures/black-circle_26ab.png",
                 "c:/R_projects/Trend_compare/figures/black-circle_26ab.png"))

func_link_to_img <- function(x, size = 16) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

library(dplyr)
other_long <- left_join(emoji_pic, other_long, by = c("Value"="Value"))

other_long$Value <- factor(other_long$Value,
                           levels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros", "Sens", "Inv", "Ins", "Unp", "n/a"), 
                           labels=c("Neutral", "Positive", "Negative", "All", "None", "Zeros", "Sens", "Inv", "Ins", "Unp", "n/a"))

library(png)
library(ggtext)
library(grid)
# Get image
img_pos <- readPNG("c:/R_projects/Trend_compare/figures/u2295-c2.png")
img_neg <- readPNG("c:/R_projects/Trend_compare/figures/u2296-c2.png")
img_neut <- readPNG("c:/R_projects/Trend_compare/figures/u29B6-c.png")

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
  Here, epsilon was set to 0.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=14, angle=90, vjust=0.5, hjust=1),
        axis.text.y = element_text(size=14),
        legend.position = c(1.1, -0.25),
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

poall.grob$grobs[[39]]$children[[2]]$grobs[[1]]$children[[1]]$gp$col <- c("dark blue", "grey30", "grey30", "grey30")

tiff(filename="C:/R_projects/Trend_compare/figures/other.tiff", width=7480, height=10255,
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
downViewport(match[10])
grid.rect(gp=gpar(col = NA, fill = "white"))
grid.raster(img_neg, interpolate=FALSE)
upViewport(0)

downViewport(match[6])
grid.rect(gp=gpar(col = NA, fill = "white"))
grid.raster(img_pos, interpolate=FALSE)
upViewport(0)

downViewport(match[7])
grid.rect(gp=gpar(col = NA, fill = "white"))
grid.raster(img_neut, interpolate=FALSE)
upViewport(0)

dev.off()

