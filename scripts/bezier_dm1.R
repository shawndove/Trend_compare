# Define curves to test distance measures for desirable properties
# Shawn Dove (s.dove@ucl.ac.uk) - November, 2020

# packages ----
library(bezier)
library(TSdist)
library(RANN)
library(reshape2)
library(ggplot2)


# set working directory ----
setwd("c:/R_projects/Trend_compare/plots/curves")

# relative duration of deviation ----

pval1.dur <- matrix(c(0,0, 2,2, 2,1, 4,3, 4,2, 6,4, 6,3, 8,5, 8,4, 10,6, 10,5, 12,7, 12,6), 
                    nrow=13, 
                    ncol=2, 
                    byrow=TRUE)

pval2.dur <- matrix(c(0,0, 2,2, 2,1, 4,3, 4,2, 6,4, 6,3, 8,5, 8,4, 10,6, 10,5, 11,7, 12,6), 
                    nrow=13, 
                    ncol=2, 
                    byrow=TRUE)

pval3.dur <- matrix(c(0,0, 2,2, 2,1, 4,3, 4,2, 6,4, 6,3, 8,5, 8,4, 9,6, 10,5, 11,7, 12,6), 
                    nrow=13, 
                    ncol=2, 
                    byrow=TRUE)

pval4.dur <- matrix(c(0,0, 2,2, 2,1, 4,3, 4,2, 6,4, 6,3, 7,5, 8,4, 9,6, 10,5, 11,7, 12,6), 
                    nrow=13, 
                    ncol=2, 
                    byrow=TRUE)

pval5.dur <- matrix(c(0,0, 2,2, 2,1, 4,3, 4,2, 5,4, 6,3, 7,5, 8,4, 9,6, 10,5, 11,7, 12,6), 
                    nrow=13, 
                    ncol=2, 
                    byrow=TRUE)

pval6.dur <- matrix(c(0,0, 2,2, 2,1, 3,3, 4,2, 5,4, 6,3, 7,5, 8,4, 9,6, 10,5, 11,7, 12,6), 
                    nrow=13, 
                    ncol=2, 
                    byrow=TRUE)

pval7.dur <- matrix(c(0,0, 1,2, 2,1, 3,3, 4,2, 5,4, 6,3, 7,5, 8,4, 9,6, 10,5, 11,7, 12,6), 
                    nrow=13, 
                    ncol=2, 
                    byrow=TRUE)

pval8.dur <- matrix(c(2,0, 4,2, 4,1, 6,3, 6,2, 8,4, 8,3, 10,5, 10,4, 12,6, 12,5), 
                    nrow=11, 
                    ncol=2, 
                    byrow=TRUE)

pval9.dur <- matrix(c(2,0, 4,2, 4,1, 6,3, 6,2, 8,4, 8,3, 10,5, 10,4, 11,6, 12,5), 
                    nrow=11, 
                    ncol=2, 
                    byrow=TRUE)

pval10.dur <- matrix(c(2,0, 4,2, 4,1, 6,3, 6,2, 8,4, 8,3, 9,5, 10,4, 11,6, 12,5), 
                     nrow=11, 
                     ncol=2, 
                     byrow=TRUE)

pval11.dur <- matrix(c(2,0, 4,2, 4,1, 6,3, 6,2, 7,4, 8,3, 9,5, 10,4, 11,6, 12,5), 
                     nrow=11, 
                     ncol=2, 
                     byrow=TRUE)

pval12.dur <- matrix(c(2,0, 4,2, 4,1, 5,3, 6,2, 7,4, 8,3, 9,5, 10,4, 11,6, 12,5), 
                     nrow=11, 
                     ncol=2, 
                     byrow=TRUE)

pval13.dur <- matrix(c(2,0, 3,2, 4,1, 5,3, 6,2, 7,4, 8,3, 9,5, 10,4, 11,6, 12,5), 
                     nrow=11, 
                     ncol=2, 
                     byrow=TRUE)

bezier1.dur <- pointsOnBezier(pval1.dur, 
                              n=150, 
                              deg=2)

bezier2.dur <- pointsOnBezier(pval2.dur, 
                              n=150, 
                              deg=2)

bezier3.dur <- pointsOnBezier(pval3.dur, 
                              n=150, 
                              deg=2)

bezier4.dur <- pointsOnBezier(pval4.dur, 
                              n=150, 
                              deg=2)

bezier5.dur <- pointsOnBezier(pval5.dur, 
                              n=150, 
                              deg=2)

bezier6.dur <- pointsOnBezier(pval6.dur, 
                              n=150, 
                              deg=2)

bezier7.dur <- pointsOnBezier(pval7.dur, 
                              n=150, 
                              deg=2)

bezier8.dur <- pointsOnBezier(pval8.dur, 
                              n=125, 
                              deg=2)

bezier9.dur <- pointsOnBezier(pval9.dur, 
                              n=125, 
                              deg=2)

bezier10.dur <- pointsOnBezier(pval10.dur, 
                               n=125, 
                               deg=2)

bezier11.dur <- pointsOnBezier(pval11.dur, 
                               n=125, 
                               deg=2)

bezier12.dur <- pointsOnBezier(pval12.dur, 
                               n=125, 
                               deg=2)

bezier13.dur <- pointsOnBezier(pval13.dur, 
                               n=125, 
                               deg=2)

bz_x.dur <- list(bezier2.dur$points, 
                 bezier3.dur$points, 
                 bezier4.dur$points, 
                 bezier5.dur$points, 
                 bezier6.dur$points, 
                 bezier7.dur$points, 
                 bezier9.dur$points, 
                 bezier10.dur$points, 
                 bezier11.dur$points, 
                 bezier12.dur$points, 
                 bezier13.dur$points)

bz_y.dur <- list(bezier1.dur$points, 
                 bezier1.dur$points, 
                 bezier1.dur$points, 
                 bezier1.dur$points, 
                 bezier1.dur$points, 
                 bezier1.dur$points, 
                 bezier8.dur$points, 
                 bezier8.dur$points, 
                 bezier8.dur$points, 
                 bezier8.dur$points, 
                 bezier8.dur$points)

tslistx.dur <- lapply(bz_x.dur, 
                      function(i) as.vector(i[, 2]))

tslisty.dur <- lapply(bz_y.dur, 
                      function(i) as.vector(i[, 2]))

res.duration <- dist.results(tslistx.dur,
                             tslisty.dur)

dur1.x <- data.frame(bezier1.dur$points[,1], 
                   bezier2.dur$points[,1], 
                   bezier3.dur$points[,1], 
                   bezier4.dur$points[,1], 
                   bezier5.dur$points[,1], 
                   bezier6.dur$points[,1], 
                   bezier7.dur$points[,1])

dur1.y <- data.frame(bezier1.dur$points[,2], 
                      bezier2.dur$points[,2], 
                      bezier3.dur$points[,2], 
                      bezier4.dur$points[,2], 
                      bezier5.dur$points[,2], 
                      bezier6.dur$points[,2], 
                      bezier7.dur$points[,2])

dur2.x <- data.frame(bezier8.dur$points[,1], 
                      bezier9.dur$points[,1], 
                      bezier10.dur$points[,1], 
                      bezier11.dur$points[,1], 
                      bezier12.dur$points[,1], 
                      bezier13.dur$points[,1])

dur2.y <- data.frame(bezier8.dur$points[,2], 
                     bezier9.dur$points[,2], 
                     bezier10.dur$points[,2], 
                     bezier11.dur$points[,2], 
                     bezier12.dur$points[,2], 
                     bezier13.dur$points[,2])

colnames(dur1.x) <- c("x1",
                    "x2", 
                    "x3", 
                    "x4",
                    "x5",
                    "x6",
                    "x7")

colnames(dur1.y) <- c("y1",
                       "y2", 
                       "y3", 
                       "y4",
                       "y5",
                       "y6",
                       "y7")

colnames(dur2.x) <- c("x8",
                       "x9",
                       "x10", 
                       "x11", 
                       "x12", 
                       "x13")

colnames(dur2.y) <- c("y8",
                      "y9",
                      "y10", 
                      "y11", 
                      "y12", 
                      "y13")

dur1.x$plot <- c(rep(6, times=25), 
                  rep(5, times=25), 
                  rep(4, times=25), 
                  rep(3, times=25), 
                  rep(2, times=25), 
                  rep(1, times=25))

dur1.y$plot <- c(rep(6, times=25), 
                  rep(5, times=25), 
                  rep(4, times=25), 
                  rep(3, times=25), 
                  rep(2, times=25), 
                  rep(1, times=25))

dur2.x$plot <- c(rep(5, times=25), 
                 rep(4, times=25), 
                 rep(3, times=25), 
                 rep(2, times=25), 
                 rep(1, times=25))

dur2.y$plot <- c(rep(5, times=25), 
                 rep(4, times=25), 
                 rep(3, times=25), 
                 rep(2, times=25), 
                 rep(1, times=25))

dur1.long.x <- melt(dur1.x, 
                    id.vars = c("plot"))

dur1.long.y <- melt(dur1.y, 
                    id.vars = c("plot"))

dur2.long.x <- melt(dur2.x,
                    id.vars = c("plot"))

dur2.long.y <- melt(dur2.y,
                    id.vars = c("plot"))

dur1.long <- data.frame(dur1.long.x, 
                        dur1.long.y)

dur1.long <- dur1.long[c(1,2,3,6)]

dur2.long <- data.frame(dur2.long.x, 
                        dur2.long.y)

dur2.long <- dur2.long[c(1,2,3,6)]

colnames(dur1.long) <- c("plot", 
                         "xfact", 
                         "x", 
                         "y")

colnames(dur2.long) <- c("plot", 
                         "xfact", 
                         "x", 
                         "y")

dur1.ref <- as.data.frame(bezier1.dur$points)

dur2.ref <- as.data.frame(bezier8.dur$points)

colnames(dur1.ref) <- c("x", 
                        "y")

colnames(dur2.ref) <- c("x", 
                        "y")

ggplot(data=dur1.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color=factor(plot)), 
             size=1, 
             position="jitter") +
  geom_line(data=dur1.ref, 
            aes(x = x, 
                y = y), 
            size=2) +
  geom_point(data=dur2.long, 
             aes(x = x,
                 y = y,
                 color=factor(plot)),
             size=1,
             position="jitter") +
  geom_line(data=dur2.ref,
            aes(x = x,
                y = y),
            size=2) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Deviation") +
  ggtitle("Relative Duration of Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("duration_curves.jpg")

#plot(bezier1.dur$points, type="o", ylim=c(-1,7), col="black")
#lines(bezier2.dur$points, type="o", col="green")
#lines(bezier3.dur$points, type="o", col="blue")
#lines(bezier4.dur$points, type="o", col="red")
#lines(bezier5.dur$points, type="o", col="yellow")
#lines(bezier6.dur$points, type="o", col="purple")
#lines(bezier7.dur$points, type="o", col="orange")

#plot(bezier8.dur$points, type="o", ylim=c(-1,6), col="black")
#lines(bezier9.dur$points, type="o", col="green")
#lines(bezier10.dur$points, type="o", col="blue")
#lines(bezier11.dur$points, type="o", col="red")
#lines(bezier12.dur$points, type="o", col="yellow")
#lines(bezier13.dur$points, type="o", col="purple")

temp_plots.dur <- list()

for (i in 1:length(res.duration)) {
  temp <- res.duration[[i]]
  if(length(temp) == 0){
    temp_plots.dur[[i]] <- NULL
  } else {
    plot(1:11, temp)
    temp_plots.dur[[i]] <- recordPlot()
  }
}

tval.test <- seq(0,1, 
                length=200)
pval1.test <- matrix(c(1,1, 2,3, 3,2, 4,1, 5,2), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

bezier_1.test <- bezier(tval.test, 
                       pval1.test)

bezier_2.test <- bezier_1.test
bezier_2.test[,2] <- 2 * bezier_2.test[,2]

bezier_3.test <- bezier_1.test
bezier_3.test[,2] <- bezier_1.test[,2] + 2

bz_x.test <- list(bezier_2.test, bezier_3.test)
bz_y.test <- list(bezier_1.test, bezier_1.test)

tslistx.test <- lapply(bz_x.test, 
                      function(i) as.vector(i[, 2]))

tslisty.test <- lapply(bz_y.test, 
                      function(i) as.vector(i[, 2]))

res.test <- dist.results(tslistx.test,
                              tslisty.test)

plot(bezier_1.test, ylim=c(0,6))
lines(bezier_2.test)
lines(bezier_3.test)

# amplitude of deviation ----

tval.amp <- seq(0,1, 
                length=100)

pval1.amp <- matrix(c(0,0,1,1,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval2.amp <- matrix(c(0,0,1,2,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval3.amp <- matrix(c(0,0,1,3,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval4.amp <- matrix(c(0,0,1,4,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval5.amp <- matrix(c(0,0,1,5,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval6.amp <- matrix(c(0,0,1,6,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval7.amp <- matrix(c(0,0,1,7,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval8.amp <- matrix(c(0,0,1,8,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval9.amp <- matrix(c(0,0,1,9,2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval10.amp <- matrix(c(0,0,1,10,2,0), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

pval11.amp <- matrix(c(0,0,1,11,2,0), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

pval12.amp <- matrix(c(0,0,1,12,2,0), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

bezier_1.amp <- bezier(tval.amp, 
                       pval1.amp)

bezier_2.amp <- bezier(tval.amp, 
                       pval2.amp)

bezier_3.amp <- bezier(tval.amp, 
                       pval3.amp)

bezier_4.amp <- bezier(tval.amp, 
                       pval4.amp)

bezier_5.amp <- bezier(tval.amp, 
                       pval5.amp)

bezier_6.amp <- bezier(tval.amp, 
                       pval6.amp)

bezier_7.amp <- bezier(tval.amp, 
                       pval7.amp)

bezier_8.amp <- bezier(tval.amp, 
                       pval8.amp)

bezier_9.amp <- bezier(tval.amp, 
                       pval9.amp)

bezier_10.amp <- bezier(tval.amp, 
                        pval10.amp)

bezier_11.amp <- bezier(tval.amp, 
                        pval11.amp)

bezier_12.amp <- bezier(tval.amp, 
                        pval12.amp)


bz_x.amp <- list(bezier_2.amp, 
                 bezier_3.amp, 
                 bezier_4.amp, 
                 bezier_5.amp, 
                 bezier_6.amp, 
                 bezier_7.amp, 
                 bezier_8.amp, 
                 bezier_9.amp, 
                 bezier_10.amp, 
                 bezier_11.amp, 
                 bezier_12.amp)

bz_y.amp <- list(bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp, 
                 bezier_1.amp)

tslistx.amp <- lapply(bz_x.amp, 
                      function(i) as.vector(i[, 2]))

tslisty.amp <- lapply(bz_y.amp, 
                      function(i) as.vector(i[, 2]))

res.amplitude <- dist.results(tslistx.amp,
                              tslisty.amp)

amp.x <- data.frame(bezier_2.amp[,1], 
                    bezier_3.amp[,1], 
                    bezier_4.amp[,1], 
                    bezier_5.amp[,1], 
                    bezier_6.amp[,1], 
                    bezier_7.amp[,1], 
                    bezier_8.amp[,1], 
                    bezier_9.amp[,1], 
                    bezier_10.amp[,1], 
                    bezier_11.amp[,1], 
                    bezier_12.amp[,1])

amp.y <- data.frame(bezier_2.amp[,2],
                    bezier_3.amp[,2], 
                    bezier_4.amp[,2], 
                    bezier_5.amp[,2], 
                    bezier_6.amp[,2], 
                    bezier_7.amp[,2], 
                    bezier_8.amp[,2], 
                    bezier_9.amp[,2], 
                    bezier_10.amp[,2], 
                    bezier_11.amp[,2], 
                    bezier_12.amp[,2])

colnames(amp.x) <- c("2",
                     "3",
                     "4",
                     "5",
                     "6",
                     "7",
                     "8",
                     "9",
                     "10",
                     "11",
                     "12")

colnames(amp.y) <- c("y2",
                     "y3",
                     "y4",
                     "y5",
                     "y6",
                     "y7",
                     "y8",
                     "y9",
                     "y10",
                     "y11",
                     "y12")

amp.long.x <- melt(amp.x)
amp.long.y <- melt(amp.y)

amp.long <- data.frame(amp.long.x, 
                       amp.long.y)

colnames(amp.long) <- c("xfactor", 
                        "x", 
                        "yfactor", 
                        "y")

amp.long.ref <- as.data.frame(bezier_1.amp)

colnames(amp.long.ref) <- c("x", 
                            "y")



ggplot(data = amp.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  geom_line(data = amp.long.ref, 
            aes(x = x, 
                y = y), 
            size = 2) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Curve",guide = guide_legend(reverse = TRUE)) +
  ggtitle("Amplitude of Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("amplitude_curves.jpg")

#plot(bezier_1.amp, type="o", ylim=c(0,6))
#lines(bezier_2.amp, type="o")
#lines(bezier_3.amp, type="o")
#lines(bezier_4.amp, type="o")
#lines(bezier_5.amp, type="o")
#lines(bezier_6.amp, type="o")
#lines(bezier_7.amp, type="o")
#lines(bezier_8.amp, type="o")
#lines(bezier_9.amp, type="o")
#lines(bezier_10.amp, type="o")
#lines(bezier_11.amp, type="o")
#lines(bezier_12.amp, type="o")

temp_plots <- list()

for (i in 1:length(res.amplitude)) {
  temp <- res.amplitude[[i]]
  if(length(temp) == 0){
    temp_plots[[i]] <- NULL
  } else {
    plot(1:11, temp)
    temp_plots[[i]] <- recordPlot()
  }
}


# order dependence ----

pval1.ord <- matrix(c(0,0, 1,2, 2,0, 3,-2, 4,0), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

pval2.ord <- matrix(c(0,0, 1,2, 2,0, 3,-2, 4,0), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

bezier_1.ord <- pointsOnBezier(pval1.ord, 
                               n=200, 
                               deg=2)

bezier_2.ord <- pointsOnBezier(pval2.ord, 
                               n=200, 
                               deg=2)

bezier_2.ord.v <- sample(bezier_2.ord$points[,2])

bezier_2.ord$points[,2] <- bezier_2.ord.v

bz_x.ord <- list(bezier_1.ord$points)

bz_y.ord <- list(bezier_2.ord$points)

tslistx.ord <- lapply(bz_x.ord, 
                      function(i) as.vector(i[, 2]))

tslisty.ord <- lapply(bz_y.ord, 
                      function(i) as.vector(i[, 2]))

res.order <- dist.results(tslistx.ord,
                          tslisty.ord)

order.x <- data.frame(bezier_1.ord$points[,1],
                      bezier_2.ord$points[,1])

colnames(order.x) <- c("reference",
                       "randomized")

order.y <- data.frame(bezier_1.ord$points[,2],
                      bezier_2.ord$points[,2])

colnames(order.y) <- c("y1",
                       "y2")

order.long.x <- melt(order.x) 

order.long.y <- melt(order.y)

order.long <- data.frame(order.long.x,
                         order.long.y)

colnames(order.long) <- c("xfactor",
                          "x",
                          "yfactor",
                          "y")

ggplot(data=order.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Points") +
  ggtitle("Order Dependence") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("order_curves.jpg")

#plot(bezier_1.ord$points, type="o", ylim=c(-2,2))
#lines(bezier_2.ord$points, type="o")


# temporal scale sensitivity ----

pval1.tscale <- matrix(c(0,0, 1,0, 2,0, 3,1, 4,0, 5,0, 6,0), 
                       nrow=7, 
                       ncol=2, 
                       byrow=TRUE)

pval2.tscale <- matrix(c(0,0, 1,0, 2.2,0, 3,1, 3.8,0, 5,0, 6,0), 
                       nrow=7, 
                       ncol=2, 
                       byrow=TRUE)

pval3.tscale <- matrix(c(0,0, 1,0, 2.4,0, 3,1, 3.6,0, 5,0, 6,0), 
                       nrow=7, 
                       ncol=2, 
                       byrow=TRUE)

pval4.tscale <- matrix(c(0,0, 1,0, 2.6,0, 3,1, 3.4,0, 5,0, 6,0), 
                       nrow=7, 
                       ncol=2, 
                       byrow=TRUE)

pval5.tscale <- matrix(c(0,0, 1,0, 2.8,0, 3,1, 3.2,0, 5,0, 6,0), 
                       nrow=7, 
                       ncol=2, 
                       byrow=TRUE)

bezier_1.tscale <- pointsOnBezier(pval1.tscale, 
                                  n=300, 
                                  deg=2)

bezier_2.tscale <- pointsOnBezier(pval2.tscale, 
                                  n=300, 
                                  deg=2)

bezier_3.tscale <- pointsOnBezier(pval3.tscale, 
                                  n=300, 
                                  deg=2)

bezier_4.tscale <- pointsOnBezier(pval4.tscale, 
                                  n=300, 
                                  deg=2)

bezier_5.tscale <- pointsOnBezier(pval5.tscale, 
                                  n=300, 
                                  deg=2)

bz_x.tscale <- list(bezier_2.tscale$points,
                    bezier_3.tscale$points,
                    bezier_4.tscale$points,
                    bezier_5.tscale$points)

bz_y.tscale <- list(bezier_1.tscale$points,
                    bezier_1.tscale$points,
                    bezier_1.tscale$points,
                    bezier_1.tscale$points)

tslistx.tscale <- lapply(bz_x.tscale, 
                         function(i) as.vector(i[, 2]))

tslisty.tscale <- lapply(bz_y.tscale, 
                         function(i) as.vector(i[, 2]))

res.tscale <- dist.results(tslistx.tscale,
                           tslisty.tscale)

tscale.x <- data.frame(bezier_2.tscale$points[,1], 
                    bezier_3.tscale$points[,1], 
                    bezier_4.tscale$points[,1], 
                    bezier_5.tscale$points[,1])

tscale.y <- data.frame(bezier_2.tscale$points[,2],
                    bezier_3.tscale$points[,2], 
                    bezier_4.tscale$points[,2], 
                    bezier_5.tscale$points[,2])

colnames(tscale.x) <- c("2",
                     "3",
                     "4",
                     "5")

colnames(tscale.y) <- c("y2",
                     "y3",
                     "y4",
                     "y5")

tscale.long.x <- melt(tscale.x)
tscale.long.y <- melt(tscale.y)

tscale.long <- data.frame(tscale.long.x, 
                       tscale.long.y)

colnames(tscale.long) <- c("xfactor", 
                        "x", 
                        "yfactor", 
                        "y")

tscale.long.ref <- as.data.frame(bezier_1.tscale$points)

colnames(tscale.long.ref) <- c("x", 
                            "y")

ggplot(data = tscale.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  geom_line(data = tscale.long.ref, 
            aes(x = x, 
                y = y), 
            size = 2) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Deviation") +
  ggtitle("Temporal Scale Sensitivity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("tscale_curves.jpg")

plot(bezier_1.tscale$points, type="o", ylim=c(-2,2), col="black")
lines(bezier_2.tscale$points, type="o", col="red")
lines(bezier_3.tscale$points, type="o", col="blue")
#lines(bezier_4.tscale$points, type="o", col="green")
#lines(bezier_5.tscale$points, type="o", col="orange")
#lines(bezier_6.tscale$points, type="o", col="purple")
#lines(bezier_7.tscale$points, type="o", col="yellow")
#lines(bezier_8.tscale$points, type="o", col="cyan")

temp_plots.tscale <- list()

for (i in 1:length(res.tscale)) {
  temp <- res.tscale[[i]]
  if(length(temp) == 0){
    temp_plots.tscale[[i]] <- NULL
  } else if(is.nan(temp)) {
    temp_plots.tscale[[i]] <- NULL
  } else {
    plot(1:4, temp)
    temp_plots.tscale[[i]] <- recordPlot()
  }
}

# symmetry ----

pval1.sym <- matrix(c(0,0, 1,1, 2,0, 3,-1, 4,0), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

pval2.sym <- matrix(c(0,0, 0.2,3, 2,0.5, 3.5,2, 4,1), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

bezier_1.sym <- pointsOnBezier(pval1.sym, 
                               n=200, 
                               deg=2)

bezier_2.sym <- pointsOnBezier(pval2.sym, 
                               n=200, 
                               deg=2)

bz_x.sym <- list(bezier_2.sym$points, 
                 bezier_1.sym$points)

bz_y.sym <- list(bezier_1.sym$points, 
                 bezier_2.sym$points)

tslistx.sym <- lapply(bz_x.sym, 
                      function(i) as.vector(i[, 2]))

tslisty.sym <- lapply(bz_y.sym, 
                      function(i) as.vector(i[, 2]))

res.symmetry <- dist.results(tslistx.sym,
                             tslisty.sym)

sym.x <- data.frame(bezier_1.sym$points[,1],
                    bezier_2.sym$points[,1])

colnames(sym.x) <- c("A",
                     "B")

sym.y <- data.frame(bezier_1.sym$points[,2],
                    bezier_2.sym$points[,2])

colnames(sym.y) <- c("y1",
                     "y2")

sym.long.x <- melt(sym.x) 

sym.long.y <- melt(sym.y)

sym.long <- data.frame(sym.long.x,
                       sym.long.y)

colnames(sym.long) <- c("xfactor",
                        "x",
                        "yfactor",
                        "y")

ggplot(data=sym.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Curves") +
  ggtitle("Symmetry") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("symmetry_curves.jpg")

#plot(bezier_1.sym$points, 
#     type="o", 
#     ylim=c(-2,2))

#lines(bezier_2.sym$points, 
#      type="o")


# parallelism ----

#pval1.par <- matrix(c(0,0, 1,2, 2,0), 
#                    nrow=3, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval2.par <- matrix(c(0,0, 1,3, 2,0), 
#                    nrow=3, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval3.par <- matrix(c(0,0, 1,2, 2,1), 
#                    nrow=3, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval4.par <- matrix(c(0,1, 1,2, 2,0), 
#                    nrow=3, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval5.par <- matrix(c(0,0, 2,2, 2,0), 
#                    nrow=3, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval6.par <- matrix(c(0,0, 0,2, 2,0), 
#                    nrow=3, 
#                    ncol=2, 
#                   byrow=TRUE)

#pval7.par <- matrix(c(0,0, 2,3, 2,0), 
#                    nrow=3, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval8.par <- matrix(c(0,0, 0,3, 2,0), 
#                    nrow=3, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval9.par <- matrix(c(0,0, 1,-2, 2,0), 
#                    nrow=3, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval10.par <- matrix(c(0,1, 1,3, 2,1), 
#                     nrow=3, 
#                     ncol=2, 
#                     byrow=TRUE)

#pval11.par <- matrix(c(0,0, 1,2, 2,0), 
#                     nrow=3, 
#                     ncol=2, 
#                     byrow=TRUE)

#pval12.par <- matrix(c(0,2, 1,6, 2,2), 
#                     nrow=3, 
#                     ncol=2, 
#                     byrow=TRUE)

#pval13.par <- matrix(c(0,-1, 1,-3, 2,-1), 
#                     nrow=3, 
#                     ncol=2, 
#                     byrow=TRUE)

#pval14.par <- matrix(c(0,1, 1,-1, 2,1), 
#                     nrow=3, 
#                     ncol=2, 
#                     byrow=TRUE)

#pval15.par <- matrix(c(0,-2, 1,-6, 2,-2), 
#                     nrow=3, 
#                     ncol=2, 
#                     byrow=TRUE)

#bezier_1.par <- pointsOnBezier(pval1.par, 
#                               n=100, 
#                               deg=2)

#bezier_2.par <- pointsOnBezier(pval2.par, 
#                               n=100, 
#                               deg=2)

#bezier_3.par <- pointsOnBezier(pval3.par, 
#                               n=100, 
#                               deg=2)

#bezier_4.par <- pointsOnBezier(pval4.par, 
#                               n=100, 
#                               deg=2)

#bezier_5.par <- pointsOnBezier(pval5.par, 
#                               n=100, 
#                               deg=2)

#bezier_6.par <- pointsOnBezier(pval6.par, 
#                               n=100, 
#                               deg=2)

#bezier_7.par <- pointsOnBezier(pval7.par, 
#                               n=100, 
#                               deg=2)

#bezier_8.par <- pointsOnBezier(pval8.par, 
#                               n=100, 
#                               deg=2)

#bezier_9.par <- pointsOnBezier(pval9.par, 
#                               n=100, 
#                               deg=2)

#bezier_10.par <- pointsOnBezier(pval10.par, 
#                                n=100, 
#                                deg=2)

#bezier_11.par <- pointsOnBezier(pval11.par, 
#                                n=100, 
#                                deg=2)

#bezier_12.par <- pointsOnBezier(pval12.par, 
#                                n=100, 
#                                deg=2)

#bezier_13.par <- pointsOnBezier(pval13.par, 
#                                n=100, 
#                                deg=2)

#bezier_14.par <- pointsOnBezier(pval14.par, 
#                                n=100, 
#                                deg=2)

#bezier_15.par <- pointsOnBezier(pval15.par, 
#                                n=100, 
#                                deg=2)

#bz_x.par <- list(bezier_2.par$points, 
#                 bezier_3.par$points, 
#                 bezier_4.par$points, 
#                 bezier_5.par$points, 
#                 bezier_6.par$points, 
#                 bezier_7.par$points, 
#                 bezier_8.par$points, 
#                 bezier_9.par$points, 
#                 bezier_10.par$points, 
#                 bezier_11.par$points, 
#                 bezier_12.par$points, 
#                 bezier_13.par$points, 
#                 bezier_14.par$points, 
#                 bezier_15.par$points)

#bz_y.par <- list(bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_10.par$points, 
#                 bezier_1.par$points, 
#                 bezier_1.par$points, 
#                 bezier_10.par$points)

#tslistx.par <- lapply(bz_x.par, 
#                      function(i) as.vector(i[, 2]))

#tslisty.par <- lapply(bz_y.par, 
#                      function(i) as.vector(i[, 2]))

#res.parallel <- dist.results(tslistx.par,
#                             tslisty.par)

#par.x <- data.frame(bezier_2.par$points[,1], 
#                    bezier_3.par$points[,1], 
#                    bezier_4.par$points[,1], 
#                    bezier_5.par$points[,1], 
#                    bezier_6.par$points[,1], 
#                    bezier_7.par$points[,1], 
#                    bezier_8.par$points[,1], 
#                    bezier_9.par$points[,1], 
#                    bezier_10.par$points[,1], 
#                    bezier_11.par$points[,1], 
#                    bezier_12.par$points[,1],
#                    bezier_13.par$points[,1],
#                    bezier_14.par$points[,1],
#                    bezier_15.par$points[,1])

#par.y <- data.frame(bezier_2.par$points[,2],
#                    bezier_3.par$points[,2], 
#                    bezier_4.par$points[,2], 
#                    bezier_5.par$points[,2], 
#                    bezier_6.par$points[,2], 
#                    bezier_7.par$points[,2], 
#                    bezier_8.par$points[,2], 
#                    bezier_9.par$points[,2], 
#                    bezier_10.par$points[,2], 
#                    bezier_11.par$points[,2], 
#                    bezier_12.par$points[,2],
#                    bezier_13.par$points[,2],
#                    bezier_14.par$points[,2],
#                    bezier_15.par$points[,2])

#colnames(par.x) <- c("2",
#                     "3",
#                     "4",
#                     "5",
#                     "6",
#                     "7",
#                     "8",
#                     "9",
#                     "10",
#                     "11",
#                     "12",
#                     "13",
#                     "14",
#                     "15")

#colnames(par.y) <- c("y2",
#                     "y3",
#                     "y4",
#                     "y5",
#                     "y6",
#                     "y7",
#                     "y8",
#                     "y9",
#                     "y10",
#                     "y11",
#                     "y12",
#                     "y13",
#                     "y14",
#                     "y15")

#par.long.x <- melt(par.x)
#par.long.y <- melt(par.y)

#par.long <- data.frame(par.long.x, 
#                       par.long.y)

#colnames(par.long) <- c("xfactor", 
#                        "x", 
#                        "yfactor", 
#                        "y")

#par.long.ref <- as.data.frame(bezier_1.par$points)

#colnames(par.long.ref) <- c("x", 
#                            "y")



#ggplot(data = par.long) +
#  geom_point(aes(x = x, 
#                 y = y, 
#                 color = factor(xfactor))) +
#  geom_line(data = par.long.ref, 
#            aes(x = x, 
#                y = y), 
#            size = 2) +
#  theme(legend.title = element_text(color="blue")) +
#  scale_color_discrete(name = "Curve",guide = guide_legend(reverse = TRUE)) +
#  ggtitle("Parallelism") +
#  theme_minimal() +
#  theme(plot.title = element_text(hjust = 0.5))

#ggsave("parallelism_curves.jpg")

#plot(bezier_1.par$points, type="o", ylim=c(-5,5), col="black")
#lines(bezier_2.par$points, type="o", col="red")
#lines(bezier_3.par$points, type="o", col="blue")
#lines(bezier_4.par$points, type="o", col="green")
#lines(bezier_5.par$points, type="o", col="yellow")
#lines(bezier_6.par$points, type="o", col="purple")
#lines(bezier_7.par$points, type="o", col="orange")
#lines(bezier_8.par$points, type="o", col="cyan")
#lines(bezier_9.par$points, type="o", col="violet")
#lines(bezier_10.par$points, type="o", col="brown")
#lines(bezier_11.par$points, type="o", col="dark green")
#lines(bezier_12.par$points, type="o", col="light blue")
#lines(bezier_13.par$points, type="o", col="light green")
#lines(bezier_14.par$points, type="o", col="gold")
#lines(bezier_15.par$points, type="o", col="dark grey")

#uniqueness ----

pval1.unq <- matrix(c(0,0, 1,2, 2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval2.unq <- matrix(c(0,0, 1,2, 2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

bezier_1.unq <- pointsOnBezier(pval1.unq, 
                               n=100, 
                               deg=2)

bezier_2.unq <- pointsOnBezier(pval2.unq, 
                               n=100, 
                               deg=2)


bz_x.unq <- list(bezier_1.unq$points)

bz_y.unq <- list(bezier_2.unq$points)

tslistx.unq <- lapply(bz_x.unq, 
                      function(i) as.vector(i[, 2]))

tslisty.unq <- lapply(bz_y.unq, 
                      function(i) as.vector(i[, 2]))

res.unique <- dist.results(tslistx.unq,
                           tslisty.unq)

unq.x <- data.frame(bezier_1.unq$points[,1],
                    bezier_2.unq$points[,1])

colnames(unq.x) <- c("A",
                     "B")

unq.y <- data.frame(bezier_1.unq$points[,2],
                    bezier_2.unq$points[,2])

colnames(unq.y) <- c("y1",
                     "y2")

unq.long.x <- melt(unq.x) 

unq.long.y <- melt(unq.y)

unq.long <- data.frame(unq.long.x,
                       unq.long.y)

colnames(unq.long) <- c("xfactor",
                        "x",
                        "yfactor",
                        "y")

ggplot(data=unq.long) +
  geom_point(aes(x = x, 
                 y = y)) +
  ggtitle("Uniqueness") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("uniqueness_curve.jpg")


# direction of deviation ----

#tval.dir <- seq(0,2, 
#                length=200)

pval1.dir <- matrix(c(0,0, 1,1, 2,0, 3,-1, 4,0), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

pval2.dir <- matrix(c(0,0, 1,-1, 2,0, 3,1, 4,0), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

pval3.dir <- matrix(c(0,0, 1,3, 2,0, 3,-3, 4,0), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

pval4.dir <- matrix(c(0,0, 1,2, 2,0, 3,-2, 4,0), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

pval5.dir <- matrix(c(0,0, 1,-2, 2,0, 3,2, 4,0), 
                    nrow=5, 
                    ncol=2, 
                    byrow=TRUE)

#pval6.dir <- matrix(c(0,0, 1,2, 2.5,0, 3,-2, 4,0), 
#                    nrow=5, 
#                    ncol=2, 
#                    byrow=TRUE)

#pval7.dir <- matrix(c(0,0, 1,-2, 2.5,0, 3,2, 4,0), 
#                    nrow=5, 
#                    ncol=2, 
#                    byrow=TRUE)

bezier_1.dir <- pointsOnBezier(pval1.dir, 
                               n=200, 
                               deg=2)

bezier_2.dir <- pointsOnBezier(pval2.dir, 
                               n=200, 
                               deg=2)

bezier_3.dir <- pointsOnBezier(pval3.dir, 
                               n=200, 
                               deg=2)

bezier_4.dir <- pointsOnBezier(pval4.dir, 
                               n=200, 
                               deg=2)

bezier_5.dir <- pointsOnBezier(pval5.dir, 
                               n=200, 
                               deg=2)

#bezier_6.dir <- pointsOnBezier(pval6.dir, 
#                               n=200, 
#                               deg=2)

#bezier_7.dir <- pointsOnBezier(pval7.dir, 
#                               n=200, 
#                               deg=2)

bz_x.dir <- list(bezier_2.dir$points, 
                 bezier_3.dir$points, 
                 bezier_4.dir$points, 
                 bezier_5.dir$points)

bz_y.dir <- list(bezier_1.dir$points, 
                 bezier_1.dir$points, 
                 bezier_1.dir$points, 
                 bezier_1.dir$points)

tslistx.dir <- lapply(bz_x.dir, 
                      function(i) as.vector(i[, 2]))

tslisty.dir <- lapply(bz_y.dir, 
                      function(i) as.vector(i[, 2]))

res.direction <- dist.results(tslistx.dir,
                              tslisty.dir)

dir.x <- data.frame(bezier_2.dir$points[,1], 
                    bezier_3.dir$points[,1], 
                    bezier_4.dir$points[,1], 
                    bezier_5.dir$points[,1])

dir.y <- data.frame(bezier_2.dir$points[,2],
                    bezier_3.dir$points[,2], 
                    bezier_4.dir$points[,2], 
                    bezier_5.dir$points[,2])

colnames(dir.x) <- c("1",
                     "2",
                     "3",
                     "4")

colnames(dir.y) <- c("y1",
                     "y2",
                     "y3",
                     "y4")

dir.long.x <- melt(dir.x)
dir.long.y <- melt(dir.y)

dir.long <- data.frame(dir.long.x, 
                       dir.long.y)

colnames(dir.long) <- c("xfactor", 
                        "x", 
                        "yfactor", 
                        "y")

dir.long.ref <- as.data.frame(bezier_1.dir$points)

colnames(dir.long.ref) <- c("x", 
                            "y")

ggplot(data = dir.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  geom_line(data = dir.long.ref, 
            aes(x = x, 
                y = y), 
            size = 2) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Deviation") +
  ggtitle("Direction of Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("direction_curves.jpg")

#plot(bezier_1.dir$points, type="o", ylim=c(-2,2))
#lines(bezier_2.dir$points, type="o")
#lines(bezier_3.dir$points, type="o")
#lines(bezier_4.dir$points, type="o")
#lines(bezier_5.dir$points, type="o")
#lines(bezier_6.dir$points, type="o")
#lines(bezier_7.dir$points, type="o")

#offset sensitivity ----

tval.off <- seq(0,1, 
                length=100)

pval1.off <- matrix(c(0,0, 1,1, 2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval2.off <- matrix(c(0,1, 1,2, 2,1), 
                    nrow=3,
                    ncol=2, 
                    byrow=TRUE)

pval3.off <- matrix(c(0,2, 1,3, 2,2), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval4.off <- matrix(c(0,3, 1,4, 2,3), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval5.off <- matrix(c(0,4, 1,5, 2,4), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval6.off <- matrix(c(0,5, 1,6, 2,5), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval7.off <- matrix(c(0,6, 1,7, 2,6), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval8.off <- matrix(c(0,7, 1,8, 2,7), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval9.off <- matrix(c(0,8, 1,9, 2,8), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval10.off <- matrix(c(0,9, 1,10, 2,9), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

pval11.off <- matrix(c(0,10, 1,11, 2,10), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

pval12.off <- matrix(c(0,11, 1,12, 2,11), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

bezier_1.off <- bezier(tval.off, 
                       pval1.off, 
                       deg=2)

bezier_2.off <- bezier(tval.off, 
                       pval2.off, 
                       deg=2)

bezier_3.off <- bezier(tval.off, 
                       pval3.off, 
                       deg=2)

bezier_4.off <- bezier(tval.off, 
                       pval4.off, 
                       deg=2)

bezier_5.off <- bezier(tval.off, 
                       pval5.off, 
                       deg=2)

bezier_6.off <- bezier(tval.off, 
                       pval6.off, 
                       deg=2)

bezier_7.off <- bezier(tval.off, 
                       pval7.off, 
                       deg=2)

bezier_8.off <- bezier(tval.off, 
                       pval8.off, 
                       deg=2)

bezier_9.off <- bezier(tval.off, 
                       pval9.off, 
                       deg=2)

bezier_10.off <- bezier(tval.off, 
                        pval10.off, 
                        deg=2)

bezier_11.off <- bezier(tval.off, 
                        pval11.off, 
                        deg=2)

bezier_12.off <- bezier(tval.off, 
                        pval12.off, 
                        deg=2)

bz_x.off <- list(bezier_2.off, 
                 bezier_3.off, 
                 bezier_4.off, 
                 bezier_5.off, 
                 bezier_6.off, 
                 bezier_7.off, 
                 bezier_8.off, 
                 bezier_9.off, 
                 bezier_10.off, 
                 bezier_11.off, 
                 bezier_12.off)

bz_y.off <- list(bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off, 
                 bezier_1.off)

tslistx.off <- lapply(bz_x.off, 
                      function(i) as.vector(i[, 2]))

tslisty.off <- lapply(bz_y.off, 
                      function(i) as.vector(i[, 2]))

res.offset <- dist.results(tslistx.off,
                           tslisty.off)

off.x <- data.frame(bezier_2.off[,1], 
                    bezier_3.off[,1], 
                    bezier_4.off[,1], 
                    bezier_5.off[,1], 
                    bezier_6.off[,1], 
                    bezier_7.off[,1], 
                    bezier_8.off[,1], 
                    bezier_9.off[,1], 
                    bezier_10.off[,1], 
                    bezier_11.off[,1], 
                    bezier_12.off[,1])

off.y <- data.frame(bezier_2.off[,2],
                    bezier_3.off[,2], 
                    bezier_4.off[,2], 
                    bezier_5.off[,2], 
                    bezier_6.off[,2], 
                    bezier_7.off[,2], 
                    bezier_8.off[,2], 
                    bezier_9.off[,2], 
                    bezier_10.off[,2], 
                    bezier_11.off[,2], 
                    bezier_12.off[,2])

colnames(off.x) <- c("2",
                     "3",
                     "4",
                     "5",
                     "6",
                     "7",
                     "8",
                     "9",
                     "10",
                     "11",
                     "12")

colnames(off.y) <- c("y2",
                     "y3",
                     "y4",
                     "y5",
                     "y6",
                     "y7",
                     "y8",
                     "y9",
                     "y10",
                     "y11",
                     "y12")

off.long.x <- melt(off.x)
off.long.y <- melt(off.y)

off.long <- data.frame(off.long.x, 
                       off.long.y)

colnames(off.long) <- c("xfactor", 
                        "x", 
                        "yfactor", 
                        "y")

off.long.ref <- as.data.frame(bezier_1.off)

colnames(off.long.ref) <- c("x", 
                            "y")

ggplot(data = off.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  geom_line(data = off.long.ref, 
            aes(x = x, 
                y = y), 
            size = 2) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Curve",guide = guide_legend(reverse = TRUE)) +
  ggtitle("Offset Sensitivity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("offset_curves.jpg")


#plot(bezier_1.off, type="o", ylim=c(-1,12))
#lines(bezier_2.off, type="o")
#lines(bezier_3.off, type="o")
#lines(bezier_4.off, type="o")
#lines(bezier_5.off, type="o")
#lines(bezier_6.off, type="o")
#lines(bezier_7.off, type="o")
#lines(bezier_8.off, type="o")
#lines(bezier_9.off, type="o")
#lines(bezier_10.off, type="o")
#lines(bezier_11.off, type="o")
#lines(bezier_12.off, type="o")

temp_plots.off <- list()

for (i in 1:length(res.offset)) {
  temp <- res.offset[[i]]
  if(length(temp) == 0){
    temp_plots.off[[i]] <- NULL
  } else {
    plot(1:11, temp)
    temp_plots.off[[i]] <- recordPlot()
  }
}

# rotational deviation ----

tval.rot <- seq(0,1, 
                length=100)

pval1.rot <- matrix(c(0,0,1,1.1,2,1), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval2.rot <- matrix(c(0,0,1,2.2,2,2), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval3.rot <- matrix(c(0,0,1,3.3,2,3), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval4.rot <- matrix(c(0,0,1,14.4,2,4), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval5.rot <- matrix(c(0,0,1,5.5,2,5), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval6.rot <- matrix(c(0,0,1,6.6,2,6), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

bezier_1.rot <- bezier(tval.rot, 
                       pval1.rot)

bezier_2.rot <- bezier(tval.rot, 
                       pval2.rot)

bezier_3.rot <- bezier(tval.rot, 
                       pval3.rot)

bezier_4.rot <- bezier(tval.rot, 
                       pval4.rot)

bezier_5.rot <- bezier(tval.rot, 
                       pval5.rot)

bezier_6.rot <- bezier(tval.rot, 
                       pval6.rot)


bz_x.rot <- list(bezier_2.rot, 
                 bezier_3.rot, 
                 bezier_4.rot, 
                 bezier_5.rot, 
                 bezier_6.rot)

bz_y.rot <- list(bezier_1.rot, 
                 bezier_1.rot, 
                 bezier_1.rot, 
                 bezier_1.rot, 
                 bezier_1.rot)

tslistx.rot <- lapply(bz_x.rot, 
                      function(i) as.vector(i[, 2]))

tslisty.rot <- lapply(bz_y.rot, 
                      function(i) as.vector(i[, 2]))

res.rotation <- dist.results(tslistx.rot,
                             tslisty.rot)

rot.x <- data.frame(bezier_2.rot[,1], 
                    bezier_3.rot[,1], 
                    bezier_4.rot[,1], 
                    bezier_5.rot[,1], 
                    bezier_6.rot[,1])

rot.y <- data.frame(bezier_2.rot[,2],
                    bezier_3.rot[,2], 
                    bezier_4.rot[,2], 
                    bezier_5.rot[,2], 
                    bezier_6.rot[,2])

colnames(rot.x) <- c("2",
                     "3",
                     "4",
                     "5",
                     "6")

colnames(rot.y) <- c("y2",
                     "y3",
                     "y4",
                     "y5",
                     "y6")

rot.long.x <- melt(rot.x)
rot.long.y <- melt(rot.y)

rot.long <- data.frame(rot.long.x, 
                       rot.long.y)

colnames(rot.long) <- c("xfactor", 
                        "x", 
                        "yfactor", 
                        "y")

rot.long.ref <- as.data.frame(bezier_1.rot)

colnames(rot.long.ref) <- c("x", 
                            "y")



ggplot(data = rot.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  geom_line(data = rot.long.ref, 
            aes(x = x, 
                y = y), 
            size = 2) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Curve",guide = guide_legend(reverse = TRUE)) +
  ggtitle("Rotational Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("rotation_curves.jpg")

temp_plots <- list()

for (i in 1:length(res.rotation)) {
  temp <- res.rotation[[i]]
  if(length(temp) == 0){
    temp_plots[[i]] <- NULL
  } else {
    plot(1:11, temp)
    temp_plots[[i]] <- recordPlot()
  }
}

# uniform time scaling sensitivity ----

pval1.utinv <- matrix(c(0,0, 1,0, 2,1, 3,0, 4,0, 5,0, 6,1, 7,0, 8,0), 
                       nrow=9, 
                       ncol=2, 
                       byrow=TRUE)

pval2.utinv <- matrix(c(0,0, 1.1,0, 2.2,1, 3.3,0, 4.4,0, 5.5,0, 6.6,1, 7.7,0, 8.8,0), 
                       nrow=9, 
                       ncol=2, 
                       byrow=TRUE)

pval3.utinv <- matrix(c(0,0, 1.2,0, 2.4,1, 3.6,0, 4.8,0, 6,0, 7.2,1, 8.4,0, 9.6,0), 
                       nrow=9, 
                       ncol=2, 
                       byrow=TRUE)

pval4.utinv <- matrix(c(0,0, 1.3,0, 2.6,1, 3.9,0, 5.2,0, 6.5,0, 7.8,1, 9.1,0, 10.4,0), 
                       nrow=9, 
                       ncol=2, 
                       byrow=TRUE)

pval5.utinv <- matrix(c(0,0, 1.4,0, 2.8,1, 4.2,0, 5.6,0, 7,0, 8.4,1, 9.8,0, 11.2,0), 
                       nrow=9, 
                       ncol=2, 
                       byrow=TRUE)

bezier_1.utinv <- pointsOnBezier(pval1.utinv, 
                                  n=300, 
                                  deg=2)

bezier_2.utinv <- pointsOnBezier(pval2.utinv, 
                                  n=300, 
                                  deg=2)

bezier_3.utinv <- pointsOnBezier(pval3.utinv, 
                                  n=300, 
                                  deg=2)

bezier_4.utinv <- pointsOnBezier(pval4.utinv, 
                                  n=300, 
                                  deg=2)

bezier_5.utinv <- pointsOnBezier(pval5.utinv, 
                                  n=300, 
                                  deg=2)

bz_x.utinv <- list(bezier_2.utinv$points,
                    bezier_3.utinv$points,
                    bezier_4.utinv$points,
                    bezier_5.utinv$points)

bz_y.utinv <- list(bezier_1.utinv$points,
                    bezier_1.utinv$points,
                    bezier_1.utinv$points,
                    bezier_1.utinv$points)

tslistx.utinv <- lapply(bz_x.utinv, 
                         function(i) as.vector(i[, 2]))

tslisty.utinv <- lapply(bz_y.utinv, 
                         function(i) as.vector(i[, 2]))

res.utinv <- dist.results(tslistx.utinv,
                           tslisty.utinv)

utinv.x <- data.frame(bezier_2.utinv$points[,1], 
                       bezier_3.utinv$points[,1], 
                       bezier_4.utinv$points[,1], 
                       bezier_5.utinv$points[,1])

utinv.y <- data.frame(bezier_2.utinv$points[,2],
                       bezier_3.utinv$points[,2], 
                       bezier_4.utinv$points[,2], 
                       bezier_5.utinv$points[,2])

colnames(utinv.x) <- c("2",
                        "3",
                        "4",
                        "5")

colnames(utinv.y) <- c("y2",
                        "y3",
                        "y4",
                        "y5")

utinv.long.x <- melt(utinv.x)
utinv.long.y <- melt(utinv.y)

utinv.long <- data.frame(utinv.long.x, 
                          utinv.long.y)

colnames(utinv.long) <- c("xfactor", 
                           "x", 
                           "yfactor", 
                           "y")

utinv.long.ref <- as.data.frame(bezier_1.utinv$points)

colnames(utinv.long.ref) <- c("x", 
                               "y")

ggplot(data = utinv.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  geom_line(data = utinv.long.ref, 
            aes(x = x, 
                y = y), 
            size = 2) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Deviation") +
  ggtitle("Uniform Time Scaling Sensitivity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("utinv_curves.jpg")

plot(bezier_1.utinv$points, type="o", ylim=c(-2,2), col="black")
lines(bezier_2.utinv$points, type="o", col="red")
lines(bezier_3.utinv$points, type="o", col="blue")
#lines(bezier_4.utinv$points, type="o", col="green")
#lines(bezier_5.utinv$points, type="o", col="orange")
#lines(bezier_6.utinv$points, type="o", col="purple")
#lines(bezier_7.utinv$points, type="o", col="yellow")
#lines(bezier_8.utinv$points, type="o", col="cyan")

temp_plots.utinv <- list()

for (i in 1:length(res.utinv)) {
  temp <- res.utinv[[i]]
  if(length(temp) == 0){
    temp_plots.utinv[[i]] <- NULL
  } else if(is.nan(temp)) {
    temp_plots.utinv[[i]] <- NULL
  } else {
    plot(1:4, temp)
    temp_plots.utinv[[i]] <- recordPlot()
  }
}

#scale sensitivity ----

tval.scl <- seq(0,1, 
                length=100)

pval1.scl <- matrix(c(0,0, 1,1, 2,0), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval2.scl <- matrix(c(0,1, 1,2, 2,1), 
                    nrow=3,
                    ncol=2, 
                    byrow=TRUE)

pval3.scl <- matrix(c(0,2, 1,4, 2,2), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval4.scl <- matrix(c(0,3, 1,6, 2,3), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval5.scl <- matrix(c(0,4, 1,8, 2,4), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval6.scl <- matrix(c(0,5, 1,10, 2,5), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval7.scl <- matrix(c(0,6, 1,12, 2,6), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval8.scl <- matrix(c(0,7, 1,14, 2,7), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval9.scl <- matrix(c(0,8, 1,16, 2,8), 
                    nrow=3, 
                    ncol=2, 
                    byrow=TRUE)

pval10.scl <- matrix(c(0,9, 1,18, 2,9), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

pval11.scl <- matrix(c(0,10, 1,20, 2,10), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

pval12.scl <- matrix(c(0,11, 1,22, 2,11), 
                     nrow=3, 
                     ncol=2, 
                     byrow=TRUE)

bezier_1.scl <- bezier(tval.scl, 
                       pval1.scl, 
                       deg=2)

bezier_2.scl <- bezier(tval.scl, 
                       pval2.scl, 
                       deg=2)

bezier_3.scl <- bezier(tval.scl, 
                       pval3.scl, 
                       deg=2)

bezier_4.scl <- bezier(tval.scl, 
                       pval4.scl, 
                       deg=2)

bezier_5.scl <- bezier(tval.scl, 
                       pval5.scl, 
                       deg=2)

bezier_6.scl <- bezier(tval.scl, 
                       pval6.scl, 
                       deg=2)

bezier_7.scl <- bezier(tval.scl, 
                       pval7.scl, 
                       deg=2)

bezier_8.scl <- bezier(tval.scl, 
                       pval8.scl, 
                       deg=2)

bezier_9.scl <- bezier(tval.scl, 
                       pval9.scl, 
                       deg=2)

bezier_10.scl <- bezier(tval.scl, 
                        pval10.scl, 
                        deg=2)

bezier_11.scl <- bezier(tval.scl, 
                        pval11.scl, 
                        deg=2)

bezier_12.scl <- bezier(tval.scl, 
                        pval12.scl, 
                        deg=2)

bz_x.scl <- list(bezier_2.scl, 
                 bezier_3.scl, 
                 bezier_4.scl, 
                 bezier_5.scl, 
                 bezier_6.scl, 
                 bezier_7.scl, 
                 bezier_8.scl, 
                 bezier_9.scl, 
                 bezier_10.scl, 
                 bezier_11.scl, 
                 bezier_12.scl)

bz_y.scl <- list(bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl, 
                 bezier_1.scl)

tslistx.scl <- lapply(bz_x.scl, 
                      function(i) as.vector(i[, 2]))

tslisty.scl <- lapply(bz_y.scl, 
                      function(i) as.vector(i[, 2]))

res.sclset <- dist.results(tslistx.scl,
                           tslisty.scl)

scl.x <- data.frame(bezier_2.scl[,1], 
                    bezier_3.scl[,1], 
                    bezier_4.scl[,1], 
                    bezier_5.scl[,1], 
                    bezier_6.scl[,1], 
                    bezier_7.scl[,1], 
                    bezier_8.scl[,1], 
                    bezier_9.scl[,1], 
                    bezier_10.scl[,1], 
                    bezier_11.scl[,1], 
                    bezier_12.scl[,1])

scl.y <- data.frame(bezier_2.scl[,2],
                    bezier_3.scl[,2], 
                    bezier_4.scl[,2], 
                    bezier_5.scl[,2], 
                    bezier_6.scl[,2], 
                    bezier_7.scl[,2], 
                    bezier_8.scl[,2], 
                    bezier_9.scl[,2], 
                    bezier_10.scl[,2], 
                    bezier_11.scl[,2], 
                    bezier_12.scl[,2])

colnames(scl.x) <- c("2",
                     "3",
                     "4",
                     "5",
                     "6",
                     "7",
                     "8",
                     "9",
                     "10",
                     "11",
                     "12")

colnames(scl.y) <- c("y2",
                     "y3",
                     "y4",
                     "y5",
                     "y6",
                     "y7",
                     "y8",
                     "y9",
                     "y10",
                     "y11",
                     "y12")

scl.long.x <- melt(scl.x)
scl.long.y <- melt(scl.y)

scl.long <- data.frame(scl.long.x, 
                       scl.long.y)

colnames(scl.long) <- c("xfactor", 
                        "x", 
                        "yfactor", 
                        "y")

scl.long.ref <- as.data.frame(bezier_1.scl)

colnames(scl.long.ref) <- c("x", 
                            "y")

ggplot(data = scl.long) +
  geom_point(aes(x = x, 
                 y = y, 
                 color = factor(xfactor))) +
  geom_line(data = scl.long.ref, 
            aes(x = x, 
                y = y), 
            size = 2) +
  theme(legend.title = element_text(color="blue")) +
  scale_color_discrete(name = "Curve",guide = guide_legend(reverse = TRUE)) +
  ggtitle("Scale Sensitivity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("scale_curves.jpg")


#plot(bezier_1.scl, type="o", ylim=c(-1,12))
#lines(bezier_2.scl, type="o")
#lines(bezier_3.scl, type="o")
#lines(bezier_4.scl, type="o")
#lines(bezier_5.scl, type="o")
#lines(bezier_6.scl, type="o")
#lines(bezier_7.scl, type="o")
#lines(bezier_8.scl, type="o")
#lines(bezier_9.scl, type="o")
#lines(bezier_10.scl, type="o")
#lines(bezier_11.scl, type="o")
#lines(bezier_12.scl, type="o")

temp_plots.scl <- list()

for (i in 1:length(res.sclset)) {
  temp <- res.sclset[[i]]
  if(length(temp) == 0){
    temp_plots.scl[[i]] <- NULL
  } else {
    plot(1:11, temp)
    temp_plots.scl[[i]] <- recordPlot()
  }
}
