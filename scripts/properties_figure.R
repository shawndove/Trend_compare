## create time series ----

# uniqueness
test.trans.a <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
test.trans.b <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)

# amplitude
test.amp.a <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
test.amp.b <- c(1, 1, 3, 3, 1, 1, 1, 1, 1, 1)

# white noise
test.wn.a <- c(1, 4, 3, 6, 5, 6, 3, 4, 1, 1)
test.wn.b <- c(1, 4.5, 2.5, 6.5, 4.5, 6.5, 2.5, 4.5, 1, 1)

# biased noise
test.bn.a <- c(1, 4, 3, 6, 5, 6, 3, 4, 1, 1)
test.bn.b <- c(1, 4, 2, 6, 4, 6, 2, 4, 1, 1)

# outlier
test.out.a <- c(1, 4, 3, 6, 5, 6, 3, 4, 1, 1)
test.out.b <- c(1, 4, 3, 6, 1, 6, 3, 4, 1, 1)

# phase shift
test.phase.a <- c(1, 1, 4, 2, 1, 3, 1, 1, 1, 1)
test.phase.b <- c(1, 1, 1, 4, 2, 1, 3, 1, 1, 1)

# time scaling
test.tsc.a <- c(1, 2, 1, 1, 3, 1)
test.tsc.b <- c(1, 1.5, 2, 1.5, 1, 1, 1, 2, 3, 2, 1)

# warping
test.warp.a <- c(1, 1, 2, 1, 1, 3, 1, 1)
test.warp.b <- c(1, 1, 2, 1, 1, 2, 3, 2, 1, 1, 1)

# duration
test.dur.a <- c(1, 1, 3, 3, 1, 1, 1, 1, 1, 1)
test.dur.b <- c(1, 1, 3, 3, 3, 1, 1, 1, 1, 1)

# frequency
test.freq.a <- c(1, 3, 1, 1, 1, 1, 1, 1, 1, 1)
test.freq.b <- c(1, 3, 1, 1, 3, 1, 1, 1, 1, 1)

## plot figure ----

# create directory to save figure if it doesn't exist
if(!dir.exists("figures/")) {dir.create("figures/")}

# create tiff file
tiff(file = "figures/properties.tiff", 
     compression = "lzw", 
     res = 1000,
     width = 7480, 
     height = 9055, 
     units = "px", 
     pointsize = 12, 
     bg = "white")

# set parameters
par(mfrow = c(5, 2), # row and column structure
    mar = c(1, 0.5 , 1, 1) + 0.1, # individual plot margins
    oma = c(3, 3, 0, 0), # margins for whole figure
    ljoin = 2, # bevelled line joins
    tck = -0.05) # length of tick marks

# translation plot
plot(1:10, 
     test.trans.a, 
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(1, 3.05))

lines(test.trans.b+1.05, 
      type = "l", 
      lty = 2)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(a)"), 
     cex = 2)

text(x = 8.5, 
     y = 2.8, 
     labels = ("translation"), 
     cex = 2)

# amplitude plot
plot(1:10, 
     test.amp.a, 
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "",
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(1, 3.05))

lines(test.amp.b+0.05, 
      type = "l", 
      lty = 2)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(b)"), 
     cex = 2)

text(x = 8.6, 
     y = 2.8, 
     labels = ("amplitude"), 
     cex = 2)

# white noise plot
plot(1:10, 
     test.wn.a/2.167, 
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(0.46, 3.05))

lines(test.wn.b/2.167+0.05, 
      type = "l", 
      lty = 2)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(c)"), 
     cex = 2)

text(x = 8.4, 
     y = 2.8, 
     labels = ("white noise"), 
     cex = 2)

# biased noise plot
plot(1:10, 
     test.bn.a/2, 
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(0.5, 3.05))

lines(test.bn.b/2+0.05, 
      type = "l", 
      lty = 2)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(d)"), 
     cex = 2)

text(x = 8.2, 
     y = 2.8, 
     labels = ("biased noise"), 
     cex = 2)

# outlier plot
plot(1:10, 
     test.out.a/2,
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(0.5, 3.05))

lines(test.out.b/2+0.05, 
      type = "l", 
      lty = 2)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(e)"), 
     cex = 2)

text(x = 9, 
     y = 2.8, 
     labels = ("outliers"), 
     cex = 2)

# phase shift plot
plot(1:10, 
     test.phase.a/1.33, 
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(0.75, 3.05))

lines(test.phase.b/1.33+0.05, 
      type = "l", 
      lty = 2)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(f)"), 
     cex = 2)

text(x = 9.1, 
     y = 2.8, 
     labels = ("phase"), 
     cex = 2)

# time scaling plot
plot(1:11, 
     test.tsc.b+0.05, 
     type = "l", 
     lty = 2, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(1, 3.05), 
     xlim = c(1, 10))

lines(test.tsc.a, 
      type = "l", 
      lty = 1)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(g)"), 
     cex = 2)

text(x = 8.3, 
     y = 2.8, 
     labels = ("time scaling"), 
     cex = 2)

# warping plot
plot(1:11, 
     test.warp.b+0.05, 
     type = "l", 
     lty = 2, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(1, 3.05), 
     xlim = c(1, 10))

lines(test.warp.a, 
      type = "l", 
      lty = 1)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(h)"), 
     cex = 2)

text(x = 8.9, 
     y = 2.8, 
     labels = ("warping"), 
     cex = 2)

# duration plot
plot(1:10, 
     test.dur.a, 
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(1, 3.05))

lines(test.dur.b+0.05, 
      type = "l", 
      lty = 2)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(i)"), 
     cex = 2)

text(x = 8.9, 
     y = 2.8, 
     labels = ("duration"), 
     cex = 2)

# frequency plot
plot(1:10, 
     test.freq.a, 
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     ylim = c(1, 3.05))

lines(test.freq.b+0.05, 
      type = "l", 
      lty = 2)

axis(side = 1, 
     labels = FALSE, 
     at = c(0,1,2,3,4,5,6,7,8,9,10))

text(x = 1.2, 
     y = 2.8, 
     labels = ("(j)"), 
     cex = 2)

text(x = 8.6, 
     y = 2.8, 
     labels = ("frequency"), 
     cex = 2)

# add axis labels
mtext(text = "value", 
      side = 2, 
      outer = TRUE, 
      cex = 1.5, 
      line = 1)

mtext(text = "time", 
      side = 1, 
      outer = TRUE, 
      cex = 1.5, 
      line = 1.5)

# close tiff file
dev.off()

