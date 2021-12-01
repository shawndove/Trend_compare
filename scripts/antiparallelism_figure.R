## plot figure ----

# create directory to save figure if it doesn't exist
if(!dir.exists("figures/")) {dir.create("figures/")}

# create tiff file
tiff(file = "figures/antiparallelism.tiff", 
     compression = "lzw", 
     res = 1000,
     width = 3543, 
     height = 2755, 
     units = "px", 
     pointsize = 12, 
     bg = "white")

# set parameters
par(mfrow = c(1, 1), # row and column structure
    mar = c(1.5, 2, 0.5, 0.5) + 0.1, # plot margins
    ljoin = 2, # bevelled line joins
    tck = -0.03) # length of tick marks

# line x
plot(1:2, 
     c(2, 3), 
     type = "l", 
     lty = 1, 
     xlab = "", 
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     xlim = c(0.8, 2.2),
     ylim = c(0, 6))

# line y
lines(c(2, 1), 
      type = "l", 
      lty = 1)

# line z
lines(c(2, 5), 
      type = "l", 
      lty = 1)

# dotted line
abline(h = 2,
       col = "grey50",
       lty = 2)

axis(side = 2, 
     labels = TRUE,
     line = 0,
     at = c(0,1,2,3,4,5,6))

text(x = 2, 
     y = 4, 
     labels = ("(positive slope)"), 
     cex = 0.7)

text(x = 2, 
     y = 0.3, 
     labels = ("(negative slope)"), 
     cex = 0.7)

text(x = 2.1, 
     y = 3, 
     labels = ("x"), 
     cex = 1.2)

text(x = 2.1, 
     y = 1, 
     labels = ("y"), 
     cex = 1.2)

text(x = 2.1, 
     y = 5, 
     labels = ("z"), 
     cex = 1.2)

# add x-axis label
mtext(text = "Time", 
      side = 1, 
      cex = 1, 
      line = 0.5)

# close tiff file
dev.off()