test.unq.a2 <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
test.unq.b2 <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)

test.amp.a2 <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
test.amp.b2 <- c(1, 1, 3, 3, 1, 1, 1, 1, 1, 1)

test.wn.a2 <- c(1, 4, 3, 6, 5, 6, 3, 4, 1, 1)
test.wn.b2 <- c(1, 4.5, 2.5, 6.5, 4.5, 6.5, 2.5, 4.5, 1, 1)

test.bn.a2 <- c(1, 4, 3, 6, 5, 6, 3, 4, 1, 1)
test.bn.b2 <- c(1, 4, 2, 6, 4, 6, 2, 4, 1, 1)

test.out.a2 <- c(1, 4, 3, 6, 5, 6, 3, 4, 1, 1)
test.out.b2 <- c(1, 4, 3, 6, 1, 6, 3, 4, 1, 1)

test.phase.a2 <- c(1, 1, 4, 2, 1, 3, 1, 1, 1, 1)
test.phase.b2 <- c(1, 1, 1, 4, 2, 1, 3, 1, 1, 1)

test.uni.a2 <- c(1, 2, 1, 1, 3, 1)
test.uni.b2 <- c(1, 1.5, 2, 1.5, 1, 1, 1, 2, 3, 2, 1)

test.warp.a2 <- c(1, 1, 2, 1, 1, 3, 1, 1)
test.warp.b2 <- c(1, 1, 2, 1, 1, 2, 3, 2, 1, 1, 1)

test.dur.a2 <- c(1, 1, 3, 3, 1, 1, 1, 1, 1, 1)
test.dur.b2 <- c(1, 1, 3, 3, 3, 1, 1, 1, 1, 1)

test.freq.a2 <- c(1, 3, 1, 1, 1, 1, 1, 1, 1, 1)
test.freq.b2 <- c(1, 3, 1, 1, 3, 1, 1, 1, 1, 1)

tiff(file="figures/properties2.tif", compression="lzw", res=1000,
    width = 7480, height = 9055, units = "px", pointsize = 12, bg = "white")
par(mfrow=c(5,2), mar=c(1,0.5,1,1) + 0.1, oma=c(2,1.5,0,0), ljoin=2, tck = -0.05)

plot(1:10, test.unq.a2, type="l", lty=1, xlab="", ylab="", xaxt = "n", yaxt="n", ylim=c(1,3.05))
lines(test.unq.b2+1.05, type="l", lty=2)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
#mtext(text = "Time", side = 1, line = 1.5)
mtext(text = "Value", side = 2, line = 0.5)
text(1.2, 2.8, labels=("(a)"), cex=2)

plot(1:10, test.amp.a2, type="l", lty=1, xlab="", ylab="",xaxt = "n", yaxt="n", ylim=c(1,3.05))
lines(test.amp.b2+0.05, type="l", lty=2)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
#mtext(text = "Time", side = 1, line = 2.5)
text(1.2, 2.8, labels=("(b)"), cex=2)

plot(1:10, test.wn.a2/2.167, type="l", lty=1, xlab="", ylab="", xaxt = "n", yaxt="n", ylim=c(0.46,3.05))
lines(test.wn.b2/2.167+0.05, type="l", lty=2)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
#mtext(text = "Time", side = 1, line = 2.5)
mtext(text = "Value", side = 2, line = 0.5)
text(1.2, 2.8, labels=("(c)"), cex=2)

plot(1:10, test.bn.a2/2, type="l", lty=1, xlab="", ylab="", xaxt = "n", yaxt="n", ylim=c(0.5,3.05))
lines(test.bn.b2/2+0.05, type="l", lty=2)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
#mtext(text = "Time", side = 1, line = 2.5)
text(1.2, 2.8, labels=("(d)"), cex=2)

plot(1:10, test.out.a2/2, type="l", lty=1, xlab="", ylab="", xaxt = "n", yaxt="n", ylim=c(0.5,3.05))
lines(test.out.b2/2+0.05, type="l", lty=2)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
#mtext(text = "Time", side = 1, line = 2.5)
mtext(text = "Value", side = 2, line = 0.5)
text(1.2, 2.8, labels=("(e)"), cex=2)

plot(1:10, test.phase.a2/1.33, type="l", lty=1, xlab="", ylab="", xaxt = "n", yaxt="n", ylim=c(0.75,3.05))
lines(test.phase.b2/1.33+0.05, type="l", lty=2)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
#mtext(text = "Time", side = 1, line = 2.5)
text(1.2, 2.8, labels=("(f)"), cex=2)

plot(1:11, test.uni.b2+0.05, type="l", lty=2, xlab="", ylab="", xaxt = "n", yaxt="n", ylim=c(1,3.05), xlim=c(1,10))
lines(test.uni.a2, type="l", lty=1)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
#mtext(text = "Time", side = 1, line = 2.5)
mtext(text = "Value", side = 2, line = 0.5)
text(1.2, 2.8, labels=("(g)"), cex=2)

plot(1:11, test.warp.b2+0.05, type="l", lty=2, xlab="", ylab="", xaxt = "n", yaxt="n", ylim=c(1,3.05), xlim=c(1,10))
lines(test.warp.a2, type="l", lty=1)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
#mtext(text = "Time", side = 1, line = 2.5)
text(1.2, 2.8, labels=("(h)"), cex=2)

plot(1:10, test.dur.a2, type="l", lty=1, xlab="", ylab="", xaxt = "n", yaxt="n", ylim=c(1,3.05))
lines(test.dur.b2+0.05, type="l", lty=2)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
mtext(text = "Time", side = 1, line = 1.5)
mtext(text = "Value", side = 2, line = 0.5)
text(1.2, 2.8, labels=("(i)"), cex=2)

plot(1:10, test.freq.a2, type="l", lty=1, xlab="", ylab="", xaxt="n", yaxt="n", ylim=c(1,3.05))
lines(test.freq.b2+0.05, type="l", lty=2)
axis(side=1, labels=FALSE, at=c(0,1,2,3,4,5,6,7,8,9,10))
mtext(text = "Time", side = 1, line = 1.5)
text(1.2, 2.8, labels=("(j)"), cex=2)
dev.off()

