# create time series for testing properties of distance measures

# Uniqueness ----

test.unq.a <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
test.unq.b <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)

tslistx.unq <- list(test.unq.b)
tslisty.unq <- list(test.unq.a)

# Symmetry ----

test.sym.a <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
test.sym.b <- c(3, 0.1, 1, 3, 0.1, 2, 4, 2, 2, 2)

tslistx.sym <- list(test.sym.b,
                    test.sym.a)
tslisty.sym <- list(test.sym.a,
                    test.sym.b)

# Translation sensitivity ----

test.trans.a <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
test.trans.b <- c(1.1, 1.1, 2.1, 2.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1)
test.trans.c <- c(1.2, 1.2, 2.2, 2.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2)
test.trans.d <- c(1.3, 1.3, 2.3, 2.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3)
test.trans.e <- c(1.4, 1.4, 2.4, 2.4, 1.4, 1.4, 1.4, 1.4, 1.4, 1.4)
test.trans.f <- c(1.5, 1.5, 2.5, 2.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)

tslistx.trans <- list(test.trans.b,
                    test.trans.c,
                    test.trans.d,
                    test.trans.e,
                    test.trans.f)

tslisty.trans <- list(test.trans.a,
                    test.trans.a,
                    test.trans.a,
                    test.trans.a,
                    test.trans.a)

# Phase invariance ----

test.phase.a <- c(1, 4, 2, 1, 3, 1, 1, 4, 1, 5)
test.phase.b <- c(5, 1, 4, 2, 1, 3, 1, 1, 4, 1)
test.phase.c <- c(1, 5, 1, 4, 2, 1, 3, 1, 1, 4)
test.phase.d <- c(4, 1, 5, 1, 4, 2, 1, 3, 1, 1)
test.phase.e <- c(1, 4, 1, 5, 1, 4, 2, 1, 3, 1)
test.phase.f <- c(1, 1, 4, 1, 5, 1, 4, 2, 1, 3)

tslistx.phase <- list(test.phase.b,
                      test.phase.c,
                      test.phase.d,
                      test.phase.e,
                      test.phase.f)

tslisty.phase <- list(test.phase.a,
                      test.phase.a,
                      test.phase.a,
                      test.phase.a,
                      test.phase.a)

# Uniform time scaling invariance ----

test.uni.a <- c(1, 2, 3, 2, 1, 1, 2, 3, 2, 1, 1)
test.uni.b <- c(1, 1.66, 2.33, 3, 2.33, 1.66, 1, 1, 1.33, 2, 2.66, 2.66, 2, 1.33, 1, 1)
test.uni.c <- c(1, 1.5, 2, 2.5, 3, 2.5, 2, 1.5, 1, 1, 1, 1.5, 2, 2.5, 3, 2.5, 2, 1.5, 1, 1, 1)
test.uni.d <- c(1, 1.4, 1.8, 2.2, 2.6, 3, 2.6, 2.2, 1.8, 1.4, 1, 1, 1, 1.2, 1.6, 2, 2.4, 2.8, 2.8, 2.4, 2, 1.6, 1.2, 1, 1, 1)
test.uni.e <- c(1, 1.33, 1.66, 2, 2.33, 2.66, 3, 2.66, 2.33, 2, 1.66, 1.33, 1, 1, 1, 1, 1.33, 1.66, 2, 2.33, 2.66, 3, 2.66, 2.33, 2, 1.66, 1.33, 1, 1, 1, 1)

tslistx.uni <- list(test.uni.b,
                    test.uni.c,
                    test.uni.d,
                    test.uni.e)
tslisty.uni <- list(test.uni.a,
                    test.uni.a,
                    test.uni.a,
                    test.uni.a)

# Warping invariance (local time scaling invariance) ----

test.warp.a <- c(1, 2, 3, 2, 1, 1, 2, 3, 2, 1)
test.warp.b <- c(1, 2, 3, 2, 1, 1, 1, 2, 3, 2, 1)
test.warp.c <- c(1, 2, 3, 2, 1, 1, 1, 1, 2, 3, 2, 1)
test.warp.d <- c(1, 2, 3, 2, 1, 1, 1, 1, 1, 2, 3, 2, 1)
test.warp.e <- c(1, 2, 3, 2, 1, 1, 1, 1, 1, 1, 2, 3, 2, 1)
test.warp.f <- c(1, 2, 3, 2, 1, 1, 1, 1, 1, 1, 1, 2, 3, 2, 1)

tslistx.warp <- list(test.warp.b,
                     test.warp.c,
                     test.warp.d,
                     test.warp.e,
                     test.warp.f)

tslisty.warp <- list(test.warp.a,
                     test.warp.a,
                     test.warp.a,
                     test.warp.a,
                     test.warp.f)

# Amplitude sensitivity ----

test.amp.a <- c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1)
test.amp.b <- c(1, 1, 2.5, 2.5, 1, 1, 2, 2, 1, 1)
test.amp.c <- c(1, 1, 3, 3, 1, 1, 2, 2, 1, 1)
test.amp.d <- c(1, 1, 3.5, 3.5, 1, 1, 2, 2, 1, 1)
test.amp.e <- c(1, 1, 4, 4, 1, 1, 2, 2, 1, 1)
test.amp.f <- c(1, 1, 4.5, 4.5, 1, 1, 2, 2, 1, 1)

tslistx.amp <- list(test.amp.b,
                    test.amp.c,
                    test.amp.d,
                    test.amp.e,
                    test.amp.f)

tslisty.amp <- list(test.amp.a,
                    test.amp.a,
                    test.amp.a,
                    test.amp.a,
                    test.amp.a)

# Duration Sensitivity ----

test.dur.a <- c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
test.dur.b <- c(1, 1, 2, 2, 2, 1, 1, 1, 1, 1)
test.dur.c <- c(1, 1, 2, 2, 2, 2, 1, 1, 1, 1)
test.dur.d <- c(1, 1, 2, 2, 2, 2, 2, 1, 1, 1)
test.dur.e <- c(1, 1, 2, 2, 2, 2, 2, 2, 1, 1)
test.dur.f <- c(1, 1, 2, 2, 2, 2, 2, 2, 2, 1)

tslistx.dur <- list(test.dur.b,
                    test.dur.c,
                    test.dur.d,
                    test.dur.e,
                    test.dur.f)

tslisty.dur <- list(test.dur.a,
                    test.dur.a,
                    test.dur.a,
                    test.dur.a,
                    test.dur.a)

# Frequency Sensitivity ----

test.freq.a <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
test.freq.b <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1)
test.freq.c <- c(1, 2, 1, 2, 1, 1, 1, 1, 1, 1)
test.freq.d <- c(1, 2, 1, 2, 1, 2, 1, 1, 1, 1)
test.freq.e <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 1)
test.freq.f <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)

tslistx.freq <- list(test.freq.b,
                     test.freq.c,
                     test.freq.d,
                     test.freq.e,
                     test.freq.f)

tslisty.freq <- list(test.freq.a,
                     test.freq.a,
                     test.freq.a,
                     test.freq.a,
                     test.freq.a)

# Antiparallelism Bias ----

test.par.a <- c(1, 1, 1.5, 1.5, 1, 1, 1, 1, 1, 1)
test.par.b <- c(1, 1, 0.5, 0.5, 1, 1, 1, 1, 1, 1)
test.par.c <- c(1, 1, 2.5, 2.5, 1, 1, 1, 1, 1, 1)

tslistx.par <- list(test.par.b,
                     test.par.c)

tslisty.par <- list(test.par.a,
                     test.par.a)

# White Noise Sensitivity ----

test.wn.a <- c(1, 2, 3, 6, 5, 6, 4, 2, 2, 1)
test.wn.b <- c(1, 2.125, 2.875, 6.125, 4.875, 6.125, 3.875, 2.125, 1.875, 1)
test.wn.c <- c(1, 2.25, 2.75, 6.25, 4.75, 6.25, 3.75, 2.25, 1.75, 1)
test.wn.d <- c(1, 2.375, 2.625, 6.375, 4.625, 6.375, 3.625, 2.375, 1.625, 1)
test.wn.e <- c(1, 2.5, 2.5, 6.5, 4.5, 6.5, 3.5, 2.5, 1.5, 1)
test.wn.f <- c(1, 2.625, 2.375, 6.625, 4.375, 6.625, 3.375, 2.625, 1.375, 1)

tslistx.wn <- list(test.wn.b,
                   test.wn.c,
                   test.wn.d,
                   test.wn.e,
                   test.wn.f)

tslisty.wn <- list(test.wn.a,
                   test.wn.a,
                   test.wn.a,
                   test.wn.a,
                   test.wn.a)

# Biased Noise Sensitivity ----

test.bn.a <- c(1, 2, 3, 6, 5, 6, 4, 2, 2, 1)
test.bn.b <- c(1, 2.25, 3, 6.25, 5, 6.25, 4, 2.25, 2, 1)
test.bn.c <- c(1, 2.5, 3, 6.5, 5, 6.5, 4, 2.5, 2, 1)
test.bn.d <- c(1, 2.75, 3, 6.75, 5, 6.75, 4, 2.75, 2, 1)
test.bn.e <- c(1, 3, 3, 7, 5, 7, 4, 3, 2, 1)
test.bn.f <- c(1, 3.25, 3, 7.25, 5, 7.25, 4, 3.25, 2, 1)

tslistx.bn <- list(test.bn.b,
                   test.bn.c,
                   test.bn.d,
                   test.bn.e,
                   test.bn.f)

tslisty.bn <- list(test.bn.a,
                   test.bn.a,
                   test.bn.a,
                   test.bn.a,
                   test.bn.a)

# Outlier Sensitivity ----

test.out.a <- c(1, 2, 3, 2, 4, 3, 4, 2, 2, 1)
test.out.b <- c(1, 2, 3, 2, 5, 3, 4, 2, 2, 1)
test.out.c <- c(1, 2, 3, 2, 6, 3, 4, 2, 2, 1)
test.out.d <- c(1, 2, 3, 2, 7, 3, 4, 2, 2, 1)
test.out.e <- c(1, 2, 3, 2, 8, 3, 4, 2, 2, 1)
test.out.f <- c(1, 2, 3, 2, 9, 3, 4, 2, 2, 1)

tslistx.out <- list(test.out.b,
                    test.out.c,
                    test.out.d,
                    test.out.e,
                    test.out.f)

tslisty.out <- list(test.out.a,
                    test.out.a,
                    test.out.a,
                    test.out.a,
                    test.out.a)

# Non-Positive Values Handling Test ----

test.np.a <- c(1, 2, 3, 2, 1, 1, 1, 1, 1, 1)
test.np.b <- c(1, 2, 3, 0.000001, 1, 1, 1, 1, 1, 1)
test.np.c <- c(1, 2, 3, 0, 1, 1, 1, 1, 1, 1)
test.np.d <- c(1, 2, 3, -1, 1, 1, 1, 1, 1, 1)

tslistx.np <- list(test.np.b,
                   test.np.c,
                   test.np.d)

tslisty.np <- list(test.np.a,
                   test.np.a,
                   test.np.a)

### create list for testing ----

tsx.list <- list(tslistx.unq,
                 tslistx.sym,
                 tslistx.trans,
                 tslistx.amp,
                 tslistx.dur,
                 tslistx.freq,
                 tslistx.wn,
                 tslistx.bn,
                 tslistx.out,
                 tslistx.par,
                 tslistx.phase,
                 tslistx.uni,
                 tslistx.warp,
                 tslistx.np)

tsy.list <- list(tslisty.unq,
                 tslisty.sym,
                 tslisty.trans,
                 tslisty.amp,
                 tslisty.dur,
                 tslisty.freq,
                 tslisty.wn,
                 tslisty.bn,
                 tslisty.out,
                 tslisty.par,
                 tslisty.phase,
                 tslisty.uni,
                 tslisty.warp,
                 tslisty.np)
