getwd()
install.packages("Hmisc")
library(Hmisc)
install.packages("TDA")
library(TDA)

X <- circleUnif(400)
X
describe(X)
summary(X)
Xlim <- c(-1.6, 1.6)
Ylim <- c(-1.7, 1.7)
by <- 0.065

Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Ylim[1], Ylim[2], by = by)
Grid <- expand.grid(Xseq, Yseq)

# distance function
distance <- distFct(X = X, Grid = Grid)
distance

m0 <- 0.1

#dtm function

DTM <- dtm(X = X, Grid = Grid, m0 = m0)
dtm

#kNN density estimator
k <- 60
kNN <- knnDE(X = X, Grid = Grid, k = k)
kNN



#kernel distance estimator
h <- 0.3
KDE <- kde(X = X, Grid = Grid, h = h)
Kdist <- kernelDist(X = X, Grid = Grid, h = h)
Kdist

#viz a 2 dimensional example 
#using persp from the graphics package

attach(mtcars)
par(mfrow=c(1,1))
#kde viz
persp(Xseq, Yseq,
      matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = "-20", phi = "35", ltheta = "50",
      col = 2, border = NA, main = "KDE", d = "0.5", scale = FALSE,
      expand = 3, shade = 0.9)

#distance function viz
persp(Xseq, Yseq,
      matrix(distance, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = "-20", phi = "35", ltheta = "50",
      col = 2, border = NA, main = "Distance Function", d = "0.5", scale = FALSE,
      expand = 3, shade = 0.9)

#dtm function viz
persp(Xseq, Yseq,
      matrix(dtm, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = "-20", phi = "35", ltheta = "50",
      col = 2, border = NA, main = "DTM", d = "0.5", scale = FALSE,
      expand = 3, shade = 0.9)

#knn viz
persp(Xseq, Yseq,
      matrix(kNN, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = "-20", phi = "35", ltheta = "50",
      col = 2, border = NA, main = "kNN", d = "0.5", scale = FALSE,
      expand = 3, shade = 0.9)

#kernel distance viz
persp(Xseq, Yseq,
      matrix(Kdist, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = "-20", phi = "35", ltheta = "50",
      col = 2, border = NA, main = "Kernel Distance", d = "0.5", scale = FALSE,
      expand = 3, shade = 0.9)
