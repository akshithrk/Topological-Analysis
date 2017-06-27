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

#X data viz
persp(Xseq, Yseq,
      matrix(X, ncol = length(Yseq), nrow = length(Xseq)), xlab = "Xseq on X-axis",
      ylab = "Yseq on Y-axis", zlab = "Z-axis", theta = "-30", phi = "20", ltheta = "30",
      col = 3, border = NA, main = "X", d = "0.5", scale = FALSE,
      expand = 0.5, shade = 0.9)

#presp = perspective plot
#persp() returns the viewing transformation matrix, say VT, a 4 x 4 matrix suitable for projecting 3D coordinates (x,y,z) 
#into the 2D plane using homogeneous 4D coordinates (x,y,z,t). 
#It can be used to superimpose additional graphical elements on the 3D plot, by lines() or points(), using the function trans3d().
#https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/persp.html
#theta = turn plot right or view plot from left
#phi = view plot from top
#ltheta = color intensity
#d = expands the coordinate point box in which the plot exists
#explnd = expland/stretch the graph. lesser number bigger the plot i.e. more stretch


#kde viz
persp(Xseq, Yseq,
      matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = "-30", phi = "25", ltheta = "50",
      col = 2, border = NA, main = "KDE", d = "0.3", scale = FALSE,
      expand = 2, shade = 0.9)

#distance function viz
persp(Xseq, Yseq,
      matrix(distance, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = "-30", phi = "35", ltheta = "50",
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
