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
      ylab = "", zlab = "", theta = "-25", phi = "25", ltheta = "50",
      col = "Yellow", border = NA, main = "kNN", d = "0.5", scale = FALSE,
      expand = 3, shade = 0.9)

#kernel distance viz
persp(Xseq, Yseq,
      matrix(Kdist, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = "-20", phi = "35", ltheta = "50",
      col = 2, border = NA, main = "Kernel Distance", d = "0.5", scale = FALSE,
      expand = 3, shade = 0.9)

#Bootstrap confidence bands
# for kernel density estimators, distance to measure, and kernel distance, 
#and use it in the framework of persistent homology

band <- bootstrapBand(X = X, FUN = kde, Grid = Grid, B = 100,
                      parallel = FALSE, alpha = 0.1, h = h)
band

#plot(band,
#     main = "KDE Diagram")

#gridDiag
# gridDiag function to compute the persistent
# homology of sublevel (and superlevel) sets of the functions

Diag <- gridDiag(X = X, FUN = kde, h = 0.3, lim = cbind(Xlim, Ylim),
                  by = by, sublevel = FALSE, library = "Dionysus",
                  printProgress = FALSE)

Diag

#viz of persistent homology using the grid diag function
plot(Diag[["diagram"]], band = 2 * band[["width"]],
     main = "KDE Diagram")

#The plot on the right shows the persistence diagram of the superlevel sets of the
#KDE. Black points represent connected components and red triangles represent loops. The
#features are born at high levels of the density and die at lower levels. The pink 90% confidence
#band separates significant features from noise.

plot(Diag[["diagram"]], rotated = TRUE, band = band[["width"]],
      main = "Rotated Diagram")

plot(Diag[["diagram"]], barcode = TRUE, main = "Barcode")


#Rips Diag
#The ripsDiag function computes the persistence diagram of the Rips filtration built on top
#of a point cloud. The user can choose to compute the Rips persistence diagram using either
#the C++ library GUDHI, or Dionysus.

Circle1 <- circleUnif(60)
Circle2 <- circleUnif(60, r = 2) + 3
plot(Circle1)
plot(Circle2)
plot(circleUnif(60, r = 2))
Circles <- rbind(Circle1, Circle2)
plot(Circles)

maxscale <- 5 # limit of the filtration
maxdimension <- 1 # components and loops

#persistence diagram from ripsDiag function
Diag_rips <- ripsDiag(X = Circles, maxdimension, maxscale,
                 library = "GUDHI", printProgress = FALSE)

plot(Circles)
plot(Diag_rips[["diagram"]])
#Black points represent connected components and red
#triangles represent loops

# other viz available from different functions such as 
# 1. Alpha complex persistence
# 2. Persistence diagram of alpha shape
# 3. Bottleneck and Wasserstein Distances
# 4. Landscapes and Silhouettes


#selection of smoothing parameters
XX1 <- circleUnif(600)
XX2 <- circleUnif(1000, r = 1.5) + 2.5
noise <- cbind(runif(80, -2, 5), runif(80, -2, 5))
X_smooth <- rbind(XX1, XX2, noise)
# Grid limits
Xlim <- c(-2, 5)
Ylim <- c(-2, 5)
by <- 0.2

parametersKDE <- seq(0.1, 0.6, by = 0.05)
B <- 50 # number of bootstrap iterations. Should be large.
alpha <- 0.1 # level of the confidence bands

maxKDE <- maxPersistence(kde, parametersKDE, X_smooth,
                         lim = cbind(Xlim, Ylim), by = by, sublevel = FALSE,
                         B = B, alpha = alpha, parallel = TRUE,
                         printProgress = TRUE, bandFUN = "bootstrapBand")

maxKDE_unparallel <- maxPersistence(kde, parametersKDE, X_smooth,
                         lim = cbind(Xlim, Ylim), by = by, sublevel = FALSE,
                         B = B, alpha = alpha, parallel = FALSE,
                         printProgress = FALSE, bandFUN = "bootstrapBand")

print(summary(maxKDE))

plot(X_smooth, pch = 16, cex = 0.5, main = "Two Circles")
plot(maxKDE_unparallel, main = "Max Persistence - KDE")
plot(maxKDE, main = "Max Persistence - KDE")

#Density Clustering
#Let f be the density of the probability distribution P
#generating the observed sample X = {x1, . . . , xn} ⊂ Rd. 
#For a threshold value λ > 0, the corresponding super level set 
#of f is Lf (λ) := cl({x ∈ Rs: f(x) > λ}), 
#and its d-dimensional
#subsets are called high-density regions. The high-density clusters of P are the maximal
#connected subsets of Lf (λ). By considering all the level sets simultaneously 
#(from λ = 0 to λ = ∞), 
#we can record the evolution and the hierarchy of the high-density clusters of P.
#This naturally leads to the notion of the cluster density tree of P (see, e.g., Hartigan (1981)),
#defined as the collection of sets T := {Lf (λ), λ ≥ 0},
#which satisfies the tree property:
#A, B ∈ T implies that A ⊂ B or B ⊂ A or A∩B = ∅.

