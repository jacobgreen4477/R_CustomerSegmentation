# title: run som 


# 式式式式式式式式式式式式式式式式式式式式式式式式式式式式式
# SOM
# 式式式式式式式式式式式式式式式式式式式式式式式式式式式式式
nSomX <- agile.xdim
nSomY <- agile.ydim
nEpoch <- agile.epoch
gridType <- "rectangular"
radius0 <- 0
radiusN <- 0
radiusCooling <- "linear"
scale0 <- 0
scaleN <- 0.01
scaleCooling <- "linear"
kernelType <- 0
mapType <- "planar"
compactSupport <- FALSE
neighborhood <- "gaussian"
set.seed(agile.seed)

res <- Rsomoclu.train(as.matrix(trainScaled), nEpoch, nSomX, nSomY,
                      radius0, radiusN,
                      radiusCooling, scale0, scaleN,
                      scaleCooling,
                      kernelType, mapType, gridType, compactSupport, neighborhood)

somModel <- Rsomoclu.kohonen(as.matrix(trainScaled), res)
colnames(somModel$codes) <- clusterVarLeft
# 式式式式式式式式式式式式式式式式式式式式式式式式式式式式式

# rm(list=ls(pattern="agile"))
# cat("done!")


