# title: clusterNumSelect


# set save folder
agile.saveFolder = "~/È­¼ö¸ñ/output/"


# scale train dataset 
agile.trainDt       <- inputDt[,mget(clusterVarLeft)]
NonBinaryVars       <- (agile.trainDt[,lapply(.SD, function(x) length(unique(x))>2)]==TRUE)
clusterVarScale     <- colnames(NonBinaryVars)[NonBinaryVars]
attrTrainScaledMax  <- apply(inputDt[,mget(clusterVarScale)],2,max)
attrTrainScaledMin  <- apply(inputDt[,mget(clusterVarScale)],2,min)
agile.data          <- scale(inputDt[,mget(clusterVarScale)],center=attrTrainScaledMin,scale=attrTrainScaledMax-attrTrainScaledMin)


# print dataset information 
cat("the following dataset are used for test: ")
cat("\n")
print(head(agile.data))
cat("\n")
print(setNames(data.frame(t((apply(agile.data,2,range)))),c("min","max")))



# cluster number search test 
# __________________________________________________________________________________________________
agile.wss <- (nrow(agile.data)-1)*sum(apply(agile.data,2,var))
for (i in 2:15){
  set.seed(0)
  agile.wss[i] <- sum(kmeans(agile.data, centers=i,nstart=30,iter.max = 300)$withinss)
} 

tryCatch(dev.off(),error=function(e) cat(""))
png(file = paste0(agile.saveFolder,"clusterNumberTest.png"))
plot(1:15, agile.wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
text(1:15, agile.wss, labels=round(Delt(agile.wss,k=1),2), cex= 0.7, pos=3)
abline(v=5,col="red", lty = 3)
abline(v=10,col="red", lty = 3)
dev.off()

plot(1:15, agile.wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
text(1:15, agile.wss, labels=round(Delt(agile.wss,k=1),2), cex= 0.7, pos=3)
abline(v=5,col="red", lty = 3)
abline(v=10,col="red", lty = 3)
# __________________________________________________________________________________________________



# make som plot with differnet k 
# __________________________________________________________________________________________________
for(k in 2:15){

  # options 
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

  # first cluster
  set.seed(agile.seed)
  agile.res <- Rsomoclu.train(agile.data, nEpoch, nSomX, nSomY,
                        radius0, radiusN,
                        radiusCooling, scale0, scaleN,
                        scaleCooling,
                        kernelType, mapType, gridType, compactSupport, neighborhood)
  
  agile.somModel <- Rsomoclu.kohonen(agile.data, agile.res)
  colnames(agile.somModel$codes) <- clusterVarLeft
  
  
  # second cluster
  set.seed(agile.seed)
  agile.secondCluster  <- kmeans(agile.somModel$codes,k,nstart=30,iter.max=300)$cluster
  
 
  # som plot 
  png(file = paste0(agile.saveFolder,"somCluster_w_",k,".png"))
  plot(agile.somModel
       ,type = "codes"
       ,bgcol = nicePalette[agile.secondCluster]
       ,main = paste0("Codes_",k)
  )
  add.cluster.boundaries(agile.somModel, agile.secondCluster)
  dev.off()
  
  
  # count plot
  png(file = paste0(agile.saveFolder,"somCount_w_",k,".png"))
  plot(agile.somModel
       ,type = "counts"
       ,bgcol = nicePalette[agile.secondCluster]
       ,main = paste0("Counts_",k)
  )
  dev.off()

  
  # silhouette plot
  png(file = paste0(agile.saveFolder,"somSilhouette_w_",k,".png"))
  plot(silhouette(agile.secondCluster,daisy(agile.somModel$codes)^2),col=nicePalette[1:k],main=paste0("cluster ",k))
  dev.off()

}
# __________________________________________________________________________________________________



rm(list=ls(pattern="agile"))
rm(k)
#cat("done!")












