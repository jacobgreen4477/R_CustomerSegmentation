# title: choose mapsize 


# set save folder
agile.saveFolder = "~/È­¼ö¸ñ/output/"


# scale train dataset 
agile.trainDt       <- inputDt[,mget(clusterVarLeft)]
NonBinaryVars       <- (agile.trainDt[,lapply(.SD, function(x) length(unique(x))>2)]==TRUE)
clusterVarScale     <- colnames(NonBinaryVars)[NonBinaryVars]
attrTrainScaledMax  <- apply(inputDt[,mget(clusterVarScale)],2,max)
attrTrainScaledMin  <- apply(inputDt[,mget(clusterVarScale)],2,min)
agile.data          <- scale(inputDt[,mget(clusterVarScale)],center=attrTrainScaledMin,scale=attrTrainScaledMax-attrTrainScaledMin)


# masize using som 
agile.qntErr        <- list()
agile.emptyNodesCts <- list()
agile.xydim         <- seq(agile.minMapSize,agile.maxMapSize,by=5)
agile.xydimOpt      <- data.frame(cbind(matrix(rep(agile.xydim,each=2),nrow=2)))
agile.xydimOpt      <- data.frame(t(agile.xydimOpt))
agile.xydimOpt      <- agile.xydimOpt[order(agile.xydimOpt$X1),]
agile.mapSizeOpt    <- split(as.matrix(agile.xydimOpt), seq(nrow(agile.xydimOpt)))
agile.pb            <- progress_bar$new(total=length(agile.mapSizeOpt))
agile.ptm           <- Sys.time()


for(i in 1:length(agile.mapSizeOpt)){
  
  # options 
  nSomX <- agile.mapSizeOpt[[i]][1]
  nSomY <- agile.mapSizeOpt[[i]][2]
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
  agile.secondCluster  <- kmeans(agile.somModel$codes,agile.clusterGroup,nstart=30,iter.max=300)$cluster
  
 
  # codes plot
  agile.mapSize <- paste0(agile.somModel$grid$xdim,"x",agile.somModel$grid$ydim)
  
  png(file = paste0(agile.saveFolder,"codes_",agile.mapSize,".png"))
  plot(agile.somModel
       ,type = "codes"
       ,bgcol = nicePalette[agile.secondCluster]
       ,main = paste0("Codes_",agile.mapSize)
  )
  add.cluster.boundaries(agile.somModel, agile.secondCluster)
  dev.off()
  
  
  # counts plot
  png(file = paste0(agile.saveFolder,"counts_",agile.mapSize,".png"))
  plot(agile.somModel
       ,type = "counts"
       ,bgcol = nicePalette[agile.secondCluster]
       ,main = paste0("Counts_",agile.mapSize)
  )
  dev.off()
  graphics.off()
  

  # quantization error
  agile.qntErr[i]  <- mean(agile.somModel$distances)
  
  
  # empty node counts
  agile.totalNodes <- nrow(agile.somModel$codes)
  agile.emptyNodesCts[[i]] <- agile.totalNodes-length(unique(agile.somModel$unit.classif))

  
  # progress bar
  agile.pb$tick()
  Sys.sleep(1/100)
  
}

Sys.time() - agile.ptm


# save mapSize test
qntErr <- data.frame(agile.xydimOpt,QntErr=unlist(agile.qntErr),EmptyNode=unlist(agile.emptyNodesCts))
qntErr$QntErrDelta    <- round(c(Delt(qntErr$QntErr)),2)
qntErr$EmptyNodeRatio <- qntErr$EmptyNode/(qntErr$X1*qntErr$X2)
qntErr <-qntErr[,c("X1","X2","QntErr","QntErrDelta","EmptyNode","EmptyNodeRatio")]
rownames(qntErr) <- NULL
write.csv(qntErr,paste0(agile.saveFolder,"Test4MapSize.csv"),row.names = F)
qntErr


rm(list=ls(pattern="agile"))
cat("done!")





