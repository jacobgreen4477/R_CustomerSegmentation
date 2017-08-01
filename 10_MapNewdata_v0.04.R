# title: map newdata 


# set load folder 
agile.loadFolder = "~/화수목/output/"

# 신규고객
testId <- testDt[,.(CustomerID)]
testDt <- testDt[,mget(clusterVarLeft)]

# node, cluster정보 
somNodeClusterDt <- data.table(node=somModel$unit.classif,cluster=clusterNode[somModel$unit.classif])
somNodeClusterDt <- unique(somNodeClusterDt)


# scale조정
scaleInfo <- readRDS(paste0(agile.loadFolder,"scaleInfo.Rda"))
if(paste(names(scaleInfo),collapse="")=="maxmin"){
  cat("use the following scale information: ","\n")
  print(data.frame(scaleInfo))
  testMtx  <- as.matrix(scale(testDt,center=scaleInfo$min,scale=scaleInfo$max-scaleInfo$min))
} else if(paste(names(scaleInfo),collapse="")=="centerscale"){
  cat("use the following scale information: ","\n")
  print(data.frame(scaleInfo))
  testMtx  <- as.matrix(scale(testDt,center=scaleInfo$center,scale=scaleInfo$scale))
} else cat("incorrect scale information!")


# 노드 할당 
outputMap <- kohonen::map(somModel,testMtx)                            
outputDt  <- data.table(testId,node=outputMap$unit.classif)


# 군집 할당 
setkey(somNodeClusterDt,node)                                                
setkey(outputDt,node)
outputDt <- outputDt[somNodeClusterDt]


#cat("done!")


















