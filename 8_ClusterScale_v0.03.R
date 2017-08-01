# title: scale method 


# set save folder 
agile.saveFolder = "~/È­¼ö¸ñ/output/"


# choose non binary variables
agile.trainDt <- inputDt[,mget(clusterVarLeft)]
NonBinaryVars <- (agile.trainDt[,lapply(.SD, function(x) length(unique(x))>2)]==TRUE)


# choose scale method 
agile.answer  <- readline(prompt = "choose scale method between standard(=1) or min-max(=2) ?")



# standard normalization except binary variables
if(agile.answer==1){
  
  if(sum(NonBinaryVars)!=length(clusterVarLeft)){
    cat("\n")
    cat("binary variable(s) are  found: ",colnames(NonBinaryVars)[!NonBinaryVars])
    cat("\n")
    cat("scale the following variables: ",colnames(NonBinaryVars)[NonBinaryVars],"\n")
  } else {
    cat("\n")  
    cat("scale the following variables: ",colnames(NonBinaryVars)[NonBinaryVars],"\n")
  } 
  
  # scale
  clusterVarScale       <- colnames(NonBinaryVars)[NonBinaryVars]
  trainScaled           <- scale(inputDt[,mget(clusterVarScale)])
  attrTrainScaledCenter <- attr(trainScaled,"scaled:center")
  attrTrainScaledScale  <- attr(trainScaled,"scaled:scale")
  print(data.frame(mean=round(apply(trainScaled,2,mean),1),sd=apply(trainScaled,2,sd),var=apply(trainScaled,2,var)))
  
  # save 
  tryCatch(rm(scaleInfo),warning=function(w) cat(""))
  scaleInfo <- list(center=attrTrainScaledCenter,scale=attrTrainScaledScale)
  saveRDS(scaleInfo,paste0(agile.saveFolder,"scaleInfo.Rda"))
  
}



# min-max normalization except binary variables
if(agile.answer==2){
  
  if(sum(NonBinaryVars)!=length(clusterVarLeft)){
    cat("\n")
    cat("binary variable(s) are  found: ",colnames(NonBinaryVars)[!NonBinaryVars])
    cat("\n")
    cat("scale the following variables: ",colnames(NonBinaryVars)[NonBinaryVars],"\n")
  } else { 
    cat("\n")  
    cat("scale the following variables: ",colnames(NonBinaryVars)[NonBinaryVars],"\n")
  }
  
  # scale
  clusterVarScale       <- colnames(NonBinaryVars)[NonBinaryVars]
  attrTrainScaledMax    <- apply(inputDt[,mget(clusterVarScale)],2,max)
  attrTrainScaledMin    <- apply(inputDt[,mget(clusterVarScale)],2,min)
  trainScaled           <- scale(inputDt[,mget(clusterVarScale)],center=attrTrainScaledMin,scale=attrTrainScaledMax-attrTrainScaledMin)
  print(setNames(data.frame(t((apply(trainScaled,2,range)))),c("min","max")))
  
  # save
  tryCatch(rm(scaleInfo),warning=function(w) cat(""))
  scaleInfo <- list(max=attrTrainScaledMax,min=attrTrainScaledMin)
  saveRDS(scaleInfo,paste0(agile.saveFolder,"scaleInfo.Rda"))
  
}




# error message 
"%ni%"=Negate("%in%")
if(agile.answer %ni% c(1,2) ){
  cat("wrong number! choose gain.","\n")
  cat("choose scale method between standard(=1) or min-max(=2) ?")
}



rm(list=ls(pattern="agile"))
rm(NonBinaryVars)


