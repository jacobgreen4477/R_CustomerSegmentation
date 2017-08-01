# title: scale each 


# 1. scale traindata 
# 2. scale testdata 


# 1. scale traindata 
# ________________________________________________________________________________________________
scaledVarList <- list()
scaleInfo     <- list(list(min=NA,max=NA),list(center=NA,scale=NA))


for(i in 1:5){
  
  agile.tmpVar <- colnames(inputDt)[i]
  agile.answer <- readline(prompt = "please choose scale type between min-max(=1) and stardard(=2)")

  if(agile.answer==1){
    # apply min-max normalization 
    agile.tmpMin <- min(inputDt[,list(get(agile.tmpVar))],na.rm=TRUE)
    agile.tmpMax <- max(inputDt[,list(get(agile.tmpVar))],na.rm=TRUE)
    scaleInfo[[1]]$min <- mapply(`[<-`, scaleInfo[[1]]$min, agile.tmpVar, value = agile.tmpMin, SIMPLIFY = FALSE)
    scaleInfo[[1]]$max <- mapply(`[<-`, scaleInfo[[1]]$max, agile.tmpVar, value = agile.tmpMax, SIMPLIFY = FALSE)
    scaledVarList[[agile.tmpVar]]<-inputDt[,lapply(.SD,function(x) scale(x,center=agile.tmpMin,scale=agile.tmpMax-agile.tmpMin))
                                           ,.SDcols=agile.tmpVar]

  } else if(agile.answer==2){
    # apply standard   
    scaledVarList[[agile.tmpVar]]<-inputDt[,lapply(.SD,scale),.SDcols=agile.tmpVar]
    agile.tmpCenter <- attr(scaledVarList[[agile.tmpVar]][[1]],"scaled:center")
    agile.tmpScale  <- attr(scaledVarList[[agile.tmpVar]][[1]],"scaled:scale")
    scaleInfo[[2]]$center <- mapply(`[<-`, scaleInfo[[2]]$center, agile.tmpVar, value = agile.tmpCenter, SIMPLIFY = FALSE)
    scaleInfo[[2]]$scale  <- mapply(`[<-`, scaleInfo[[2]]$scale,  agile.tmpVar, value = agile.tmpScale, SIMPLIFY = FALSE)
    
  } else cat("wrong number!")
  
}


# remove NA from scale information table
tryCatch({
scaleInfo[[1]]$min[[1]]    <- scaleInfo[[1]]$min[[1]][!is.na(scaleInfo[[1]]$min[[1]])]
scaleInfo[[1]]$max[[1]]    <- scaleInfo[[1]]$max[[1]][!is.na(scaleInfo[[1]]$max[[1]])]
},error=function(e) cat(""))
tryCatch({
scaleInfo[[2]]$center[[1]] <- scaleInfo[[2]]$center[[1]][!is.na(scaleInfo[[2]]$center[[1]])]
scaleInfo[[2]]$scale[[1]]  <- scaleInfo[[2]]$scale[[1]][!is.na(scaleInfo[[2]]$scale[[1]])]
},error=function(e) cat(""))
print(scaleInfo)


# combine scaled variables 
names(scaledVarList)<-NULL
trainScaled <- do.call(cbind,scaledVarList)


# check the range of scaled variables 
lapply(trainScaled,range)
lapply(trainScaled,function(x) round(mean(x),2))
head(trainScaled)


# save scale information table
saveRDS(scaleInfo,"~/scaleInfo.Rda")


rm(list=ls(pattern="agile"))
cat("done!")
# ________________________________________________________________________________________________




# 2. scale testdata  
# ________________________________________________________________________________________________

# load dataset
scaleInfo <- readRDS("~/scaleInfo.Rda")
testDt    <- inputDt


# distinguish scale type 
agile.minMaxVarsName   <- names(scaleInfo[[1]]$min[[1]])
agile.standardVarsName <- names(scaleInfo[[2]]$scale[[1]])


# apply min-max normalization 
tryCatch({
agile.minMaxVars       <- scale(testDt[,mget(agile.minMaxVarsName)]
                          ,center=scaleInfo[[1]]$min[[1]]
                          ,scale=scaleInfo[[1]]$max[[1]]-scaleInfo[[1]]$min[[1]])
},error=function(e) cat("min-max information does NOT exist!"))


# apply standard normalization 
tryCatch({
agile.standardVars     <- scale(testDt[,mget(agile.standardVarsName)]
                          ,center=scaleInfo[[2]]$center[[1]]
                          ,scale=scaleInfo[[2]]$scale[[1]])
},error=function(e) cat("center-scale information does NOT exist!"))


# combine scaled variables 
is_exist <- function(x) tryCatch(is.matrix(x),error=function(e) return(FALSE))
is_empty <- function(x) (!is_exist(x))

if(is_empty(agile.standardVars)){
  testScaled <- data.table(agile.minMaxVars)
} else if(is_empty(agile.minMaxVars)){
  testScaled <- data.table(agile.standardVars)
} else if((is_exist(agile.standardVars)+is_exist(agile.minMaxVars))==2){
  testScaled <- data.table(c(agile.minMaxVars,agile.standardVars))
} else cat("error")


# check the range of scaled variables 
lapply(testScaled,range)
lapply(testScaled,function(x) round(mean(x),2))
head(testScaled)


# convert to matrix for kohonen 
testMtx <- as.matrix(testScaled)


rm(list=ls(pattern="agile"))
cat("done!")
# ________________________________________________________________________________________________






















