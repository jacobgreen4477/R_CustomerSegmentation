# ttile: boxplot


agile.tmpData <- inputDt[,which(sapply(inputDt,is.numeric)),with=FALSE]
tryCatch(dev.off(),error=function(e) cat(""))
par(mar=c(7,7,7,7))   
boxplot(scale(agile.tmpData),las = 2,horizontal=TRUE,col=terrain.colors(ncol(agile.tmpData)),cex.axis=0.5)


rm(list=ls(pattern="agile"))
cat("done!")

