# title: plot heatmap 


PlotHeatmap <- function(){
  
  for(i in 1:ncol(inputDt)){
    
    agile.colorVar                <- inputDt[,i,with=F]
    agile.colorVarUnscaled        <- aggregate(agile.colorVar, by=list(somModel$unit.classif), FUN=mean, simplify=TRUE)
    names(agile.colorVarUnscaled) <- c("Node","Value")
    agile.missingNodes            <- which(!(seq(1,nrow(somModel$codes)) %in% agile.colorVarUnscaled$Node))
    
    if(length(agile.missingNodes)>0){
      agile.colorVarUnscaled      <- rbind(agile.colorVarUnscaled,data.frame(Node=agile.missingNodes,Value=NA))
      agile.colorVarUnscaled      <- agile.colorVarUnscaled[order(agile.colorVarUnscaled$Node),]
    } else {
      agile.colorVarUnscaled      <- agile.colorVarUnscaled[order(agile.colorVarUnscaled$Node),]
    }
    
    # plot heatmap
    plot(somModel,type = "property",property = agile.colorVarUnscaled$Value,main = names(inputDt)[i],palette.name = coolBlueHotRed)
    readline(prompt = "Press enter to move next")
    
  }
}

PlotHeatmap()


rm(list=ls(pattern="agile"))
cat("done!")


