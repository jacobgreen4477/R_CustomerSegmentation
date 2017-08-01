# title spider chart 


# set save folder
agile.saveFolder = "~/È­¼ö¸ñ/output/"


# choose variables 
agile.answer <- readline(prompt="mean spider chart with only cluster variables(=1)? or with all variables(=2)?")
if(agile.answer==1) {
  agile.tmpData  <- data.table(clusterID,inputDt[,mget(clusterVarLeft)]) 
} else if(agile.answer==2) {
  agile.tmpData  <- data.table(clusterID,inputDt)
}


# spider chart 
tryCatch(dev.off(),error=function(e) cat(""))
#par(mfrow=c(2,2))
for(i in 1:length(unique(somNodeClusterDt$cluster))){
  
  # choose cluster
  agile.spiderTable  <- agile.tmpData[clusterID==i,lapply(.SD,mean)]
  agile.spiderTable[,clusterID:=NULL]
  
  # compute range and mean 
  agile.tmpVars  <- colnames(agile.spiderTable)
  agile.tmpRange <- rbind(inputDt[,lapply(.SD,max),.SDcols=agile.tmpVars],inputDt[,lapply(.SD,min),.SDcols=agile.tmpVars])
  agile.spiderTable  <- rbind(agile.tmpRange,agile.spiderTable)
  agile.spiderTable  <- agile.spiderTable[,lapply(.SD,round)]
  
  # Custom the radarChart
  png(file = paste0(agile.saveFolder,paste0("SpiderChartCluster",i,".png")))
  radarchart(agile.spiderTable
             ,axistype=2
             ,pcol="black"
             ,pfcol=nicePalette[i]
             ,plwd=1
             ,cglcol="grey"
             ,cglty=1
             ,axislabcol="grey"
             ,cglwd=0.7
             ,vlcex=0.8
             ,title=paste0("cluster",i))
  dev.off()
  radarchart(agile.spiderTable
             ,axistype=2
             ,pcol="black"
             ,pfcol=nicePalette[i]
             ,plwd=1
             ,cglcol="grey"
             ,cglty=1
             ,axislabcol="grey"
             ,cglwd=0.7
             ,vlcex=0.8
             ,title=paste0("cluster",i))
  readline(prompt = "please enter to move next")
}


rm(list=ls(pattern="agile"))
cat("done!")







