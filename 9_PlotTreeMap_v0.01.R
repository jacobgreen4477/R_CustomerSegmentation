# title: plot tree map 


agile.visualDt <- data.table(cluster=clusterID,inputDt)
agile.avg      <- data.frame(agile.visualDt[,lapply(.SD,mean),by=cluster])
agile.avg      <- melt(agile.avg,id="cluster")
agile.avg      <- data.table(agile.avg)
agile.avg[,rank:=rank(-value,ties.method="first"),by=list(variable)]
agile.avg[,value:=NULL]
agile.avg      <- as.data.frame(agile.avg)


# treemap 
p <- treemap(agile.avg, 
             index=c("variable","cluster"),  
             vSize = c("rank"),  
             type="index", 
             vColor="GNI",
             title="ranking variable by clusters",
             fontsize.title = 14)


rm(list=ls(pattern="agile"))
cat("done!")






