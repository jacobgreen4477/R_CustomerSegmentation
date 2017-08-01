# title: plot histogram


# set save folder
agile.saveFolder = "~/È­¼ö¸ñ/output/"


plotHist <- function(){
  
  agile.visualdataDt = data.table(node=somModel$unit.classif,cluster=clusterID,inputDt)
  
  for(i in 3:ncol(agile.visualdataDt)){
    
    # group mean  
    agile.means <- data.frame(agile.visualdataDt[,mean(get(colnames(agile.visualdataDt)[i]),na.rm=TRUE),by=cluster])
    colnames(agile.means)<-c("cluster","means")
    
    # histogram per group
    p <- ggplot(agile.visualdataDt,aes_string(x=names(agile.visualdataDt)[i],fill=names(agile.visualdataDt)[2]))+
      geom_histogram(alpha=.5)+
      facet_grid(cluster~.,margins=TRUE,scales="free")

    # plot 
    print(p)
    ggsave(filename=paste0(agile.saveFolder,"hist_",colnames(agile.visualdataDt)[i],".jpg"), plot=p)
    #readline(prompt = "Press enter to move next")
    dev.off()
    
  }
  
}

plotHist()


rm(list=ls(pattern="agile"))
cat("done!")


