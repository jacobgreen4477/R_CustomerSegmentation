# title: pie chart 


# data
tmpData <- data.table(table(clusterID))
tmpData[,proportion:=round(N/sum(tmpData$N),2)]
tmpData[,cluster:=paste0("Cluster",clusterID,": ",proportion*100,"%")]


# pie chart
p <- pie3D(tmpData$proportion,labels=tmpData$cluster,explode=0.2,labelcex=1,shade=0.3,main = "Cluster Proportion")
print(p)
