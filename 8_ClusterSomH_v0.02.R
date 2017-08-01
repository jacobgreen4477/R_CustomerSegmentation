# title: second cluster


# set save folder
agile.saveFolder = "~/화수목/output/"


# second cluster
agile.distance   <- dist(somModel$codes,method="euclidean") 
clusterNode      <- cutree(hclust(agile.distance,method = "ward.D2"),agile.clusterGroup)
png(file = paste0(agile.saveFolder,"somH.png"))
plot(somModel,type = "codes",bgcol = nicePalette[clusterNode],main="hirarchical")
add.cluster.boundaries(somModel, clusterNode)
dev.off()


# silhouette plot
png(file = paste0(agile.saveFolder,"somHSilhouette.png"))
plot(silhouette(clusterNode,daisy(somModel$codes)^2),col=nicePalette[1:agile.clusterGroup],main="hirarchical")
dev.off()


# node, cluster정보 저장
clusterID        <- clusterNode[somModel$unit.classif]
somNodeClusterDt <- data.table(node=somModel$unit.classif,cluster=clusterID)
somNodeClusterDt <- unique(somNodeClusterDt)


# 저장 
agile.saveFolder = "~/화수목/output/"
agile.saveFile = paste0("SOM_",Sys.Date(),".RData")
#save.image(file.path(agile.saveFolder,agile.saveFile))


# print
plot(somModel,type = "codes",bgcol = nicePalette[clusterNode],main="hirarchical")
add.cluster.boundaries(somModel, clusterNode)


# rm(list=ls(pattern="agile"))
# cat("done!")




