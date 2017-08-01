# title: second cluster


# set save folder
agile.saveFolder = "~/화수목/output/"

# second cluster
set.seed(1234)
clusterNode      <- kmeans(somModel$codes,agile.clusterGroup,nstart = 30,iter.max = 300)$cluster

# node, cluster정보 저장
clusterID        <- clusterNode[somModel$unit.classif]
somNodeClusterDt <- data.table(node=somModel$unit.classif,cluster=clusterID)
somNodeClusterDt <- unique(somNodeClusterDt)

# rm(list=ls(pattern="agile"))
# cat("done!")




