# title: customer segmentation 
rm(list=ls())
dev.off()
wd = "C:/users/jacob/Documents/화수목/코드종합/Shopping" 
source(file.path(wd,"0_common_v0.01.R"))


# _________________________
# 1단계: 파생변수 생성 
# _________________________
# run
source(file.path(wd,"1_Shopping_v0.05.R"))
# output
inputDt
# _________________________


# _________________________
# 2단계: 데이터 탐색
# _________________________
# run
# source(file.path(wd,"2_ExploreDataAnalysis_v0.01.R"))
# source(file.path(wd,"2_PlotBox_v0.01.R"))
# source(file.path(wd,"2_PlotHist_v0.02.R"))
customerID <- inputDt[,list(CustomerID)]
inputDt[,CustomerID:=NULL]
inputDt
# output
colnames(inputDt)
clusterVarLeft <- c("MeanPriceOfItem"                   # item
                    ,"SumQuantityOfItem"                # item
                    ,"MeanNumberOfItemInBasket"         # basket
                    ,"MeanSpendOfBasket"                # basket
                    ,"MeanPercentOfSalesitemInBasket"   # sales
                    ,"MeanPercentOfPremiumitemInBasket" # premium 
                    ,"SumUniqueDate")                   # visit(number of baskets)
# _________________________


# _________________________
# 4단계: 상관계수 처리  
# _________________________
agile.corr=0.9
source(file.path(wd,"4_HighCorrRemoval_v0.02.R"))
# output
highCorrTable
M <- cor(inputDt[,clusterVarLeft,with=F])
corrplot(M, method="color",diag=F,addCoef.col = "black",mar = c(1,1,1,1), tl.cex=0.5)
# _________________________


# _________________________
# 3단계: 이상값 처리 
# _________________________
# run
# source(file.path(wd,"3_SkewnessTest_v0.01.R"))
# source(file.path(wd,"3_RuleBasedRemoval_v0.03.R"))
# source(file.path(wd,"3_CapFunction_v0.01.R"))
inputDt$MeanPriceOfItem           <- CapVectorBoth(inputDt$MeanPriceOfItem)
inputDt$SumQuantityOfItem         <- CapVectorBoth(inputDt$SumQuantityOfItem)
inputDt$MeanNumberOfItemInBasket  <- CapVectorBoth(inputDt$MeanNumberOfItemInBasket)
inputDt$MeanSpendOfBasket         <- CapVectorBoth(inputDt$MeanSpendOfBasket)
inputDt$SumUniqueDate             <- CapVectorBoth(inputDt$SumUniqueDate)
source(file.path(wd,"3_PlotHist_v0.01.R"))
# output
inputDt
# _________________________


# _________________________
# 5단계: 2차군집 개수 선택 
# _________________________
agile.seed  <- 1234
agile.epoch <- 100
agile.xdim  <- 10
agile.ydim  <- 10
source(file.path(wd,"5_ClusterNumSelect_v0.04.R")) 
# make a decision 
clusterGroup <- 8
# _________________________


# _________________________
# 6단계: 맵사이즈 선택
# _________________________
# option
agile.clusterGroup <- clusterGroup
agile.epoch        <- 100
agile.seed         <- 1234
agile.minMapSize   <- 10
agile.maxMapSize   <- 35
# run
source(file.path(wd,"6_ClusterMapSize_v0.05.R"))
# output
qntErr
write.table(qntErr, "clipboard", sep="\t", row.names=FALSE)
# _________________________


# _________________________
# 8단계: som 학습 
# _________________________
# scale
source(file.path(wd,"8_ClusterScale_v0.03.R"))

p <- par(mfrow=c(4,5))
somTrainRst <- list()
mytry = c(2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,500,1000)
for(i in mytry){
# somoclu 
agile.seed  <- 1234
agile.epoch <- i
agile.xdim  <- 10
agile.ydim  <- 10
source(file.path(wd,"8_ClusterSomoclu.R"))

# second 
# agile.clusterGroup <- clusterGroup
# source(file.path(wd,"8_ClusterSomH_v0.02.R")) 
agile.clusterGroup <- clusterGroup
source(file.path(wd,"8_ClusterSomK_v0.03.R"))

# summary
plot(somModel,type = "codes",bgcol = nicePalette[clusterNode],main=paste0("epoch_",i))
add.cluster.boundaries(somModel, clusterNode)
somTrainRst[[i]] <- data.frame(epoch=agile.epoch,quantizationError=mean(somModel$distances))
cat(agile.epoch,":",mean(somModel$distances),"\n")
}

somTrainRst <- rbindlist(somTrainRst)
write.table(somTrainRst, "clipboard", sep="\t", row.names=FALSE)
# _________________________



# _________________________
# 9단계: 시각화 
# _________________________
# summary 
summaryDt <- data.table(cluster=clusterID,node=somModel$unit.classif,inputDt)
summaryDt <- summaryDt[,list(SumProfit=round(mean(SumProfit))
                             ,MeanProfitOfBasket=round(mean(MeanProfitOfBasket))
                             ,SumSpend=round(mean(SumSpend))
                             ,count=length(node)
                             ,percent=round(length(node)/length(clusterID),2)),by=cluster]
summaryDt <- summaryDt[order(cluster),]
write.table(summaryDt, "clipboard", sep="\t", row.names=FALSE)
summaryDt
par(mfrow=c(1,2))
plot(somModel,"codes",main="codes plot")
plot(somModel,"mapping",labels=as.integer(clusterID),col = as.integer(clusterID))
#plot(somModel,"counts",main="counts plot")
#plot(somModel,"dist.neighbours")
#plot(somModel,"quality")


# run
colnames(inputDt)
tryCatch(dev.off(),error=function(e) cat(""))
par(mfrow=c(4,7))
source(file.path(wd,"9_PlotHeatMap_v0.01.R")) 
source(file.path(wd,"9_PlotHistogram_v0.02.R")) 
source(file.path(wd,"9_PlotAvgRank_v0.02.R")) 
source(file.path(wd,"9_PlotSpiderChart_v0.01.R")) 
source(file.path(wd,"9_PlotPieChart_v0.01.R"))
source(file.path(wd,"9_PlotBivariateNode_v0.01.R"))
source(file.path(wd,"9_PlotBivariateRawdata_v0.01.R"))
# _________________________


# _________________________
# 10단계: 신규고객 매핑(*)
# _________________________
# run
source(file.path(wd,"10_FeNewdata_v0.01.R")) 
testDt
source(file.path(wd,"10_MapNewdata_v0.04.R")) 
outputDt
# _________________________




