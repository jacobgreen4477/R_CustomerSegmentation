# title: grocery shopping segmentation 


# load dataset
inputDt <- read.csv(file.path("C:/users/jacob/Documents/화수목/코드종합/Shopping","Shopping.csv")) 
inputDt <- data.table(inputDt)
inputDt[,profit:=price-asset]
inputDt


# 변수_방문
dateDt <- unique(inputDt[,list(customer_id,date)])
dateDt[,date:=gsub("-","",date)]
dateDt[,YM:=substring(date,1,6)]
inds <- unique(dateDt$YM)
dateDt[, paste0("YM",inds) := lapply(inds, function(x) YM == x)]
tmp <- c("YM200101","YM200102","YM200011","YM200012")
dateDt <- dateDt[,lapply(.SD,function(x) sum(x,na.rm=T)),.SDcols=tmp,by=customer_id]
dateDt[,SumUniqueDate:=rowSums(dateDt[,.SD,.SDcols=tmp])]
dateDt[,SumVisitMonth:=4-rowSums(dateDt[,.SD,.SDcols=tmp]==0)]
dateDt


# 변수_아이템
itemDt <- inputDt[,list(customer_id,quantity,asset,price,profit)]
itemDt <- itemDt[,list(MeanPriceOfItem=mean(price)
                         ,MeanAssetOfItem=mean(asset)
                         ,SumQuantityOfItem=sum(quantity)
                         ,SumProfit=sum(quantity*profit)
                         ,SumSpend=sum(quantity*price))
                   ,by=customer_id]
itemDt


# 변수_장바구니
basketDt <- inputDt[,list(customer_id,date,quantity,asset,price,profit)]
basketDt[,salesitem:=ifelse(profit<=0,1,0)]
basketDt[,premiumitem:=ifelse(price>=quantile(price,0.75),1,0)]

basketDt <- basketDt[,list(NumberOfItemInBasket=sum(quantity)
                         ,SumSalesitemQuantityInBasket=sum(quantity*salesitem)
                         ,SumPremiumitemQuantityInBasket=sum(quantity*premiumitem)
                         ,profitOfBasket=sum(quantity*profit)
                         ,spendOfBasket=sum(quantity*price))
                   ,by=list(customer_id,date)]

basketDt[,PercentOfSalesitemInBasket:=SumSalesitemQuantityInBasket/NumberOfItemInBasket]
basketDt[,PercentOfPremiumitemInBasket:=SumPremiumitemQuantityInBasket/NumberOfItemInBasket]

basketDt <- basketDt[,list(MeanNumberOfItemInBasket=mean(NumberOfItemInBasket)
                         ,MeanProfitOfBasket=mean(profitOfBasket)
                         ,MeanSpendOfBasket=mean(spendOfBasket)
                         ,MeanPercentOfSalesitemInBasket=mean(PercentOfSalesitemInBasket)
                         ,MeanPercentOfPremiumitemInBasket=mean(PercentOfPremiumitemInBasket))
                   ,by=list(customer_id)]
basketDt


# 변수_나이  
ageDt <- inputDt[,list(customer_id,age_group)]
sort(as.character(unique(ageDt$age_group)))
ageDt[,age_group:=gsub("A",22,age_group)]
ageDt[,age_group:=gsub("B",27,age_group)]
ageDt[,age_group:=gsub("C",32,age_group)]
ageDt[,age_group:=gsub("D",37,age_group)]
ageDt[,age_group:=gsub("E",42,age_group)]
ageDt[,age_group:=gsub("F",47,age_group)]
ageDt[,age_group:=gsub("G",52,age_group)]
ageDt[,age_group:=gsub("H",57,age_group)]
ageDt[,age_group:=gsub("I",62,age_group)]
ageDt[,age_group:=gsub("J",67,age_group)]
ageDt[,age_group:=gsub("K",NA,age_group)]
ageDt[,age_group:=as.numeric(age_group)]
ageDt <- ageDt[,lapply(.SD,function(x) mean(x,na.rm=T)),.SDcols=c("age_group"),by=customer_id]
ageDt


# 변수_주소 
addressDt <- inputDt[,list(customer_id,address)]
sort(as.character(unique(addressDt$address)))
addressDt[,address:=gsub("E",1,address)]
addressDt[,address:=gsub("F",2,address)]
addressDt[,address:=gsub("D",3,address)]
addressDt[,address:=gsub("A",4,address)]
addressDt[,address:=gsub("B",5,address)]
addressDt[,address:=gsub("C",6,address)]
addressDt[,address:=gsub("G",NA,address)]
addressDt[,address:=gsub("H",NA,address)]
addressDt[,address:=as.numeric(address)]
addressDt <- addressDt[,lapply(.SD,function(x) mean(x,na.rm=T)),.SDcols=c("address"),by=customer_id]
addressDt


# 변수_상품 
# tmp <- inputDt[,list(price,product_subclass)]
# tmp[,product_subclass:=substring(product_subclass,1,1)]
# tmp[,mean(price),by=product_subclass]
subclassDt <- inputDt[,list(customer_id,product_subclass,quantity)]
subclassDt[,product_subclass:=substring(product_subclass,1,1)]
inds <- unique(subclassDt$product_subclass)
subclassDt[, paste0("subclass",inds) := lapply(inds, function(x) product_subclass == x)]
tmp <- c("subclass1","subclass5","subclass3","subclass7","subclass4")
subclassDt <- subclassDt[,lapply(.SD,function(x) sum(x*quantity)),.SDcols=tmp,by=customer_id]
subclassDt[,SumQuantityOfItem:=rowSums(subclassDt[,-1])]
subclassDt[,PercentSubclass1Item:=subclass1/SumQuantityOfItem]
subclassDt[,PercentSubclass5Item:=subclass5/SumQuantityOfItem]
subclassDt[,PercentSubclass3Item:=subclass3/SumQuantityOfItem]
subclassDt[,PercentSubclass7Item:=subclass7/SumQuantityOfItem]
subclassDt[,PercentSubclass4Item:=subclass4/SumQuantityOfItem]
subclassDt[,SumQuantityOfItem:=NULL]
subclassDt



# 종합
setkey(dateDt,customer_id)
setkey(itemDt,customer_id)
setkey(basketDt,customer_id)
setkey(ageDt,customer_id)
setkey(addressDt,customer_id)
setkey(subclassDt,customer_id)
inputDt <- subclassDt[addressDt[ageDt[basketDt[itemDt[dateDt]]]]]
inputDt <- inputDt[complete.cases(inputDt),]
colnames(inputDt)[1] <- "CustomerID"
inputDt


cat("the number of character variables: ", sum(!sapply(inputDt,is.numeric)))
rm(list=ls(pattern="tmp"))







