# title:  historgram 


agile.tmpData <- inputDt[,which(sapply(inputDt,is.numeric)),with=FALSE]
agile.tmpData[,tmpId:=1:nrow(agile.tmpData)]
agile.tmpData <- agile.tmpData[,lapply(.SD,as.numeric)]

tryCatch(dev.off(),error=function(e) cat(""))
agile.tmpMeltData <- melt(agile.tmpData,id.vars = "tmpId")
p <- ggplot(agile.tmpMeltData, aes(value,fill=variable)) + 
  facet_wrap(~variable, scales = 'free') +
  geom_histogram()
print(p)

agile.tmpData[,tmpId:=NULL]

rm(list=ls(pattern="agile"))
cat("done!")

