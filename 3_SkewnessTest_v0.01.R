# title: skewness test 


agile.numVars       <- which((inputDt[,lapply(.SD,is.numeric)])==1)
agile.tmpData       <- inputDt[,c(agile.numVars),with=FALSE]
agile.skewnessTable <- data.frame(skewness=t(agile.tmpData[,lapply(.SD,moments::skewness)]))
agile.skewnessTable <- data.frame(variables=rownames(agile.skewnessTable),agile.skewnessTable)
rownames(agile.skewnessTable) <- NULL
agile.skewnessTable <- agile.skewnessTable[order(abs(agile.skewnessTable$skewness),decreasing=TRUE),]


cat("
skewness test only for numeric variables: 

If the skewness is between -0.5 and 0.5 the data are fairly symmetrical
If the skewness is between -1 and -0.5 or between 0.5 and 1 the data are moderately skewed
If the skewness is less than -1 or greater than 1 the data are highly skewed

threshold: +-0.5

")
print(agile.skewnessTable[abs(agile.skewnessTable$skewness)>0.5,])


rm(list=ls(pattern="agile"))
#cat("done!")



