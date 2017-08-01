# title: rule-based remover 


tryCatch({
  
  # 결층행 제거 
  agile.agile.highCorrInfoData <- na.omit(inputDt)
  
  
  # 숫자형 변수 선택 
  if(sum(sapply(agile.agile.highCorrInfoData, is.numeric))!=ncol(inputDt)) cat("warn: non-numeric variable(s) will be excluded","\n")
  agile.numeric <- names(which(sapply(agile.agile.highCorrInfoData, is.numeric)))
  agile.agile.highCorrInfoData <- agile.agile.highCorrInfoData[, .SD, .SDcols=agile.numeric]
  agile.agile.highCorrInfoData
  
  
  # 상관계수 산출 
  agile.higCorr <- cor(agile.agile.highCorrInfoData)
  
  
  # 상관관계 정보 추출
  agile.highCorrInfo <- capture.output(caret::findCorrelation(agile.higCorr,cutoff=agile.corr,verbose=T,exact=FALSE))
  agile.highCorrInfo <- agile.highCorrInfo[-length(agile.highCorrInfo)]
  agile.highCorrInfo <- agile.highCorrInfo[-grep("Flagging column",agile.highCorrInfo)]
  agile.highCorrInfo <- gsub("value = .*","",agile.highCorrInfo)
  agile.highCorrInfo <- gsub("[^0-9]"," ",agile.highCorrInfo)
  agile.highCorrInfo <- gsub("\\s+"," ",agile.highCorrInfo)
  agile.highCorrInfo <- gsub("^\\s+|\\s+$", "", agile.highCorrInfo)
  agile.highCorrInfo <- strsplit(agile.highCorrInfo," ")
  agile.highCorrInfo <- data.table(rbindlist(lapply(agile.highCorrInfo,function(x) strsplit(x," "))))
  agile.highCorrInfo <- agile.highCorrInfo[,lapply(.SD, as.numeric), .SDcols=1:2]
  
  
  # 상관관계 테이블 생성 
  highCorrTable <- list()
  for(i in 1:nrow(agile.highCorrInfo)){
    highCorrTable[[i]] <- data.frame(varLeft=colnames(agile.agile.highCorrInfoData)[agile.highCorrInfo$V1[i]]
                                     ,varRight=colnames(agile.agile.highCorrInfoData)[agile.highCorrInfo$V2[i]]
                                     ,highCorr=cor(agile.agile.highCorrInfoData[[agile.highCorrInfo$V1[i]]],agile.agile.highCorrInfoData[[agile.highCorrInfo$V2[i]]]))
  }
  
  highCorrTable <- rbindlist(highCorrTable)
  highCorrTable <- highCorrTable[order(abs(highCorr),decreasing=TRUE),]
  highCorrTable
  
  
  rm(list=ls(pattern="agile"))
  cat("done!")
  
},error=function(e) cat("there are no highly correlated vairables above the cut-off")) 

