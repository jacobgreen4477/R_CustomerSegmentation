# title: apply capvector


for(i in 1:ncol(inputDt)){
  
  tryCatch(dev.off(),error=function(e) cat(""))
  par(mfrow=c(1,2))
  hist(inputDt[,colnames(inputDt)[i],with=FALSE][[1]],main=paste0("before: ",colnames(inputDt)[i]),xlab="",col="red")
  agile.data = CapVectorBoth(inputDt[,colnames(inputDt)[i],with=FALSE][[1]])
  hist(agile.data,main=paste0("after: ",colnames(inputDt)[i]),xlab="",col="green")
  
  agile.answer = readline(prompt="do you want to apply the cap function?(y/n)")
  
  if(agile.answer=="y"){
    inputDt[,colnames(inputDt)[i]:=CapVectorBoth(inputDt[,get(colnames(inputDt)[i])])]
    cat("cap function is applied")
  }
  
}


rm(list=ls(pattern="agile"))
#cat("done!")
















