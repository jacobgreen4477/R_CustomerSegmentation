# title: EDA for numerical variables


agile.answer <- readline(prompt = "what type of data do you want to explore? (num=1|char=2)")
tryCatch({
if(agile.answer==1){
  agile.tmpNumData <- data.frame(round(numSummary(inputDt),2))
  colnames(agile.tmpNumData) <- gsub("X","quantile ",gsub("[.]","%",colnames(agile.tmpNumData)))
  print(datatable(agile.tmpNumData, options = list(pageLength = 50)))
} else if(agile.answer==2){
  agile.tmpCharData <- charSummary(inputDt)
  print(datatable(agile.tmpCharData, options = list(pageLength = 50)))
}},warning =function(w) cat("no character variables!"))


rm(list=ls(pattern="agile"))
#cat("done!")

