# title: common 

install.packages("https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.15.tar.gz",repos=NULL, method="libcurl")
install.packages("https://cran.r-project.org/src/contrib/Archive/Rsomoclu/Rsomoclu_1.6.2.1.tar.gz",repos=NULL, method="libcurl")
library(kohonen)
library(Rsomoclu)
library(devtools)
#install_github("ujjwalkarn/xda")
library(xda)
library(data.table)
library(ggplot2)
library(lubridate)
library(data.table)
library(RColorBrewer)
library(Hmisc)
library(psych)
library(DT)
library(moments)
library(caret)
library(quantmod)
library(progress)
library(plotly)
library(shiny)
library(plotrix)
library(fmsb)
library(treemap)
library(cluster)
library(corrplot)


fillNA <- function(tmp){
  tmp <- data.matrix(tmp)
  tmp[is.na(tmp)] <- 0
  tmp <- data.table(tmp) 
  return(tmp)
}


CapVectorBoth  <- function(x, probs = c(0.02,0.98)){
  agile.ranges <- quantile(x, probs=probs, na.rm=T) 
  x[x < agile.ranges[1]] <- agile.ranges[1] 
  x[x > agile.ranges[2]] <- agile.ranges[2] 
  return(x) 
}


coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}


nicePalette <- c(brewer.pal(brewer.pal.info$maxcolors[11],rownames(brewer.pal.info)[11]),brewer.pal(brewer.pal.info$maxcolors[15],rownames(brewer.pal.info)[15]))

