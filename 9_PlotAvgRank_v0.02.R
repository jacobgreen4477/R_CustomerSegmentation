# title: plot average ranking 


# list top 
theme_set(theme_bw())
agile.visualDt <- data.table(cluster=clusterID,scale(inputDt))


# rank plot 
agile.avg <- agile.visualDt[,lapply(.SD,mean),by=cluster]
agile.avg <- rbind(agile.avg,data.table(cluster="avg",agile.visualDt[,lapply(.SD,mean)][,-1]))
agile.avg <- melt(agile.avg,id="cluster")


# plot for all clusters
p <- ggplot(agile.avg, aes(x=value, y=variable, label=cluster, fill=cluster, color=cluster)) + 
  geom_point(stat='identity', size=14) +
  geom_text(color="white", size=6) 
print(p)


# plot for each cluster
for(i in sort(unique(agile.avg$cluster))){
agile.avg<-agile.avg[,tag:=ifelse(cluster==i,10,0)]
p <- ggplot(agile.avg, aes(x=value, y=variable, label=cluster, fill=cluster, color=cluster)) + 
  geom_point(stat='identity', size=6,alpha = 0.7) +
  geom_point(aes(size=tag), show.legend = F) +
  geom_text(color="white", size=5) 
print(p)
readline(prompt = "please enter to move next")
}


rm(list=ls(pattern="agile"))
cat("done!")

