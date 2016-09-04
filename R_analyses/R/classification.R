#Clssification
#sudo apt-get install libgtk2.0-dev if rattle creates prob 
mainDir <- getwd()
subDir <- "figs/classification"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}


plot_rtree<-function(arg1,arg2){
  
arg1$Keyword<-as.factor(arg1$Keyword)
arg1k<-arg1[1:1000,-c(5,6,7,12,13)]
set.seed(1988)
Groups<-sample(2,nrow(arg1k),replace=TRUE,prob=c(0.6,0.4))
g1<-subset(arg1k,Position<=50)
g2<-subset(arg1k,Position>50)
cl<-Position~Search.Volume+Traffic.Cost....+Competition
temp_tree<-rpart(cl,data=g1)
out <- capture.output(summary(temp_tree))
cat("recurssive partition ", out, file="output/summary_of_recurssive.txt", sep="\n", append=TRUE)
png(filename = paste0("figs/classification/Recurssive_",arg2,".png"),width = 1200,height = 1000)
fancyRpartPlot(temp_tree,main = paste0("Recussive tree partision on basis of Search.volume,competition and Traffic cost \n",arg2))
dev.off()
}