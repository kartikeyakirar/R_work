#Clustering based on Number of results and search volume

mainDir <- getwd()
subDir <- "figs/clustering"
if (!file.exists(subDir)){
       dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
   }
set.seed(1234)
kmean_pie<-function(arg1,arg2,arg3){
PurCl<-kmeans(arg1,arg3,nstart = 25)
lbls<-c(paste0("cluster",1:arg3,"->"))
pct <- round(PurCl$size/sum(PurCl$size)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 

s1<-data.frame("Group"<-lbls,"Values"<-PurCl$size)
pie <- ggplot(s1, aes(x="", y=PurCl$size, fill=Group))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  labs(title="Sample cluster analysis of data \nPie Representation of % entity in each cluster")
ggsave(filename = paste0("figs/clustering/pie_cluster_purina_",arg2,".png"),pie)

}

## K-means analysis
export_km_qual <- sapply(1:20, function(w) {temp <- kmeans(export_num, w); return(temp$betweenss/temp$totss)})
purina_km_qual <- sapply(1:40, function(w) {temp <- kmeans(purina_num, w); return(temp$betweenss/temp$totss)})
whiskas_km_qual <- sapply(1:20, function(w) {temp <- kmeans(whiskas_num, w); return(temp$betweenss/temp$totss)})

temp<-data.frame("Index"<-1:20,"Values"<-export_km_qual)
t<-qplot(Index,Values,data=temp )+geom_line()+geom_vline(xintercept = 6,color="Red")+labs(title="Determining optimal number of cluster for k-means",x="Number of cluster",y=expression(betweenss/totss))
ggsave(filename = "figs/clustering/determining_k_topcatfood.png",t)
temp<-data.frame("Index"<-1:40,"Values"<-purina_km_qual)
t<-qplot(Index,Values,data=temp )+geom_line()+geom_vline(xintercept = 20,color="Red")+labs(title="Determining optimal number of cluster for k-means",x="Number of cluster",y=expression(betweenss/totss))
ggsave(filename = "figs/clustering/determining_k_purina.png",t)
temp<-data.frame("Index"<-1:20,"Values"<-whiskas_km_qual)
t<-qplot(Index,Values,data=temp )+geom_line()+geom_vline(xintercept = 14,color="Red")+labs(title="Determining optimal number of cluster for k-means",x="Number of cluster",y=expression(betweenss/totss))
ggsave(filename = "figs/clustering/determining_k_whiskas.png",t)
############
export_km <- kmeans(export_num, 6)## there is 6 groups in export data it can be seen if previous code run
purina_km <- kmeans(purina_num, 20)## and 20 groups in purina data
whiskas_km <- kmeans(whiskas_num, 14)## and 14 groups in whiskas data 
library(ggfortify)
f1<-autoplot(clara(export_num,6),data=export_km,label = TRUE, frame = TRUE,main="k cluster analysis in Top cat Food search \n PC-> principal component")+labs(x="principal component 1 after dimensionality reduction",y="principal component 2")
f2<-autoplot(kmeans(export_num,6,nstart = 25),data=export_num,label = TRUE, frame = TRUE,main="k-means analysis in Whiskas.com \n PC-> principal component")+labs(x="principal component 1 after dimensionality reduction",y="principal component 2")
ggsave(filename = "figs/clustering/kclust_catfood.png",f1)
ggsave(filename = "figs/clustering/kmean_catfood.png",f2)
f1<-autoplot(clara(purina_num,20),data=purina_num,label = TRUE, frame = TRUE,main="k cluster analysis in purina.com \n PC-> principal component")+labs(x="principal component 1 after dimensionality reduction",y="principal component 2")
f2<-autoplot(kmeans(purina_num,20,nstart = 25),data=purina_num,label = TRUE, frame = TRUE,main="k-means analysis in Whiskas.com \n PC-> principal component")+labs(x="principal component 1 after dimensionality reduction",y="principal component 2")
ggsave(filename = "figs/clustering/kclust_purina.png",f1)
ggsave(filename = "figs/clustering/kmean_purina.png",f2)
f1<-autoplot(clara(whiskas_num,14),data=whiskas_num,label = TRUE, frame = TRUE,main="k cluster analysis in Whiskas.com \n PC-> principal component")+labs(x="principal component 1 after dimensionality reduction",y="principal component 2")
f2<-autoplot(kmeans(whiskas_num,14,nstart = 25),data=whiskas_num,label = TRUE, frame = TRUE,main="k-means analysis in Whiskas.com \n PC-> principal component")+labs(x="principal component 1 after dimensionality reduction",y="principal component 2")
ggsave(filename = "figs/clustering/kclust_whiskas.png",f1)
ggsave(filename = "figs/clustering/kmean_whiskas.png",f2)
