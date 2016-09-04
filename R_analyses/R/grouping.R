#
mainDir <- getwd()
subDir <- "figs/dataplot"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}

subDir <- "figs/dataplot/purina"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}

subDir <- "figs/dataplot/whiskas"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}


plot_time_volume<-function(dat){
  ggplot(dat,aes(Timestamp,Search.Volume))+
    ylim(0,3000)+
    geom_point(aes(color=grouping),alpha=0.5)+
    geom_text(aes(label=factor(dat$Keyword),hjust=-0.1),angle=45,check_overlap = T)+
    facet_wrap(~grouping,nrow=2)+geom_smooth()+
    scale_x_date(date_breaks = "1 week", date_labels = "%m")+
  labs(title="Average search for cat Food category keyword and Other",y="Average number of times user serched the keyword",x="Month in which searched is performed")
    
}

##plot_time_volume(purina),plot_time_volume(whiskas)

plot_traffic_cost<-function(dat){
ggplot(dat,aes(Traffic....,Traffic.Cost....))+
    geom_point(aes(color=grouping))+
    geom_text(aes(label=dat$Keyword,hjust=-0.1),angle=0,check_overlap = T)+
    geom_smooth()+
    facet_wrap(~grouping)+
    labs(title="Relationship between traffic driven to website and cost paid for",x="% Share of Traffic for given keyword",y="% share paid to Google AdWords")

  }

##plot_traffic_cost(purina),plot_traffic_cost(wiskas)


plot_traffic_volume<-function(dat){

  ggplot(dat,aes(Traffic....,Search.Volume))+
    geom_point(aes(color=grouping))+
    facet_wrap(~grouping)+
    geom_smooth( col="steelblue")+
  geom_text(aes(label=dat$Keyword,hjust=-0.1),angle=0,check_overlap = T)+
    labs(title="Relationship between traffic driven to website and average volume searched",x="% Share of Traffic for given keyword",y="% Average volume searched")
  
  }

##plot_traffic_volume(purina)...

plot_competition_cpc<-function(dat){
  
t<-ggplot(dat,aes(Competition,CPC))+
    geom_point(aes(color=grouping))+
    facet_wrap(~grouping)+
  geom_smooth( col="steelblue")+
    geom_text(aes(label=dat$Keyword,hjust=-0.1),angle=45,check_overlap = T)+
    labs(title="Relationship between earning CPC and competitive density of keywords ",x="% Competitive density of advertiser",y="CPC earned")
  

}
#************************
#choose the top searching Keywords

top_under_keyword<-function(arg1,arg2){
temp<-subset(arg1,arg1$Search.Volume>=arg2)
ggplot(temp,aes(x=temp$Position,y=temp$Search.Volume))+
  geom_point(color="green")+
  geom_text(aes(label=temp$Keyword,hjust=-0.1),angle=60,check_overlap = T)+
  geom_smooth()+
  labs(title="Relationship between Position of Domain and  average searched volume of keyword ",x="Position of Domain in paid search",y="average volume searched")
}


#Grouping check data set sample top keyaword
d<-dist(data[1:100,],method="euclidean")
fit<-hclust(d,method="average")
#plot(fit,main="Average Linkage",col.main="red",xlab="observations",ylab="height") #Two aobservations seems to differ
c<-ggdendrogram(fit)+ labs(title="Average Linkage by hierarchical clustering in Purina ",x="showing correlated data average linkages",y="")

ggsave(filename = "figs/dataplot/sample_hclust_purina.png",c)

d<-dist(data1[1:100,],method="euclidean")
fit<-hclust(d,method="average")
#plot(fit,main="Average Linkage",col.main="red",xlab="observations",ylab="height") #Two aobservations seems to differ
c<-ggdendrogram(fit)+
  labs(title="Average Linkage by hierarchical clustering in Whiskas",x="showing correlated data average linkages",y="")

ggsave(filename = "figs/dataplot/sample_hclust_whiskas.png",c)

####
png(filename = "figs/dataplot/purina/multivariate_1.png",width = 1200,height =1000)
tableplot(data,sortCol = 1,title="Visualization multivariate datasets pattern with other variabels \n Each columns is plotted according to Position ")
dev.off()
png(filename = "figs/dataplot/purina/multivariate_2.png",width = 1200,height =1000)
tableplot(data,sortCol = 2,title="Visualization multivariate datasets with several variabels \n columns sorted according to Previous Position ")
dev.off()
png(filename = "figs/dataplot/purina/multivariate_3.png",width = 1200,height =1000)
tableplot(data,sortCol = 3,title="Visualization multivariate datasets with several variabels \n columns sorted according to Search volume ")
dev.off()
png(filename = "figs/dataplot/purina/multivariate_4.png",width = 1200,height =1000)
tableplot(data,sortCol = 4,title="Visualization multivariate datasets with several variabels \n columns sorted according to Keyword Difficulty")
dev.off()
png(filename = "figs/dataplot/purina/multivariate_5.png",width = 1200,height =1000)
tableplot(data,sortCol = 5,title="Visualization multivariate datasets with several variabels \n columns sorted according to CPC ")
dev.off()
png(filename = "figs/dataplot/purina/multivariate_6.png",width = 1200,height =1000)
tableplot(data,sortCol = 6,title="Visualization multivariate datasets with several variabels \n columns sorted according to Traffic ")
dev.off()
png(filename = "figs/dataplot/purina/multivariate_7.png",width = 1200,height =1000)
tableplot(data,sortCol = 7,title="Visualization multivariate datasets with several variabels \n columns sorted according to Traffic cost ")
dev.off()
png(filename = "figs/dataplot/purina/multivariate_8.png",width = 1200,height =1000)
tableplot(data,sortCol = 8,title="Visualization multivariate datasets with several variabels \n columns sorted according to competition ")
dev.off()
png(filename = "figs/dataplot/purina/multivariate_9.png",width = 1200,height =1000)
tableplot(data,sortCol = 9,title="Visualization multivariate datasets with several variabels \n columns sorted according to number of result")
dev.off()




png(filename = "figs/dataplot/whiskas/multivariate_1.png",width = 1200,height =1000)
tableplot(data1,sortCol = 1,title="Visualization multivariate datasets pattern with other variabels \n Each columns is plotted according to Position ")
dev.off()
png(filename = "figs/dataplot/whiskas/multivariate_2.png",width = 1200,height =1000)
tableplot(data1,sortCol = 2,title="Visualization multivariate datasets with several variabels \n columns sorted according to Previous Position ")
dev.off()
png(filename = "figs/dataplot/whiskas/multivariate_3.png",width = 1200,height =1000)
tableplot(data1,sortCol = 3,title="Visualization multivariate datasets with several variabels \n columns sorted according to Search volume ")
dev.off()
png(filename = "figs/dataplot/whiskas/multivariate_4.png",width = 1200,height =1000)
tableplot(data1,sortCol = 4,title="Visualization multivariate datasets with several variabels \n columns sorted according to Keyword Difficulty")
dev.off()
png(filename = "figs/dataplot/whiskas/multivariate_5.png",width = 1200,height =1000)
tableplot(data1,sortCol = 5,title="Visualization multivariate datasets with several variabels \n columns sorted according to CPC ")
dev.off()
png(filename = "figs/dataplot/whiskas/multivariate_6.png",width = 1200,height =1000)
tableplot(data1,sortCol = 6,title="Visualization multivariate datasets with several variabels \n columns sorted according to Traffic ")
dev.off()
png(filename = "figs/dataplot/whiskas/multivariate_7.png",width = 1200,height =1000)
tableplot(data1,sortCol = 7,title="Visualization multivariate datasets with several variabels \n columns sorted according to Traffic cost ")
dev.off()
png(filename = "figs/dataplot/whiskas/multivariate_8.png",width = 1200,height =1000)
tableplot(data1,sortCol = 8,title="Visualization multivariate datasets with several variabels \n columns sorted according to competition ")
dev.off()
png(filename = "figs/dataplot/whiskas/multivariate_9.png",width = 1200,height =1000)
tableplot(data1,sortCol = 9,title="Visualization multivariate datasets with several variabels \n columns sorted according to number of result")
dev.off()

#top_under_keyword
# pyscho describe(purina)
# ggplot(export,aes(Competitive.Density, CPC..USD.))+geom_point()

#ggplot(export,aes(Competitive.Density, CPC..USD.))+geom_point(aes(color=SERP.Features))+facet_wrap(~SERP.Features,nrow = 6,ncol=7)

