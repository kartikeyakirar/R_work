# analyses script

source("R/install.packages.R")
source("R/data.translation.R")
source("R/piechart.R")
source("R/grouping.R")
source("R/correlation.R")
source("R/bayesian.R")
source("R/classification.R")
source("R/clustering.R")
source("R/prediction.R")
source("R/timeseries.R")
source("R/trends.R")

#saving credentials for plotly
Sys.setenv("plotly_username"="asherswing")
Sys.setenv("plotly_api_key"="jz8mnst8vm")

###################################################################################
# saving data contribution in form of pie chart
png(height=1200, width=1200, pointsize=25, file="figs/pie/pie_for_eachdomain.png")
plot_pie_3d_eachDomain(nothing_in_pw,common_pXw,excl_pXe,excl_wXe)
dev.off()

png(height=1200, width=1200, pointsize=25, file="figs/pie/catFood_searched_eachdomain.png")
plot_pie_catfood(pXe_withDuplicated,other_pXe,wXe_withDuplicated,other_wXe)
dev.off()


#temp<-compare_position(purina,whiskas)
#htmlwidgets::saveWidget(as.widget(temp),"position_distribution_comparision.html")

#temp<-html_pie_3d_eachDomain(nothing_in_pw,common_pXw,excl_pXe,excl_wXe)
#htmlwidgets::saveWidget(as.widget(temp),"pie_for_eachdomain.html")

#temp<-html_pie_catfood(pXe_withDuplicated,other_pXe,wXe_withDuplicated,other_wXe)
#htmlwidgets::saveWidget(as.widget(temp),"catFood_searched_eachdomain.html")

################################################################################

temp<-plot_time_volume(purina)
ggsave(file="figs/dataplot/purina/time_vs_volume_purina.png",temp)
temp<-plot_time_volume(whiskas)
ggsave(file="figs/dataplot/whiskas/time_vs_volume_whiskas.png",temp)
temp<-plot_traffic_cost(purina)
ggsave(file="figs/dataplot/purina/traffic_vs_cost_purina.png",temp)
temp<-plot_traffic_cost(whiskas)
ggsave(file="figs/dataplot/whiskas/traffic_vs_cost_whiskas.png",temp)
temp<-plot_traffic_volume(purina)
ggsave(file="figs/dataplot/purina/traffic_vs_volume_purina.png",temp)
temp<-plot_traffic_volume(whiskas)
ggsave(file="figs/dataplot/whiskas/traffic_vs_volume_whiskas.png",temp)
temp<-plot_competition_cpc(whiskas)
ggsave(file="figs/dataplot/whiskas/compe_vs_cpc_whiskas.png",temp)
temp<-plot_competition_cpc(purina)
ggsave(file="figs/dataplot/purina/compe_vs_cpc_purina.png",temp)
temp<-top_under_keyword(purina,10000)
ggsave(file="figs/dataplot/purina/search_vsvolume_purina.png",temp)
temp<-top_under_keyword(whiskas,10000)
ggsave(file="figs/dataplot/whiskas/search_vsvolume_whiskas.png",temp)


df<-export
df$SERP.Features<-as.character(df$SERP.Features)
df$SERP.Features[is.na(df$SERP.Features)]<-"unknown"
s <- strsplit(df$SERP.Features, split =", ")
dat.fram<-data.frame("Keyword" = rep(df$Keyword, sapply(s, length)),"Volume" = rep(df$Volume, sapply(s, length)),"Keyword.Difficulty" = rep(df$Keyword.Difficulty, sapply(s, length)),"CPC..USD." = rep(df$CPC..USD., sapply(s, length)), "Competitive.Density"= rep(df$Competitive.Density, sapply(s, length)), "SERP.Features"= unlist(s))

temp<-ggplot(export,aes(Competitive.Density, CPC..USD.))+
geom_point(aes(color=SERP.Features))+geom_smooth()+
facet_wrap(~SERP.Features,nrow = 6,ncol=7)+
labs(title="Relationship between earning CPC and competitive density of keywords categorised by multiple SERP ")
ggsave(file="figs/dataplot/CPC_vs_CompetitiveDensity_grouped_SERP.png",temp,width = 20, height = 14)

temp<-ggplot(dat.fram,aes(Competitive.Density, CPC..USD.))+
geom_point(aes(color=SERP.Features))+
facet_wrap(~SERP.Features,nrow = 6,ncol=7)+
labs(title="Relationship between earning CPC and competitive density of keywords categorised by  individual SERP" ,x="% Competitive density of advertiser",y="CPC earned")
ggsave(file="figs/dataplot/CPC_vs_CompetitiveDensity_single.png",temp)



##################################
export1<-export
export1$Keyword<-as.character(export1$Keyword)
purina1<-purina
purina1$Keyword<-as.character(purina1$Keyword)
whiskas1<-whiskas
whiskas1$Keyword<-as.character(whiskas1$Keyword)
##################################
mergeset1<-merge(export1,whiskas1,by.x="Keyword",by.y="Keyword")
mergeset1$SERP.Features<-as.character(mergeset1$SERP.Features)
mergeset_t1<-subset(mergeset1,select = -c(Keyword,Trends,Url,Timestamp))
mergeset_t1$SERP.Features[is.na(mergeset_t1$SERP.Features)]<-"unknown"
s <- strsplit(mergeset_t1$SERP.Features, split =", ")
dat.fram<-data.frame("Traffic" = rep(mergeset_t1$Traffic...., sapply(s, length)),"TCost" = rep(mergeset_t1$Traffic.Cost...., sapply(s, length)),"Keyword.Difficulty" = rep(mergeset_t1$Keyword.Difficulty, sapply(s, length)),"CPC..USD." = rep(mergeset_t1$CPC..USD., sapply(s, length)), "Competitive.Density"= rep(mergeset_t1$Competitive.Density, sapply(s, length)), "SERP.Features"= unlist(s))

temp<-ggplot(dat.fram,aes(Traffic,TCost))+
geom_point(aes(color=SERP.Features))+ 
geom_smooth(method="lm", se=FALSE, col="steelblue")+
facet_wrap(~SERP.Features)+
labs(title="Relationship between traffic driven to website and cost paid for keywords in \nwhiskas doamin for cat food category keywords",x="% Share of Traffic for given keyword",y="% share paid to Google AdWords")
ggsave(file="figs/dataplot/whiskas/Traffic_Vs_trafficCost_whiskas_catfood.png",temp)


temp<-ggplot(dat.fram,aes(Competitive.Density,CPC..USD.))+
geom_point(aes(color=SERP.Features))+ 
geom_smooth(method="lm", se=FALSE, col="steelblue")+
facet_wrap(~SERP.Features)+
labs(title="Relationship between earning CPC and competitive density of keywords in \n whiskas domain for cat food category keywors",x="% Competitive density of advertiser",y="CPC earned")
ggsave(file="figs/dataplot/whiskas/CPC_Vs_CompetitiveDesnsity_whiskas_catfood.png",temp)



###################################

mergeset2<-merge(export1,purina1,by.x="Keyword",by.y="Keyword")
mergeset2$SERP.Features<-as.character(mergeset2$SERP.Features)
mergeset_t2<-subset(mergeset2,select = -c(Keyword,Trends,Url,Timestamp))
mergeset_t2$SERP.Features[is.na(mergeset_t2$SERP.Features)]<-"unknown"
s <- strsplit(mergeset_t2$SERP.Features, split =", ")
dat.fram<-data.frame("Traffic" = rep(mergeset_t2$Traffic...., sapply(s, length)),"TCost" = rep(mergeset_t2$Traffic.Cost...., sapply(s, length)),"Keyword.Difficulty" = rep(mergeset_t2$Keyword.Difficulty, sapply(s, length)),"CPC..USD." = rep(mergeset_t2$CPC..USD., sapply(s, length)), "Competitive.Density"= rep(mergeset_t2$Competitive.Density, sapply(s, length)), "SERP.Features"= unlist(s))

temp<-ggplot(dat.fram,aes(Traffic,TCost))+
 geom_smooth(method="lm", se=FALSE, col="steelblue")+
 geom_point(aes(color=SERP.Features))+
 facet_wrap(~SERP.Features)+
 labs(title="Relationship between traffic driven to website and cost paid for keywords in \nPurina doamin for cat food category keywords",x="% Share of Traffic for given keyword",y="% share paid to Google AdWords")
ggsave(file="figs/dataplot/purina/Traffic_Vs_trafficCost_purina_catfood.png",temp)


temp<-ggplot(dat.fram,aes(Competitive.Density,CPC..USD.))+
 geom_smooth(method="lm", se=FALSE, col="steelblue")+
 geom_point(aes(color=SERP.Features))+
 facet_wrap(~SERP.Features)+
labs(title="Relationship between earning CPC and competitive density of keywords in \n Purina domain for cat food category keywors",x="% Competitive density of advertiser",y="CPC earned")
ggsave(file="figs/dataplot/purina/CPC_Vs_CompetitiveDesnsity_purina_catfood.png",temp)

#####################################

mergeset1<-mergeset1[,-19]
mergeset<-rbind(mergeset2,mergeset1)
mergeset$SERP.Features<-as.character(mergeset$SERP.Features)
mergeset_t<-subset(mergeset,select = -c(Keyword,Trends,Url))
mergeset_t$SERP.Features[is.na(mergeset_t$SERP.Features)]<-"unknown"
s <- strsplit(mergeset_t$SERP.Features, split =", ")
dat.fram<-data.frame("Traffic" = rep(mergeset_t$Traffic...., sapply(s, length)),"TCost" = rep(mergeset_t$Traffic.Cost...., sapply(s, length)),"Keyword.Difficulty" = rep(mergeset_t$Keyword.Difficulty, sapply(s, length)),"CPC..USD." = rep(mergeset_t$CPC..USD., sapply(s, length)), "Competitive.Density"= rep(mergeset_t$Competitive.Density, sapply(s, length)), "SERP.Features"= unlist(s))

temp<-ggplot(dat.fram,aes(Traffic,TCost))+
geom_point(aes(color=SERP.Features))+
facet_wrap(~SERP.Features)+ 
geom_smooth(method="lm", se=FALSE, col="steelblue")+
 labs(title="Relationship between traffic driven to website and cost paid for keywords in \nPurina and whiskas domains for cat food category keywords",x="% Share of Traffic for given keyword",y="% share paid to Google AdWords")
ggsave(file="figs/dataplot/Traffic_Vs_trafficCost_purina_whiskas_catfood.png",temp)

temp<-ggplot(dat.fram,aes(Competitive.Density,CPC..USD.))+
geom_point(aes(color=SERP.Features))+
facet_wrap(~SERP.Features)+ 
geom_smooth(method="lm", se=FALSE, col="steelblue")+
labs(title="Relationship between earning CPC and competitive density of keywords in \n Purina and Whiskas domains for cat food category keywors",x="% Competitive density of advertiser",y="CPC earned")
ggsave(file="figs/dataplot/CPC_Vs_CompetitiveDesnsity_purina_whiskas_catfood.png",temp)


###################################################################################
# Correlation matrix
corelationMatrix(subsetpurina,"Purina_Correlation_Matrix")
corelationMatrix(subsetwhiskas,"Whiskas_Correlation_Matrix")

###################################################################################
#Descriptive Statistics
out <- capture.output(summary(export))
out1 <- capture.output(describe(export))
cat("Top cat Food dataset summary", out, file="output/export.txt", sep="\n", append=TRUE)
cat("Top cat Food dataset description", out1, file="output/export.txt", sep="\n", append=TRUE)
out <- capture.output(summary(purina))
out1 <- capture.output(describe(purina))
cat("Purina.com dataset summary", out, file="output/export.txt", sep="\n", append=TRUE)
cat("Purina.com dataset description", out1, file="output/export.txt", sep="\n", append=TRUE)
out <- capture.output(summary(whiskas))
out1 <- capture.output(describe(whiskas))
cat("whiskas.com dataset summary", out, file="output/export.txt", sep="\n", append=TRUE)
cat("whiskas.com dataset description", out1, file="output/export.txt", sep="\n", append=TRUE)

#######################################################################################
#classification

plot_rtree(purina,"purina")
plot_rtree(whiskas,"whiskas")

###################################################################################
#clustering
kmean_pie(data,"purina",4)
kmean_pie(data1,"whiskas",4)
#################################################################################
#timeseries
timeseries_cpc(purina,"purina")
timeseries_traffic(purina,"purina")
timeseries_cpc(whiskas,"whiskas")
timeseries_traffic(whiskas,"whiskas")
####################################################################################
#Trend analysis

plothclust(Domain_trend_analyses(purina,70),"purina")

plothclust(Domain_trend_analyses(whiskas,70),"whiskas")

