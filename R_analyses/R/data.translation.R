#Data frame translation
purina<-read.csv("data/purina.com-domain_organic-us.csv")
whiskas<-read.csv("data/whiskas.com-domain_organic-us.csv")
export<-read.csv("data/export.csv",na.strings=c(""," ","NA"))
##############################################################
data<-purina[,-c(1,7,12,13)]
data1<-whiskas[,-c(1,7,12,13)]

data$Number.of.Results<-(data$Number.of.Results-mean(data$Number.of.Results))/sd(data$Number.of.Results) #standardize the values in order to have value compability between the variables
data$Search.Volume<-(data$Search.Volume-mean(data$Search.Volume))/sd(data$Search.Volume) #standardize the values in order to have value compability between the variables
data1$Number.of.Results<-(data1$Number.of.Results-mean(data1$Number.of.Results))/sd(data1$Number.of.Results) #standardize the values in order to have value compability between the variables
data1$Search.Volume<-(data1$Search.Volume-mean(data1$Search.Volume))/sd(data1$Search.Volume) #standardize the values in order to have value compability between the variables

#############################################################

## data prepocessing for different analysis
export_num <- scale(export[ ,-c(1, 6)], center = TRUE, scale = TRUE)
export_num_mean <- attr(export_num, "scaled:center")
export_num_sd <- attr(export_num, "scaled:scale")
purina_num <- scale(purina[ ,-c(1, 7, 12, 13)], center = TRUE, scale = TRUE)
purina_num_mean <- attr(purina_num, "scaled:center")
purina_num_sd <- attr(purina_num, "scaled:scale")
whiskas_num <- scale(whiskas[ ,-c(1, 7, 12, 13)], center = TRUE, scale = TRUE)
whiskas_num_mean <- attr(whiskas_num, "scaled:center")
whiskas_num_sd <- attr(whiskas_num, "scaled:scale")
purina_URL <- purina$Url[1]
whiskas_URL <- whiskas$Url[1]
whiskas$Page <- gsub(whiskas_URL, "", whiskas$Url)## extracting Url subpage information
whiskas$Page[whiskas$Page == ""] <- "/Main"
whiskas$Page <- unname(sapply(whiskas$Page, function(w) {temp <- strsplit(w, "/")[[1]]; temp[length(temp)]}))
################################################################
pXe<-intersect(export$Keyword,purina$Keyword)# all purina data which is in export all duplicated removed
wXe<-intersect(export$Keyword,whiskas$Keyword)# all whiskas data which is in export export all duplicated removed
pXw<-intersect(whiskas$Keyword,purina$Keyword)# all purina data and whiskas common data export all duplicated removed

#Timestam conversion to dd-mm-yyyy formate
purina$Timestamp<-as.Date(as.POSIXct(as.numeric(purina$Timestamp),origin="1970-01-01",tz="GMT"))
whiskas$Timestamp<-as.Date(as.POSIXct(as.numeric(whiskas$Timestamp),origin="1970-01-01",tz="GMT"))

pXe_withDuplicated<-purina$Keyword[purina$Keyword %in% export$Keyword]
other_pXe<-purina$Keyword[!purina$Keyword %in% export$Keyword]

wXe_withDuplicated<-whiskas$Keyword[whiskas$Keyword %in% export$Keyword]
other_wXe<-whiskas$Keyword[!whiskas$Keyword %in% export$Keyword]

nothing_in_pw<-setdiff(setdiff(export$Keyword,purina$Keyword),whiskas$Keyword)
common_pXw<-intersect(pXw,export$Keyword)
common_pXw_not_in_export<-setdiff(pXw,export$Keyword)
excl_pXe<-setdiff(pXe,pXw)
excl_wXe<-setdiff(wXe,pXw)
Other_in_purina<-setdiff(purina$Keyword,export$Keyword)

purina$grouping<-ifelse(purina$Keyword %in% export$Keyword,"Cat Food","Other Food")
purina$grouping<-factor(purina$grouping,levels=c("Other Food","Cat Food"))

whiskas$grouping<-ifelse(whiskas$Keyword %in% export$Keyword,"Cat Food","Other Food")
whiskas$grouping<-factor(whiskas$grouping,levels=c("Other Food","Cat Food"))

subsetpurina<-subset(purina,select=-c(Keyword,Url,Trends,grouping))
subsetpurina$Timestamp<-as.numeric(subsetpurina$Timestamp)
subsetwhiskas<-subset(whiskas,select=-c(Keyword,Url,Trends,grouping))
subsetwhiskas$Timestamp<-as.numeric(subsetwhiskas$Timestamp)


