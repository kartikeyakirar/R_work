mainDir <- getwd()
subDir <- "figs/timeseries"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}


#Time Series Analysis for cost per clicks
timeseries_cpc<-function(arg1,arg2){

TimeDesc<-arg1[order(as.Date(arg1$Timestamp,format="%d-%m-%Y")),,drop=FALSE]#Chronological order of the observations
arg1k<-TimeDesc #Time series analysis in the first 100 observations.
CostPerclick<-subset(arg1k,select=c(CPC,Timestamp)) #Select the variables we need

tz<-CostPerclick %>%  mutate(month = format(Timestamp, "%m"))
zz<-ggplot(tz, aes(CPC,Timestamp))+
  geom_violin()+
  geom_point(alpha = 0.2)+
  labs(title=paste0("Times Series analysis of CPC earned buy company-",arg2),x="Time space " ,y="Cost per click earned in USD")
ggsave(filename = paste0("figs/timeseries/timeseries_violin_",arg2,".png"),zz)

tz$month<-as.numeric(tz$month)
tz<-transform(tz, MonthAbb = month.abb[tz$month])
zz<-ggplot(tz, aes(Timestamp,CPC,color=MonthAbb))+
    geom_line()+
  labs(title=paste0("Times Series analysis CPC earned buy company-",arg2),x="Time space " ,y="Cost per click earned in USD")
ggsave(filename = paste0("figs/timeseries/timeseries_bymonth_",arg2,".png"),zz)


#Unit.root<-ur.df(CostPerclick$CPC,type = "trend")
PP.test(CostPerclick$CPC) #stationary series with pvalue=0.01
arima.model<-auto.arima(x = CostPerclick$CPC,D = CostPerclick$Timestamp) #ARIMA model
forec.20<-forecast.Arima(arima.model,h = 600,bootstrap = TRUE)
#forec.20<-forecast(arima.model,h = 20,bootstrap = TRUE)
ps<-autoplot(forec.20,main="Time series analysis with forcasting of CPC ")+
  labs(title=paste0("Times Series analysis of CPC for ",arg2),x="Time space shown by index and shaded region shows forcasting cpc range" ,y="Cost per click")
ggsave(filename = paste0("figs/timeseries/Costperclick_forcast_",arg2,".png"), ps)
}



#Time Series Analysis for traffic cost
timeseries_traffic<-function(arg1,arg2){
#TrafCos<-subset(arg1,select=c(colnames(arg1[9]),colnames(arg1[13])))
TimeDesc<-arg1[order(as.Date(arg1$Timestamp,format="%d-%m-%Y")),,drop=FALSE]#Chronological order of the observations
arg1k<-TimeDesc #Time series analysis in the first 100 observations.
CostPerclick<-subset(arg1k,select=c(colnames(arg1[9]),colnames(arg1[13])))

#Unit Root Test to check stationarity
#Unit.root2<-ur.df(TrafCos$Traffic.Cost....,type="trend")
#z<-zoo(CostPerclick$CPC,CostPerclick$Timestamp,frequency = 1)
zz<-ggplot(CostPerclick,aes(x=Timestamp,y=Traffic.Cost....))+geom_line()+labs(title=paste0("Times Series analysis for ",arg2),x="Time space " ,y="Traffic Cost")
ggsave(filename = paste0("figs/timeseries/timeseries_",arg2,".png"),zz)


ps<-ggplot(CostPerclick, aes(Traffic.Cost....,Timestamp,col="red"))+geom_violin()
ggsave(filename = paste0("figs/timeseries/traffic_violin_",arg2,".png"), ps)
#summary(Unit.root2) #stationary series.pvalue=2.2e-16
#PP.test(TrafCos$Traffic.Cost....) #stationary series pvalue=0.01
arima.model2<-auto.arima(x = CostPerclick$Traffic.Cost....,D = CostPerclick$Timestamp) #ARIMA model
#summary(arima.model2)
forec2.20<-forecast.Arima(arima.model2,h = 40,bootstrap = TRUE)
#forec2.21<-forecast(arima.model2,h = 50,bootstrap = TRUE)
ps<-autoplot(forec2.20,main="Time series analysis for Traffic ")+labs(title=paste0("Times Series analysis for ",arg2),x="Time space shown by index" ,y="Traffic cost")
ggsave(filename = paste0("figs/timeseries/traffic_ts_forcast_",arg2,".png"), ps)
}


#par(mfrow=c(1,2))
#plot(arg1[71:91,9])#we put as many dates we want to predict
#plot(forec2.20) #Comparison of the fitted model to the actual data.
