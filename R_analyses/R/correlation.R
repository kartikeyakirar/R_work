# For correlation matrix data required in numeric form
mainDir <- getwd()
subDir <- "figs/corr"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}

corelationMatrix<-function(arg1,arg2){
p1<-cor(subsetpurina)
write.csv(p1,file=paste0("output/",arg2,".csv"))

png(height=1200, width=1200, pointsize=25, file=paste0("figs/corr/",arg2,".png"))
corrplot(p1,method = "pie",addshade="positive",
         title=paste0(arg2,"--- Correlation Matrix"),
         addCoef.col = rgb(0,0,0,alpha =0.6),
         addgrid.col="red",type="full",mar=c(0,0,1,0))
dev.off()
}