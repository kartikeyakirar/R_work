
trend_calc <- function(trend_data, func_name = mean)
{
  trend_data <- unname(sapply(trend_data, function(w) func_name(as.numeric(strsplit(w, ",")[[1]]))))
  return(trend_data)
}

## function for extracting trend data
trend_extract <- function(trend_data)
{
  trend_data <- unname(sapply(trend_data, function(w) as.numeric(strsplit(w, ",")[[1]])))
  return(trend_data)
}

## function for extracting wavelet coefficients
wavelet_coef <- function(data) 
{
  data_coef <- dwt(as.ts(data), filter = "haar", boundary = "periodic")
  data_coef <- unname(unlist(c(data_coef@W,data_coef@V[[data_coef@level]])))
  return(data_coef)
}

mainDir <- getwd()
subDir <- "figs/trends"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}

# Trend analysis#amap
Domain_trend_analyses<-function(arg1,arg2){
  arg1<-arg1[1:arg2,]
temp_trends <- trend_extract(as.character(arg1$Trends))## getting trend numeric data to use further 
temp_coefs <- t(apply(temp_trends, 2, wavelet_coef))
temp_coefs_dist <- Dist(temp_coefs, method = "euclidean",nbproc = 8)
temp_hclust <- hclust(temp_coefs_dist, method = "ward.D")
temp_hclust
}
# input domain_trend
plothclust<-function(arg1,arg2){
c<-ggdendrogram(arg1, rotate = FALSE, size = 2,labels = T)+
  labs(title="Hierarchical clustering on basis of  trend analysis",x = "Index of Food searched ")
ggsave(filename = paste0("figs/trends/hclust_",arg2,".png"),c)
#rect.hclust(temp_hclust, 2)
}
#temp_nhclust <- cutree(temp_hclust, 2)
