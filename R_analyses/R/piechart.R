mainDir <- getwd()
subDir <- "figs/pie"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot_pie_3d_eachDomain <- function(other,common,excl_p,excl_w){
  slices <- c(length(other),length(common), length(excl_p),length(excl_w)) 
  lbls <- c("Others food item", "Common search from purina and whiskas", "Exclusive search drom purina", "Exclusive search from Whiskas")
  pct <- round(slices/nrow(export)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  s1<-data.frame("Group"<-lbls,"Values"<-slices)
  pie <- ggplot(s1, aes(x="", y=Values, fill=Group))+
    geom_bar(width = 1, stat = "identity")+
    coord_polar("y", start=0)+
    theme_grey(base_size = 18) +
    labs(title="Pie Representation of % share of  cat_food keywords from Whiskas and Purina")
  pie
  }
  
  ####plot_pie_3d(nothing_in_pw,common_pXw,excl_pXe,excl_wXe)
  
html_pie_3d_eachDomain <- function(other,common,excl_p,excl_w){
  slices <- c(length(other),length(common), length(excl_p),length(excl_w)) 
  lbls <- c("Others food item", "Common search from purina and whiskas", "Exclusive search drom purina", "Exclusive search from Whiskas")
  pct <- round(slices/nrow(export)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  d<-data.frame(labels=lbls,values=slices)
  
  plot2<-plot_ly(d, labels = labels, values = values, type = "pie",hole = 0.6,showlegend=F) %>%
    layout(title = "Pie Chart for each domain search ")
  plot2
}

####plot_pie_3d(nothing_in_pw,common_pXw,excl_pXe,excl_wXe)

plot_pie_catfood<-function(arg1,arg2,arg3,arg4){
  slices1<- c(length(arg1),length(arg2))
  slices2<- c(length(arg3),length(arg4))
  pct1 <- round(slices1/nrow(purina)*100)
  pct2 <- round(slices2/nrow(whiskas)*100)
  lblsC<-c("Other Food items Search","Cat Food item search")
  lbls1 <- paste(lblsC, pct1) # add percents to labels 
  lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
  lbls2 <- paste(lblsC, pct2) # add percents to labels 
  lbls2 <- paste(lbls2,"%",sep="") # ad % to labels 
  
  d1<-data.frame(labels=lbls1,values=slices1)
  d2<-data.frame(labels=lbls2,values=slices2)
  
  pie1 <- ggplot(d1, aes(x="", y=values, fill=labels))+
    geom_bar(width = 1, stat = "identity")+
    coord_polar("y", start=0)+
    theme_grey(base_size = 18) +
    labs(title="CatFood category keyword  % share in Purina")
  
  pie2 <- ggplot(d2, aes(x="", y=values, fill=labels))+
    geom_bar(width = 1, stat = "identity")+
    coord_polar("y", start=0)+
    theme_grey(base_size = 18) +
    labs(title="CatFood category keyword  % share in Whiskas")
  
  multiplot(pie1,pie2)
   
}
###plot_pie_catfood(pXe_withDuplicated,other_pXe,wXe_withDuplicated,other_wXe)


html_pie_catfood<-function(arg1,arg2,arg3,arg4){
slices1<- c(length(arg1),length(arg2))
slices2<- c(length(arg3),length(arg4))
pct1 <- round(slices1/nrow(purina)*100)
pct2 <- round(slices2/nrow(whiskas)*100)
lblsC<-c("Other Food items Search","Cat Food item search")
lbls1 <- paste(lblsC, pct1) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
lbls2 <- paste(lblsC, pct2) # add percents to labels 
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels 

d1<-data.frame(labels=lbls1,values=slices1)
d2<-data.frame(labels=lbls2,values=slices2)
plot1<-plot_ly(data = d1, labels = labels, values = values, type = "pie", domain = list(x = c(0,0.4), y = c(0.4, 1)),
        name = "Purina.com ", showlegend = F)%>% 
add_trace(data = d2, labels = labels, values = values, type = "pie", domain = list(x = c(0.6, 1), y = c(0.4, 1)),
          name = "Whiskas.com", showlegend = F) %>%
layout(title = "Cat Food Search % from each Domain and ")  

plot1
}
###html_pie_catfood(pXe_withDuplicated,other_pXe,wXe_withDuplicated,other_wXe)

compare_position<-function(arg1,arg2){
  dfd1<-as.data.frame(table(arg1$Position))
  dfd2<-as.data.frame(table(arg1$Previous.Position))
  p<-plot_ly(x=arg1$Position,opacity=0.6,type="histogram",width = 700,name="Current position @ purina")%>%
    add_trace(x=arg1$Previous.Position,opacity=0.6,type="histogram",width = 700,name="Previous position @ purina")%>%
    layout(barmode="hi")%>%
    layout(yaxis=list(range=c(0,900)))%>%
    layout(title="Comparision between Position distribuiton in purina.com",
           xaxis=list(title="Positions"),
           yaxis=list(title="Frequency"))%>%add_trace(x=dfd1$Var1,y=dfd1$Freq,mode="line",name="current position")%>%add_trace(x=dfd2$Var1,y=dfd2$Freq,mode="line",name="privious position")
 htmlwidgets::saveWidget(as.widget(p),"position_distribution_purina.html")
  df1<-as.data.frame(table(arg2$Position))
  df2<-as.data.frame(table(arg2$Previous.Position))
  
  w<-  plot_ly(x=arg2$Position,opacity=0.6,type="histogram",width = 700,name="Current position @ whiskas")%>%
    add_trace(x=arg2$Previous.Position,opacity=0.6,type="histogram",width = 700,name="Previous position @ whiskas")%>%
    layout(yaxis=list(range=c(0,900)))%>%
    layout(title="Comparision between Position distribuiton in whiskas.com",
           xaxis=list(title="Positions"),
           yaxis=list(title="Frequency"))%>%add_trace(x=df1$Var1,y=df1$Freq,mode="line",name="current position")%>%add_trace(x=df2$Var1,y=df2$Freq,mode="line",name = "Previous position")
  
htmlwidgets::saveWidget(as.widget(w),"position_distribution_whiskas.html")
  
  
  plot1<-subplot(p,w,margin = 0.05,nrows = 2) %>%
    layout(title="Positon Distribution" )
  plot1
}

#compare_position(purina,whiskas)

