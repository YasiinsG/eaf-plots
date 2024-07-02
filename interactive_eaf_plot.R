library(ggplot2)
library(plotly)
library(eaf)
library(dplyr)
library(moocore)
library(ggthemes)
library(scales)
library(tidyr)
library(tidyverse)

interactiveeafplot <- function(
    x,
    percentiles=c(0,50,100),
    maximise=FALSE,
    col=c("white","black"),
    type="point",
    lty="solid",
    psize=2,
    pshape=16,
    legend.pos="topright",
    xaxis.side="below",
    yaxis.side="left",
    axes=TRUE,
    xlabel=NULL,
    ylabel =NULL,
    sci.notation=FALSE){
  
  if(length(maximise)==1){
    if (maximise==TRUE){
      maximise<-c(TRUE,TRUE)
    }
    else if (maximise==FALSE){
      maximise<-c(FALSE,FALSE)
    }
  }
  
  eafdata<-as.data.frame(eaf(x,maximise = maximise,percentiles = percentiles))
  colnames(eafdata) <- c("Time","Best","Scenario")
  
  labels <- sapply(percentiles, function(pct) {
    if (pct == 0) return("Best")
    if (pct == 50) return("Median")
    if (pct == 100) return("Worst")
    return(paste0(pct, "%"))
  })
  
  eafdata$Scenario <- factor(eafdata$Scenario, levels = percentiles, labels = labels)
  newdata2 <- eafdata
  newdata2<-arrange(newdata2,desc(newdata2[2]))
  
  uniqueScenario <- unique(newdata2$Scenario)
  
  datalist=list()
  datalist2=list()
  i=1
  for (a in uniqueScenario){
    uniqueData <- newdata2 %>% filter(Scenario %in% a)
    firstDataElement <- head(uniqueData,n=1)
    lastDataElement <- tail(uniqueData,n=1)
    datalist[[i]]<- firstDataElement
    datalist2[[i]]<-lastDataElement
    i <- i+1
  }
  big_data = data.frame(do.call(rbind, datalist))
  big_data2 = data.frame(do.call(rbind, datalist2))
  
  if (all(maximise==c(TRUE,TRUE))){
    big_data$Time <- .Machine$double.xmax*-1
    big_data2$Best <- .Machine$double.xmax*-1}
  else if(all(maximise==c(FALSE,FALSE))){
    big_data$Best <- .Machine$double.xmax
    big_data2$Time <- .Machine$double.xmax}
  else if(all(maximise==c(TRUE,FALSE))){
    big_data$Best <- .Machine$double.xmax
    big_data2$Time <- .Machine$double.xmax*-1}
  else{
    big_data$Time <- .Machine$double.xmax
    big_data2$Best <- .Machine$double.xmax*-1}
  
  big_data3 <- rbind(big_data2,big_data)
  newdata3<- rbind(newdata2,big_data3)
  
  if(all(maximise==c(TRUE,FALSE))){
    newdata3<-arrange(newdata3,newdata3$Best)
  }
  else if(all(maximise==c(FALSE,TRUE))){
    newdata3<-arrange(newdata3,newdata3$Best)
  }
  else{
    newdata3<-arrange(newdata3,desc(newdata3$Best))}
  
  pal<-colorRampPalette(colors=col,space="Lab")
  if (any(c("white") %in% col)){
    plotcol<-pal(length(uniqueScenario)+1)
  }
  else{
    plotcol<-pal(length(uniqueScenario))
  }

  newdata3 <- newdata3 %>%
    group_by(Scenario) %>%
    mutate(next_Time = lead(Time, default = last(Time)),
           next_Best = lead(Best, default = last(Best))) %>%
    ungroup()
  
  p <- ggplot(data = newdata3, aes(x=Time, y=Best, color=Scenario)) +
    {if (any(c("white") %in% col)){
      scale_color_manual(values = plotcol[plotcol != "#FFFFFF"])
    }
      else{
        scale_color_manual(values = plotcol)
        }
      } +
    labs(color = NULL) +
    geom_step(direction = "vh",linetype=lty) +
    {if(type=="area"){
      if (any(c("white") %in% col)){
        scale_fill_manual(values = plotcol[plotcol != "#FFFFFF"],name="")
        }
      else{
        scale_fill_manual(values = plotcol,name="")
        }
      }
      } +
    {if(psize!=0){
      geom_point(size=psize,shape=pshape,
                 aes(text= paste("Time: ", Time, "<br>", 
                             "Best: ", Best, "<br>",
                             "Percentile: ", Scenario, sep = "")))
    }}+
    theme_bw() +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(legend.position = legend.pos)+
    {if (sci.notation==TRUE){
      theme(axis.text.x = element_text(angle = 90))
    }}+
    {if (axes==FALSE){
          theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())}}+
    {if (is.null(xlabel)){
      xlab(colnames(newdata3)[1])}
      else {
        xlab(xlabel)}} +
    {if (is.null(ylabel)){
      ylab(colnames(newdata3)[2])}
      else {
        ylab(ylabel)}}
  
  if (type=="area"){
    if (all(maximise==c(TRUE,TRUE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max *-1, ymin = Best, ymax = .Machine$integer.max*-1, fill = Scenario), alpha = 0.6,colour=NA)
    }
    else if(all(maximise==c(FALSE,FALSE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max, ymin =.Machine$integer.max,ymax=next_Best, fill = Scenario), alpha = 0.6,colour=NA)
    }
    else if(all(maximise==c(TRUE,FALSE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max*-1, ymin = Best, ymax = .Machine$integer.max, fill = Scenario), alpha = 0.6,colour=NA)
    }
    else{
      p<-p+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max, ymin = next_Best, ymax = .Machine$integer.max*-1, fill = Scenario), alpha = 0.6,colour=NA)
    }
  }
  
  
  myplot<- ggplotly(p,dynamicTicks = TRUE,tooltip = c("text"))
  
  for (i in 1:length(myplot$x$data)){
    if (!is.null(myplot$x$data[[i]]$name)){
      myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
    }
  }
  
  
  {if(legend.pos=="top"){
    myplot<-myplot%>%
      layout(legend = list(orientation = "h",xanchor="center",yanchor="top", y = 0.99, x = 0.5))
  }
  else if(legend.pos=="topleft"){
    myplot<-myplot%>%
      layout(legend = list(orientation = "v",xanchor="left",yanchor="top", y = 0.99, x = 0.01))
  }
  else if(legend.pos=="bottomright"){
    myplot<-myplot%>%
      layout(legend = list(orientation = "v",xanchor="right",yanchor="bottom", y = 0.01, x = 0.99))
  }
  else if(legend.pos=="bottom"){
    myplot<-myplot%>%
      layout(legend = list(orientation = "h",xanchor="center",yanchor="bottom", y = 0.01, x = 0.5))
  }
  else if(legend.pos=="bottomleft"){
    myplot<-myplot%>%
      layout(legend = list(orientation = "v",xanchor="left",yanchor="bottom", y = 0.01, x = 0.01))
  }
  else if(legend.pos=="right"){
    myplot<-myplot%>%
      layout(legend = list(orientation = "v",xanchor="right",yanchor="middle", y = 0.5, x = 0.99))
  }
  else if(legend.pos=="left"){
    myplot<-myplot%>%
      layout(legend = list(orientation = "v",xanchor="left",yanchor="middle", y = 0.5, x = 0.01))
  }
  else if(legend.pos=="center"){
    myplot<-myplot%>%
      layout(legend = list(orientation = "v",xanchor="center",yanchor="middle", y = 0.5, x = 0.425))
  }
  else{
    myplot<-myplot%>%
      layout(legend = list(orientation = "v",xanchor="right",yanchor="top", y = 0.99, x = 0.99))
  }}
  
  if (type=="area"){
  y_padding = (max(newdata2$Best)-min(newdata2$Best))/length(newdata2$Best)
  min_y = min(newdata2$Best) - y_padding
  max_y = max(newdata2$Best) + y_padding
  
  x_padding = (max(newdata2$Time)-min(newdata2$Time))/length(newdata2$Time)
  min_x = min(newdata2$Time) - x_padding
  max_x = max(newdata2$Time) + x_padding
  
    myplot <- myplot %>%
      layout(
        xaxis = list(autorange=FALSE,range=c(min_x,max_x),side = xaxis.side),
        yaxis = list(autorange=FALSE,range=c(min_y,max_y),side = yaxis.side)
      )
  }else{
    myplot <- myplot %>%
      layout(
        xaxis = list(side = xaxis.side),
        yaxis = list(side = yaxis.side)
      )
  }
  if (sci.notation==TRUE){
    myplot<-myplot %>%
      layout(
        xaxis = list(tickformat=".2e"),
        yaxis = list(tickformat=".2e")
      )
  }
  
  return(myplot)
  }

#Testing data
data(gcp2x2)
tabucol <- subset(gcp2x2, alg != "TSinN1")
tabucol$alg <- tabucol$alg[drop=TRUE]
data <- tabucol %>% filter(inst=="DSJC500.5")
mydata <- data[c("time","best","run")]

interactiveeafplot(mydata, c(0,50,100), col=c("yellow","red"),
                   maximise=FALSE, type="area", lty="longdash",
                   psize=3, pshape=10, legend.pos="bottomleft",
                   xaxis.side="bottom", yaxis.side="left", axes=TRUE,
                   sci.notation=FALSE, xlabel="MIN X", ylabel="MIN Y")

interactiveeafplot(mydata, c(0,50,100), col=c("yellow","red"),
                   maximise=TRUE, type="area", lty="longdash",
                   psize=3, pshape=10, legend.pos="topright",
                   xaxis.side="top", yaxis.side="right", axes=TRUE,
                   sci.notation=TRUE, xlabel="MAX X", ylabel="MAX Y")

interactiveeafplot(mydata, c(0,50,100), col=c("yellow","red"),
                   maximise=c(FALSE,TRUE), type="area", lty="longdash",
                   psize=3, pshape=10, legend.pos="topleft",
                   xaxis.side="top", yaxis.side="left", axes=TRUE,
                   sci.notation=TRUE, xlabel="MIN X", ylabel="MAX Y")

interactiveeafplot(mydata, c(0,50,100), col=c("yellow","red"),
                   maximise=c(TRUE,FALSE), type="area", lty="longdash",
                   psize=3, pshape=10, legend.pos="bottomright",
                   xaxis.side="bottom", yaxis.side="right", axes=TRUE,
                   sci.notation=TRUE, xlabel="MAX X", ylabel="MIN Y")

data(SPEA2minstoptimeRichmond)
SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60

interactiveeafplot(SPEA2minstoptimeRichmond, percentiles = c(0,50,100),
                   col=c("yellow","red"),maximise=FALSE, type="area",
                   lty="longdash", psize=3, pshape=10, legend.pos="bottomleft",
                   xaxis.side="bottom", yaxis.side="left", axes=TRUE,
                   sci.notation=FALSE, xlabel = "C[E]",
                   ylabel = "Minimum idle time (minutes)")

interactiveeafplot(SPEA2minstoptimeRichmond, percentiles = c(0,50,100),
                   col=c("yellow","red"), maximise=TRUE, type="area",
                   lty="longdash", psize=3, pshape=10, legend.pos="topright",
                   xaxis.side="top", yaxis.side="right", axes=TRUE,
                   sci.notation=TRUE, xlabel = "C[E]",
                   ylabel = "Minimum idle time (minutes)")

interactiveeafplot(SPEA2minstoptimeRichmond, percentiles = c(0,50,100),
                   col=c("yellow","red"), maximise=c(FALSE,TRUE), type="area",
                   lty="longdash", psize=3, pshape=10, legend.pos="topleft",
                   xaxis.side="top", yaxis.side="left", axes=TRUE,
                   sci.notation=TRUE, xlabel = "C[E]",
                   ylabel = "Minimum idle time (minutes)")

interactiveeafplot(SPEA2minstoptimeRichmond, percentiles = c(0,50,100),
                   col=c("yellow","red"), maximise=c(TRUE,FALSE), type="area",
                   lty="longdash", psize=3, pshape=10, legend.pos="bottomright",
                   xaxis.side="bottom", yaxis.side="right", axes=TRUE,
                   sci.notation=TRUE, xlabel = "C[E]",
                   ylabel = "Minimum idle time (minutes)")
