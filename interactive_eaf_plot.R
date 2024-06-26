library(ggplot2)
library(plotly)
library(eaf)
library(dplyr)
library(moocore)
library(ggthemes)
library(scales)
library(tidyr)
library(tidyverse)

#things to do:
#fix shading/filling upto Inf or similar
#choice of dashed line or not
#size of points
#legend position
#x axis side:bottom or top
#y axis side: left or right
#axes: axes or no axes?
#fix sci.notation


interactiveeafplot <- function(x, percentiles=c(0,50,100), maximise=FALSE, col=c("white","black"), type="point", xlabel=NULL, ylabel =NULL, sci.notation=FALSE){
  
  #Function to plot axis in scientific format
  scientific <- function(x){
    ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
  }
  
  #CHECK x input
  #table needs to be of size three columns
  #needs to be data frame
  if (ncol(x)!=3){
    stop("Each element must have three columns")
  }
  if (is.data.frame(x)==FALSE){
    stop("Each element must be of type data frame") 
  }
  
  #CHECK sci.notation input
  #If not Boolean output stop
  if (sci.notation != TRUE&FALSE){
    stop("sci.notation must be a boolean value")
  }
  
  if(length(maximise)==1){
    if (maximise==TRUE){
      maximise<-c(TRUE,TRUE)
    }
    else if (maximise==FALSE){
      maximise<-c(FALSE,FALSE)
    }
  }
  
  
  eafdata<-as.data.frame(eaf(x,maximise = maximise,percentiles = percentiles))
  newdata2 <- eafdata
  newdata2<-arrange(newdata2,desc(newdata2[2]))
  newdata2$V3[newdata2$V3==0] <- "Best"
  newdata2$V3[newdata2$V3==50] <- "Median"
  newdata2$V3[newdata2$V3==100] <- "Worst"
  colnames(newdata2) <- c("Time","Best","Scenario")
  
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
    group_by(as.character(Scenario)) %>%
    mutate(next_Time = lead(Time, default = last(Time))) %>%
    mutate(next_Best = lead(Best, default = last(Best))) %>%
    mutate(fill_color = factor(Scenario))
  
  p <- ggplot(data = newdata3, aes(x=Time, y=Best, color=factor(Scenario))) +
    {if (any(c("white") %in% col)){
      scale_color_manual(values = plotcol[plotcol != "#FFFFFF"])
    }
      else{
        scale_color_manual(values = plotcol)
        }
      } +
    labs(color = NULL) +
    geom_step(direction = "vh") +
    {if(type=="area"){
      if (any(c("white") %in% col)){
        scale_fill_manual(values = plotcol[plotcol != "#FFFFFF"],name="")
        }
      else{
        scale_fill_manual(values = plotcol,name="")
        }
      }
      } +
    geom_point(size=2) +
    theme_bw() +
    {if (sci.notation==TRUE)
      scale_x_continuous(label = scientific)} +
    {if (sci.notation==TRUE)
      scale_y_continuous(label = scientific)} +
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
        geom_rect(aes(xmin = next_Time, xmax = min(newdata2$Time), ymin = next_Best, ymax = min(newdata2$Best), fill = fill_color), alpha = 0.6,colour=NA)
    }
    else if(all(maximise==c(FALSE,FALSE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = max(newdata2$Time), ymin =max(newdata2$Best),ymax=next_Best, fill = fill_color), alpha = 0.6,colour=NA)
    }
    else if(all(maximise==c(TRUE,FALSE))){
      p<-p+
        geom_rect(aes(xmin = next_Time, xmax = min(newdata2$Time), ymin = next_Best, ymax = max(newdata2$Best), fill = fill_color), alpha = 0.6,colour=NA)
    }
    else{
      p<-p+
        geom_rect(aes(xmin = Time, xmax = max(newdata2$Time), ymin = next_Best, ymax = min(newdata2$Best), fill = fill_color), alpha = 0.6,colour=NA)
    }
  }

  myplot<- ggplotly(p,dynamicTicks = TRUE)
  
  for (i in 1:length(myplot$x$data)){
    if (!is.null(myplot$x$data[[i]]$name)){
      myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
    }
  }
  myplot
  }

#Testing data
# data(gcp2x2)
# tabucol <- subset(gcp2x2, alg != "TSinN1")
# tabucol$alg <- tabucol$alg[drop=TRUE]
# data <- tabucol %>% filter(inst=="DSJC500.5")
# mydata <- data[c("time","best","run")]
# interactiveeafplot(mydata, c(0,50,100), col=c("yellow","red"),maximise=FALSE, type="area",sci.notation=FALSE, xlabel="MIN X", ylabel="MIN Y")
# interactiveeafplot(mydata,c(0,50,100), col=c("yellow","red"),maximise=TRUE,sci.notation = FALSE,xlab="MAX X",ylab="MAX Y")
# interactiveeafplot(mydata,c(0,50,100), col=c("yellow","red"),maximise=c(FALSE,TRUE),sci.notation = FALSE,xlab="MIN X",ylab="MAX Y")
# interactiveeafplot(mydata,c(0,50,100), col=c("yellow","red"),maximise=c(TRUE,FALSE),sci.notation = FALSE,xlab="MAX X",ylab="MIN Y")
# 
# data(SPEA2minstoptimeRichmond)
# SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60
# interactiveeafplot(SPEA2minstoptimeRichmond, col=c("yellow","red"), xlab = "C[E]",
#          ylab = "Minimum idle time (minutes)", maximise = c(FALSE, FALSE))
# interactiveeafplot(SPEA2minstoptimeRichmond, col=c("yellow","red"), xlab = "C[E]",
#                    ylab = "Minimum idle time (minutes)", maximise = c(TRUE, TRUE))
# interactiveeafplot(SPEA2minstoptimeRichmond, col=c("yellow","red"), xlab = "C[E]",
#                   ylab = "Minimum idle time (minutes)", maximise = c(FALSE, TRUE))
# interactiveeafplot(SPEA2minstoptimeRichmond, col=c("yellow","red"), xlab = "C[E]",
#                    ylab = "Minimum idle time (minutes)", maximise = c(TRUE, FALSE))
