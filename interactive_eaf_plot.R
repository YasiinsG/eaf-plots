library(ggplot2)
library(plotly)
library(eaf)
library(dplyr)
library(moocore)
library(ggthemes)
library(scales)


interactiveeafplot <- 
  function(x, percentiles=c(10,50,100), maximise=FALSE, xlabel=NULL, ylabel =NULL, sci.notation=FALSE){
    
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
    
    
    #CHECK percentiles input
    #if not in c(10,...,100) output stop
    percentileIntervals <- c(10,20,30,40,50,60,70,80,90,100)
    for (i in percentiles){
      if ((i %in% percentileIntervals) == TRUE){
        percentileIntervals <- percentileIntervals[percentileIntervals != i]
      } else {
        stop("percentile must be a suitable value between 10 and 100 in intervals of 10")
      }
    }
    
    
    #CHECK sci.notation input
    #If not Boolean output stop
    if (sci.notation != TRUE&FALSE){
      stop("sci.notation must be a boolean value")
    }
    
    ##PLOT GRAPH
    eafdata<-as.data.frame(eaf(x,maximise = maximise))
    #eafdata$V3<- round_any(eafdata$V3, 10, f = ceiling)
    print(eafdata)
    eafdata[3]<-round(eafdata[3],-1)
    print(eafdata)
    newdata2 <- eafdata %>% filter(V3 %in% percentiles)
    newdata2<-arrange(newdata2,desc(newdata2[2]))
    newdata2 <- mutate(newdata2, V3 = case_when(
     V3  == 10 ~ "Best",
     V3 == 50 ~ "Median",
     V3 == 100 ~ "Worst"))
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
    
    if(length(maximise)==1){
      if (maximise==TRUE){
        big_data$Time <- .Machine$double.xmax*-1
        big_data2$Best <- .Machine$double.xmax*-1}
      else{
        big_data$Best <- .Machine$double.xmax
        big_data2$Time <- .Machine$double.xmax}
    }
    else{
      if(all(maximise==c(TRUE,FALSE))){
        big_data$Best <- .Machine$double.xmax
        big_data2$Time <- .Machine$double.xmax*-1}
      else{
        big_data$Time <- .Machine$double.xmax
        big_data2$Best <- .Machine$double.xmax*-1}
    }
    
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
    print(newdata3)
    
    
    p <- ggplot(data = newdata3, aes(x=Time,
                                     y=Best,
                                     color=factor(Scenario))) +
      geom_point(size=3) +
      labs(color = NULL) +
      geom_step(direction = "hv") +
      theme_bw()+
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
    
    ggplotly(p,dynamicTicks=TRUE )
  }

data(gcp2x2)
tabucol <- subset(gcp2x2, alg != "TSinN1")
tabucol$alg <- tabucol$alg[drop=TRUE]
data<-tabucol %>% filter(inst=="DSJC500.5")
mydata <- data[c("time","best","run")]
# interactiveeafplot(mydata,c(10,50,100),maximise=FALSE,sci.notation = FALSE,xlab="MIN X",ylab="MIN Y")
# interactiveeafplot(mydata,c(10,50,100),maximise=TRUE,sci.notation = FALSE,xlab="MAX X",ylab="MAX Y")
# interactiveeafplot(mydata,c(10,50,100),maximise=c(FALSE,TRUE),sci.notation = FALSE,xlab="MIN X",ylab="MAX Y")
# interactiveeafplot(mydata,c(10,50,100),maximise=c(TRUE,FALSE),sci.notation = FALSE,xlab="MAX X",ylab="MIN Y")

data(SPEA2minstoptimeRichmond)
SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60
# interactiveeafplot(SPEA2minstoptimeRichmond, xlab = "C[E]",
#          ylab = "Minimum idle time (minutes)", maximise = c(FALSE, FALSE))
# interactiveeafplot(SPEA2minstoptimeRichmond, xlab = "C[E]",
#                    ylab = "Minimum idle time (minutes)", maximise = c(TRUE, TRUE))
#interactiveeafplot(SPEA2minstoptimeRichmond, xlab = "C[E]",
#                   ylab = "Minimum idle time (minutes)", maximise = c(FALSE, TRUE))
# interactiveeafplot(SPEA2minstoptimeRichmond, xlab = "C[E]",
#                    ylab = "Minimum idle time (minutes)", maximise = c(TRUE, FALSE))
