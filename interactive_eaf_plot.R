library(ggplot2)
library(plotly)
library(eaf)
library(dplyr)
library(moocore)
library(ggthemes)
library(scales)


interactiveeafplot <- 
  function(x, percentiles=c(10,50,100), maximise=FALSE, xlabel=NULL, ylabel =NULL, sci.notation=FALSE){
    ##DO ALL CHECKS AGAINST INPUTS FIRST, THEN PLOT GRAPH
    
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
    eafdata<-as.data.frame(eaf(x))
    newdata2 <- eafdata %>% filter(V3 %in% percentiles)
    newdata2<-arrange(newdata2,desc(newdata2[2]))
    #newdata2 <- newdata[order(-newdata[2]),]
    #newdata2 <- order(newdata[newdata[2],])
    #newdata2 <- mutate(newdata2, V3 = case_when( 
    #  V3  == 10 ~ "Best",
    #  V3 == 50 ~ "Median",
    #  V3 == 100 ~ "Worst"))
    colnames(newdata2) <- c("Time","Best","Scenario")
    
    ##maximise
    #for x=2:n-1,
    #take y value of element x-1 and x value of element x+1 to be new point
    #get unique scenarios
    #loop thru each scenario filtering for just them.
    #then get new x and y
    #then merge back
    if (maximise==TRUE){
      #newdata2 %>%
      #  mutate(newcolx = NA) %>%
      #  mutate(newcoly = NA)
      newdata2<-arrange(newdata2,desc(newdata2[2]))
      uniqueScenario <- unique(newdata2$Scenario)
      maximisedData <- data.frame(matrix(NA, nrow = 0, ncol = ncol(newdata2)))
      colnames(maximisedData) <- c("Time","Best","Scenario")
      for (i in uniqueScenario){
        filteredData <- newdata2[newdata2$Scenario==i,]
        filteredData %>%
          mutate(newcolx = NA) 
        #%>%
          #mutate(newcoly = NA)
        
        for (a in 2:nrow(filteredData)){
          newXPoint <- filteredData[a+2,1]
          #newYPoint <- filteredData[a,2]
          filteredData[a,4] <- newXPoint
          #filteredData[a,5] <- newYPoint
        }
        filteredData <- subset(filteredData, select = -c(Time))
        filteredData <- filteredData %>%
          select(V4, everything())
        names(filteredData)[names(filteredData) == 'V4'] <- 'Time'
        #newdata2 <- merge(maximisedData,filteredData,all=TRUE)
        maximisedData<-rbind(maximisedData,filteredData)
        #newdata2$Time <- replace(newdata2$Time, filteredData$Time, newdata2$Best == filteredData$best && newdata2$Scenario == filteredData$Scenario)
        #newdata2 = merge(newdata2, filteredData, by = c("Best","Scenario"),all.x=TRUE)
        #newdata2 <- subset(newdata2, select=-c())
        #newdata2 %>% mutate(mycol = coalesce(x,y,z)) %>%
        #  select(a, mycol)
        #newdata2 <- merge(x=newdata2,y=filteredData,by=c("Best","Scenario"),all.x=TRUE)
      }
      maximisedData <- maximisedData[!is.na(maximisedData$Time), ]
      print(maximisedData)
      newdata2 <- maximisedData
      print(newdata2)
      #newdata2[1,1]<-NA
      #newdata2[2,1]<-NA
      #newdata2[1,tail(newdata2,n=1)]<-NA
      #newdata2[2,tail(newdata2,n=1)]<-NA
    }
    
    p <- ggplot(newdata2, aes(x=Time,
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
        xlab(colnames(newdata2)[1])}
        else {
          xlab(xlabel)}} +
      {if (is.null(ylabel)){
        ylab(colnames(newdata2)[2])}
        else {
          ylab(ylabel)}}
    
    'x = colnames(newdata2)[1],'
    'y = colnames(newdata2)[2],'
    ggplotly(p)
    
  }

data(gcp2x2)
tabucol <- subset(gcp2x2, alg != "TSinN1")
tabucol$alg <- tabucol$alg[drop=TRUE]
data<-tabucol %>% filter(inst=="DSJC500.5")
mydata <- data[c("time","best","run")]
interactiveeafplot(mydata,c(10,50,100),maximise=FALSE,sci.notation = TRUE,xlab="my x axis",ylab="my y axis")
interactiveeafplot(mydata,c(10,50,100),maximise=TRUE,sci.notation = TRUE,xlab="my x axis",ylab="my y axis")
