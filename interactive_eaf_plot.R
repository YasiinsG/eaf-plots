library(ggplot2)
library(plotly)
library(eaf)
library(dplyr)
library(moocore)
library(ggthemes)
library(scales)


interactiveeafplot <- 
  function(x,percentiles=c(10,50,100),sci.notation=FALSE) {
    ##DO ALL CHECKS AGAINST INPUTS FIRST, THEN PLOT GRAPH
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
    #If not boolean output stop
    if (sci.notation != TRUE | sci.notation != FALSE){
      stop("sci.notation must be a Boolean value")
    }
    
    
    ##PLOT GRAPH
    eafdata<-as.data.frame(eaf(x))
    newdata2 <- eafdata %>% filter(V3 %in% percentiles)
    newdata2<-arrange(newdata2,V2)
    #newdata2 <- mutate(newdata2, V3 = case_when( 
    #  V3  == 10 ~ "Best",
    #  V3 == 50 ~ "Median",
    #  V3 == 100 ~ "Worst"))
    colnames(newdata2) <- c("Time","Best","Scenario")
    
    
    p <- ggplot(newdata2, aes(x=Time, 
                         y=Best,
                         color=factor(Scenario))) +
      geom_point(size=3) +
      labs(x = colnames(newdata2)[1],
           y = colnames(newdata2)[2],
           color = "Scenario") +
      geom_step(direction = "hv") +
      {if (sci.notation==TRUE)
        scale_x_continuous(label = scientific)} +
      {if (sci.notation==TRUE)
        scale_y_continuous(label = scientific)} +
      theme_bw()
      
    ggplotly(p)
    
  }

data(gcp2x2)
tabucol <- subset(gcp2x2, alg != "TSinN1")
tabucol$alg <- tabucol$alg[drop=TRUE]
data<-tabucol %>% filter(inst=="DSJC500.5")