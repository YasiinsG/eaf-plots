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
    threshold=NULL,
    thrcol="cyan",
    type="point",
    lty="solid",
    psize=0.1,
    pshape=16,
    legend.pos="topright",
    xaxis.side="bottom",
    yaxis.side="left",
    axes=TRUE,
    xlabel=NULL,
    ylabel =NULL,
    sci.notation=FALSE,
    plot="plotly"){
  
  if (all(maximise == c(TRUE, TRUE))) {
    dir<-"vh"
  } else if (all(maximise == c(FALSE, FALSE))) {
    dir<-"hv"
  } else if (all(maximise == c(TRUE, FALSE))) {
    dir<-"vh"
  } else {
    dir<-"vh"
  }
  
  if (plot=="plotly"){
    limvar <- .Machine$double.xmax
    limvarfill <- .Machine$integer.max
  }else{
    limvar <- Inf
    limvarfill <- Inf
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
  colnames(eafdata) <- c("Time","Best","Scenario")
  
  labels <- sapply(percentiles, function(pct) {
    if (pct == 0) return("Best")
    if (pct == 50) return("Median")
    if (pct == 100) return("Worst")
    return(paste0(pct, "%"))
  })
  
  eafdata$Scenario <- factor(eafdata$Scenario, levels = percentiles, labels = labels)
  newdata2 <- eafdata
  
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
  
  if (all(maximise == c(TRUE, TRUE))) {
    big_data$Best <- limvar * -1
    big_data2$Time <- limvar * -1
  } else if (all(maximise == c(FALSE, FALSE))) {
    big_data$Best <- limvar
    big_data2$Time <- limvar
  } else if (all(maximise == c(TRUE, FALSE))) {
    big_data$Best <- limvar
    big_data2$Time <- limvar * -1
  } else {
    big_data$Best <- limvar *-1
    big_data2$Time <- limvar 
  }
  
  big_data3 <- rbind(big_data2,big_data)
  newdata3<- rbind(newdata2,big_data3)
  
  # if (all(maximise == c(FALSE,TRUE))) {
  #   newdata3<-arrange(newdata3,newdata3$Best)
  #   newdata3<-arrange(newdata3,newdata3$Scenario)
  # }
  # else if (all(maximise == c(FALSE,FALSE))){
  #   newdata3<-arrange(newdata3,desc(newdata3$Best))
  #   newdata3<-arrange(newdata3,newdata3$Scenario)
  # }
  # else if (all(maximise == c(TRUE,FALSE))){
  #   newdata3<-arrange(newdata3,newdata3$Best)
  #   newdata3<-arrange(newdata3,newdata3$Scenario)
  # }
  # else {
  #   newdata3<-arrange(newdata3,desc(newdata3$Best))
  #   newdata3<-arrange(newdata3,newdata3$Scenario)
  # }
  
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
    geom_step(direction = dir,linetype=lty,show.legend = FALSE) +
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
      geom_point(size=psize,shape=pshape,show.legend = FALSE,
                 aes(text= paste("Time: ", Time, "<br>", 
                                 "Best: ", Best, "<br>",
                                 "Percentile: ", Scenario, sep = "")))
    }}+
    theme_bw() +
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
        ylab(ylabel)}}+
    coord_cartesian(xlim = c(min(newdata2$Time),max(newdata2$Time)),ylim = c(min(newdata2$Best),max(newdata2$Best)))
  
  if (sci.notation==TRUE){
    p<-p+theme(axis.text.x = element_text(angle = 90))+
      scale_x_continuous(position = xaxis.side,labels = function(x) format(x, scientific = TRUE))+
      scale_y_continuous(position = yaxis.side,labels = function(x) format(x, scientific = TRUE))
  }else{
    p<-p+
      scale_x_continuous(position = xaxis.side)+
      scale_y_continuous(position = yaxis.side)
  }
  if (plot=="ggplot"){
    {if(legend.pos=="top"){
      p<-p+ theme(legend.title = element_blank(),
                  legend.position = "top")
    }
      else if(legend.pos=="topleft"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "left",
                    legend.position = "top")
      }
      else if(legend.pos=="bottomright"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "right",
                    legend.position = "bottom")
      }
      else if(legend.pos=="bottom"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.position = "bottom")
      }
      else if(legend.pos=="bottomleft"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "left",
                    legend.position = "bottom")
      }
      else if(legend.pos=="right"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "right")
      }
      else if(legend.pos=="left"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.position  = "left")
      }
      else if(legend.pos=="center"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.position = "inside")
      }
      else{
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "right",
                    legend.position = "top")
      }}
  }
  print(newdata3)
  if (type=="area"){
    if (all(maximise==c(TRUE,TRUE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = limvarfill*-1 , ymin = Best, ymax = limvarfill*-1, fill = Scenario), colour=NA)
    }
    else if(all(maximise==c(FALSE,FALSE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = limvarfill, ymin =limvarfill,ymax=Best, fill = Scenario), colour=NA)
    }
    else if(all(maximise==c(TRUE,FALSE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = limvarfill*-1, ymin = Best, ymax = limvarfill, fill = Scenario),colour=NA)
    }
    else{
      p<-p+
        geom_rect(aes(xmin = Time, xmax = limvarfill, ymin = next_Best, ymax = limvarfill*-1, fill = Scenario),colour=NA)
    }
    
    # Add filling for percentiles using geom_ribbon
    # for (i in seq_along(percentiles)[-length(percentiles)]) {
    #   lower_label <- labels[i]
    #   upper_label <- labels[i + 1]
    #   
    #   lower_data <- newdata3 %>% filter(Scenario == lower_label)
    #   upper_data <- newdata3 %>% filter(Scenario == upper_label)
    #   
    #   fill_data <- merge(lower_data, upper_data, by = "Time", suffixes = c("_lower", "_upper"))
    #   fill_data <- fill_data %>%
    #     mutate(next_Time = lead(Time, default = last(Time)))
    #   print(fill_data)
    #   p <- p + geom_rect(
    #     data = fill_data,
    #     aes(xmin = Time,xmax=next_Time_lower, ymin = Best_lower, ymax = next_Best_upper, fill = Scenario_upper),
    #     alpha = 0.2, inherit.aes = FALSE
    #   )
    # }
    
    thrdata<-as.data.frame(eaf(x,maximise = maximise,percentiles = threshold))
    colnames(thrdata) <- c("Time","Best","Scenario")
    thrdata$Scenario <- factor(thrdata$Scenario, levels = percentiles, labels = labels)
    
    p<-p+geom_step(data = thrdata,aes(x=Time,y=Best,direction = dir),colour=thrcol)
  }
  
  if (plot=="ggplot"){
    p
  }
  else{
    
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
          layout(legend = list(orientation = "v",xanchor="left",yanchor="top", y = 0.99, x = 0.005))
      }
      else if(legend.pos=="bottomright"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="right",yanchor="bottom", y = 0.01, x = 0.995))
      }
      else if(legend.pos=="bottom"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "h",xanchor="center",yanchor="bottom", y = 0.01, x = 0.5))
      }
      else if(legend.pos=="bottomleft"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="left",yanchor="bottom", y = 0.01, x = 0.005))
      }
      else if(legend.pos=="right"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="right",yanchor="middle", y = 0.5, x = 0.995))
      }
      else if(legend.pos=="left"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="left",yanchor="middle", y = 0.5, x = 0.005))
      }
      else if(legend.pos=="center"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="center",yanchor="middle", y = 0.5, x = 0.425))
      }
      else{
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="right",yanchor="top", y = 0.99, x = 0.995))
      }}
    
    y_padding = (max(newdata2$Best)-min(newdata2$Best))/length(newdata2$Best)
    min_y = min(newdata2$Best) - y_padding
    max_y = max(newdata2$Best) + y_padding
    
    x_padding = (max(newdata2$Time)-min(newdata2$Time))/length(newdata2$Time)
    min_x = min(newdata2$Time) - x_padding
    max_x = max(newdata2$Time) + x_padding
    
    if (type=="area"){
      
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
}

data(CPFs, package = "moocore")
res <- moocore::vorobT(CPFs, reference = c(2, 200))
print(res$threshold)
#> [1] 44.14062

## Display Vorob'ev expectation and attainment function
# First style
interactiveeafplot(x=CPFs,threshold=res$threshold, percentiles = c(0,20,40,60,80,100),
                   type="area",thrcol="cyan",plot = "plotly")
