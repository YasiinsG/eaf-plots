library(ggplot2)
library(plotly)
library(eaf)
library(dplyr)
library(moocore)
library(ggthemes)
library(scales)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(stringr)

interactiveeafdiffplot <- function(
    x,
    y,
    intervals=3,
    percentiles=c(0,50,100),
    maximise=FALSE,
    col=c("white","black"),
    type="point",
    lty="solid",
    psize=0.1,
    pshape=16,
    legend.pos="topright",
    xaxis.side="below",
    yaxis.side="left",
    axes=TRUE,
    xlabel=NULL,
    ylabel =NULL,
    sci.notation=FALSE,
    plot="plotly",
    title=FALSE,
    title1="Data 1",
    title2="Data 2"){
  
  if(length(maximise)==1){
    if (maximise==TRUE){
      maximise<-c(TRUE,TRUE)
    }
    else if (maximise==FALSE){
      maximise<-c(FALSE,FALSE)
    }
  }
  
  #calculate intervals
  # Define parameters
  start <- 0
  end <- 100
  interval_width <- 100/intervals
  # Generate the intervals
  intervals <- seq(0, 1, by = interval_width/100)
  # Original sapply function
  interval_labels <- sapply(seq_along(intervals[-length(intervals)]), function(i) {
    paste0("[", round(intervals[i], 5), ", ", round(intervals[i + 1], 5), ")")
  })

  # Adjusted sapply function
  interval_labels <- sapply(seq_along(intervals[-length(intervals)]), function(i) {
    if (i == length(intervals) - 1) {
      # For the last interval, use [80, 100] instead of [80, 100)
      paste0("[", round(intervals[i], 5), ", ", round(intervals[i + 1], 5), "]")
    } else {
      paste0("[", round(intervals[i], 5), ", ", round(intervals[i + 1], 5), ")")
    }
  })
  
  lower_bounds <- seq(from = start, to = end, by = interval_width)
  lower_bounds <- lower_bounds[-length(intervals)]
  
  #50% of x and y
  eafdata <- as.data.frame(eaf(x, maximise = maximise, percentiles = c(lower_bounds)))
  eafdata2 <- as.data.frame(eaf(y, maximise=maximise, percentiles = c(lower_bounds)))
  #0 and 100% of joint
  joint <- rbind_datasets(x, y)
  eafdatajoint <- as.data.frame(eaf(joint, maximise = maximise))
  eafdatajoint1 <- eafdatajoint %>%
    filter(V3 %in% min(V3))
  eafdatajoint2 <- eafdatajoint %>%
    filter(V3 %in% max(V3))
  eafdata50 <- as.data.frame(eaf(x, maximise = maximise, percentiles = 50))
  eafdata250 <- as.data.frame(eaf(y, maximise = maximise, percentiles = 50))
  
  
  
  #make 2 datasets for 2 facet plots
  # percentiles<-c(min(eafdatajoint$V3),50,max(eafdatajoint$V3))
  # dataset1 <- rbind(eafdatajoint,eafdata)
  # dataset2 <- rbind(eafdatajoint,eafdata2)
  dataset1<- eafdata
  dataset2<-eafdata2
  
  colnames(dataset1) <- c("Time","Best","Scenario")
  colnames(dataset2) <- c("Time","Best","Scenario")
  colnames(eafdatajoint1) <- c("Time","Best","Scenario")
  colnames(eafdatajoint2) <- c("Time","Best","Scenario")
  colnames(eafdata50) <- c("Time","Best","Scenario")
  colnames(eafdata250) <- c("Time","Best","Scenario")
  
  
  # 
  # # Ensure Scenario is a factor
  # dataset1$Scenario <- as.factor(dataset1$Scenario)
  # dataset2$Scenario <- as.factor(dataset2$Scenario)
  # eafdatajoint1$Scenario <- as.factor(eafdatajoint1$Scenario)
  # eafdatajoint2$Scenario <- as.factor(eafdatajoint2$Scenario)
  # eafdata50$Scenario <- as.factor(eafdata50$Scenario)
  # eafdata250$Scenario <- as.factor(eafdata250$Scenario)
  
  print(lower_bounds)
  print(interval_labels)
  dataset1$Scenario <- factor(dataset1$Scenario, levels = lower_bounds, labels = interval_labels)
  dataset2$Scenario <- factor(dataset2$Scenario, levels = lower_bounds, labels = interval_labels)
  eafdatajoint1$Scenario <- factor(eafdatajoint1$Scenario, levels = lower_bounds, labels = interval_labels)
  eafdatajoint2$Scenario <- factor(eafdatajoint2$Scenario, levels = lower_bounds, labels = interval_labels)
  eafdata50$Scenario <- factor(eafdata50$Scenario, levels = lower_bounds, labels = interval_labels)
  eafdata250$Scenario <- factor(eafdata250$Scenario, levels = lower_bounds, labels = interval_labels)
  
  #extend grand lines
  uniqueScenario <- unique(eafdatajoint1$Scenario)
  uniqueScenario2 <- unique(eafdatajoint2$Scenario)
  uniqueScenario3 <- unique(eafdata50$Scenario)
  uniqueScenario4 <- unique(eafdata250$Scenario)
  
  datalist=list()
  datalist2=list()
  i=1
  for (a in uniqueScenario){
    uniqueData <- eafdatajoint1 %>% filter(Scenario %in% a)
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
  eafdatajoint1<- rbind(eafdatajoint1,big_data3)
  
  datalist=list()
  datalist2=list()
  i=1
  for (a in uniqueScenario2){
    uniqueData <- eafdatajoint2 %>% filter(Scenario %in% a)
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
  eafdatajoint2<- rbind(eafdatajoint2,big_data3)
  
  datalist=list()
  datalist2=list()
  i=1
  for (a in uniqueScenario3){
    uniqueData <- eafdata50 %>% filter(Scenario %in% a)
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
  eafdata50<- rbind(eafdata50,big_data3)
  
  datalist=list()
  datalist2=list()
  i=1
  for (a in uniqueScenario4){
    uniqueData <- eafdata250 %>% filter(Scenario %in% a)
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
  eafdata250<- rbind(eafdata250,big_data3)
  
  # labels <- sapply(percentiles, function(pct) {
  #   if (pct == 0) return("Best")
  #   if (pct == 50) return("Median")
  #   if (pct == 100) return("Worst")
  #   return(paste0(pct, "%"))
  # })
  # 
  # dataset1$Scenario <- factor(dataset1$Scenario, levels = percentiles, labels = labels)
  # dataset2$Scenario <- factor(dataset2$Scenario, levels = percentiles, labels = labels)
  # 
  newdata2 <- dataset1
  newdata22 <- dataset2
  #newdata2<-arrange(newdata2,desc(newdata2[2]))
  
  uniqueScenario <- unique(newdata2$Scenario)
  uniqueScenario2 <- unique(newdata22$Scenario)
  
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
  # if (any(c("white") %in% col)){
  #   plotcol<-pal(length(uniqueScenario)+1)
  # }
  # else{
  #   plotcol<-pal(length(uniqueScenario))
  # }
  col[col %in% c("white", "#FFFFFF")] <- "transparent"
  plotcol<-pal(length(uniqueScenario))
  plotcol[plotcol %in% c("white", "#FFFFFF")] <- "transparent"
  print(plotcol)
  
  newdata3 <- newdata3 %>%
    group_by(Scenario) %>%
    mutate(next_Time = lead(Time, default = last(Time)),
           next_Best = lead(Best, default = last(Best))) %>%
    ungroup()
  
  p1 <- ggplot(data = newdata3, aes(x=Time, y=Best, color=Scenario)) +
    {if (any(c("white") %in% col)){
      scale_color_manual(values = plotcol[plotcol != "#FFFFFF"])
    }
      else{
        scale_color_manual(values = plotcol)
      }
    } +
    labs(color = NULL) +
    geom_step(direction = "hv",linetype=lty) +
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
      geom_point(size=psize,stroke=0,shape=pshape,
                 aes(text= paste("Objective 1: ", Time, "<br>",
                                 "Objective 2: ", Best, "<br>",
                                 "Percentile: ", Scenario, sep = "")))
    }}+
    theme_bw() +
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
      p1<-p1+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max *-1, ymin = Best, ymax = .Machine$integer.max*-1, fill = Scenario), alpha = 0.6,colour=NA)
    }
    else if(all(maximise==c(FALSE,FALSE))){
      #p<-p+
      #  geom_rect(aes(xmin = Time, xmax = .Machine$integer.max, ymin =.Machine$integer.max,ymax=next_Best, fill = Scenario), alpha = 0.6,colour=NA)
      p1<-p1+
        geom_rect(aes(xmin = Time, xmax = 1e25, ymin =1e25,ymax=Best, fill = Scenario), alpha = 0.6,colour=NA)
      
      
    }
    else if(all(maximise==c(TRUE,FALSE))){
      p1<-p1+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max*-1, ymin = Best, ymax = .Machine$integer.max, fill = Scenario), alpha = 0.6,colour=NA)
    }
    else{
      p1<-p1+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max, ymin = next_Best, ymax = .Machine$integer.max*-1, fill = Scenario), alpha = 0.6,colour=NA)
    }
  }
  
  p1<-p1+
    geom_step(data = eafdatajoint1,aes(x=Time,y=Best),color= "black")+
    {if(psize!=0){
      geom_point(data = eafdatajoint1,stroke=0,color="black",size=psize,shape=pshape,
                 aes(text= paste("Objective 1: ", Time, "<br>",
                                 "Objective 2: ", Best, "<br>",
                                 "Percentile: ", "Grand Worst", sep = "")))}}+
    
    geom_step(data = eafdatajoint2,aes(x=Time,y=Best),color= "white")+
      {if(psize!=0){
        geom_point(data = eafdatajoint2,stroke=0,color="white",size=psize,shape=pshape,
                   aes(text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", "Grand Best", sep = "")))}}+
    
    geom_step(data = eafdata50,aes(x=Time,y=Best),color= "black",linetype="dashed")+
      {if(psize!=0){
        geom_point(data = eafdata50,stroke=0,color="black",size=psize,shape=pshape,
                   aes(text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", 50, sep = "")))}}
  
  datalist=list()
  datalist2=list()
  i=1
  for (a in uniqueScenario2){
    uniqueData <- newdata22 %>% filter(Scenario %in% a)
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
  newdata3<- rbind(newdata22,big_data3)
  
  if(all(maximise==c(TRUE,FALSE))){
    newdata3<-arrange(newdata3,newdata3$Best)
  }
  else if(all(maximise==c(FALSE,TRUE))){
    newdata3<-arrange(newdata3,newdata3$Best)
  }
  else{
    newdata3<-arrange(newdata3,desc(newdata3$Best))}
  
  pal<-colorRampPalette(colors=col,space="Lab")
  # if (any(c("white") %in% col)){
  #   plotcol<-pal(length(uniqueScenario2)+1)
  # }
  # else{
  #   plotcol<-pal(length(uniqueScenario2))
  # }
  col[col %in% c("white", "#FFFFFF")] <- "transparent"
  plotcol<-pal(length(uniqueScenario2))
  plotcol[plotcol %in% c("white", "#FFFFFF")] <- "transparent"
  
  newdata3 <- newdata3 %>%
    group_by(Scenario) %>%
    mutate(next_Time = lead(Time, default = last(Time)),
           next_Best = lead(Best, default = last(Best))) %>%
    ungroup()
  
  p2 <- ggplot(data = newdata3, aes(x=Time, y=Best, color=Scenario)) +
    {if (any(c("white") %in% col)){
      scale_color_manual(values = plotcol[plotcol != "#FFFFFF"])
    }
      else{
        scale_color_manual(values = plotcol)
      }
    } +
    labs(color = NULL) +
    geom_step(direction = "hv",linetype=lty) +
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
      geom_point(size=psize,stroke=0,shape=pshape,
                 aes(text= paste("Objective 1: ", Time, "<br>",
                                 "Objective 2: ", Best, "<br>",
                                 "Percentile: ", Scenario, sep = "")))
    }}+
    theme_bw() +
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
      p2<-p2+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max *-1, ymin = Best, ymax = .Machine$integer.max*-1, fill = Scenario), alpha = 0.6,colour=NA)
    }
    else if(all(maximise==c(FALSE,FALSE))){
      #p<-p+
      #  geom_rect(aes(xmin = Time, xmax = .Machine$integer.max, ymin =.Machine$integer.max,ymax=next_Best, fill = Scenario), alpha = 0.6,colour=NA)
      p2<-p2+
        geom_rect(aes(xmin = Time, xmax = 1e25, ymin =1e25,ymax=Best, fill = Scenario), alpha = 0.6,colour=NA)
      
      
    }
    else if(all(maximise==c(TRUE,FALSE))){
      p2<-p2+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max*-1, ymin = Best, ymax = .Machine$integer.max, fill = Scenario), alpha = 0.6,colour=NA)
    }
    else{
      p2<-p2+
        geom_rect(aes(xmin = Time, xmax = .Machine$integer.max, ymin = next_Best, ymax = .Machine$integer.max*-1, fill = Scenario), alpha = 0.6,colour=NA)
    }
  }
  
  p2<-p2+
    geom_step(data = eafdatajoint1,aes(x=Time,y=Best),color= "black") +
    {if(psize!=0){
      geom_point(data = eafdatajoint1,stroke=0,color="black",size=psize,shape=pshape,
                 aes(text= paste("Objective 1: ", Time, "<br>",
                                 "Objective 2: ", Best, "<br>",
                                 "Percentile: ", "Grand Worst", sep = "")))}}+
    
    geom_step(data = eafdatajoint2,aes(x=Time,y=Best),color= "white")+
      {if(psize!=0){
        geom_point(data = eafdatajoint2,stroke=0,color="white",size=psize,shape=pshape,
                   aes(text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", "Grand Best", sep = "")))}}+
    
    geom_step(data = eafdata250,aes(x=Time,y=Best),color= "black",linetype="dashed")+
      {if(psize!=0){
        geom_point(data = eafdata250,stroke=0,color="black",size=psize,shape=pshape,
                   aes(text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", 50, sep = "")))}}
  
  p1<- p1+scale_y_continuous(position = "left")
  p2<- p2+scale_y_continuous(position = "right")
  
  # Convert ggplot objects to plotly objects
  p1_plotly <- ggplotly(p1,dynamicTicks = TRUE,tooltip = c("text"))
  p2_plotly <- ggplotly(p2, dynamicTicks = TRUE,tooltip = c("text"))
  
  # Add annotations for subplot titles
  if (title==TRUE){
  p1_plotly <- p1_plotly %>%
    layout(
      annotations = list(
        list(
          x = 0.5, y = 1, # Adjust x and y to position the title
          text = title1, # Set the title for subplot 1
          showarrow = FALSE,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          font = list(size = 20)
        )
      )
    )
  
  p2_plotly <- p2_plotly %>%
    layout(
      annotations = list(
        list(
          x = 0.5, y = 1, # Adjust x and y to position the title
          text = title2, # Set the title for subplot 2
          showarrow = FALSE,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          font = list(size = 20)
        )
      )
    )
  }
  # Combine both plots using subplot with shared axes
  myplot <- subplot(p1_plotly, p2_plotly, nrows = 1, margin = 0,shareX = TRUE)
  count<-0
  for (i in 1:length(myplot$x$data)/2){
    if (count == 1){
    myplot$x$data[[i]]$showlegend <- FALSE
    count <- 0
    }else{
      count <- 1
    }
  }
  
  for (i in 1:length(myplot$x$data)) {
    if (!is.null(myplot$x$data[[i]]$name)) {
        interval <- gsub(".*\\((\\[.*?\\)),.*", "\\1", myplot$x$data[[i]]$name)
      }
      myplot$x$data[[i]]$name <- interval
  }
  for (i in 1:length(myplot$x$data)) {
    if (!is.null(myplot$x$data[[i]]$name)) {
      interval <- gsub(".*\\((\\[.*?\\]),.*", "\\1", myplot$x$data[[i]]$name)
    }
    myplot$x$data[[i]]$name <- interval
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
  
  a <- max(max(newdata2$Time),max(newdata22$Time))
  b <- min(min(newdata2$Time),min(newdata22$Time))
  c <- max(max(newdata2$Best),max(newdata22$Best))
  d <- min(min(newdata2$Best),min(newdata22$Best))
  e <- min(length(newdata2$Time),length(newdata22$Time))
  f <- min(length(newdata2$Best),length(newdata22$Best))
  x_padding = (a-b)/e
  y_padding = (c-d)/f
  min_x <- b- x_padding
  max_x <- a + x_padding
  min_y <- d-y_padding
  max_y <- c + y_padding
  
  if (type=="area"){
    
    myplot <- myplot %>%
      layout(
        xaxis = list(title =xlabel,autorange=FALSE,range=c(min_x,max_x)),
        yaxis = list(title =ylabel,autorange=FALSE,range=c(min_y,max_y)),
        yaxis2 = list(title =ylabel,matches="y", autorange=FALSE,range=c(min_y,max_y),side = "right"),
        xaxis2 = list(title =xlabel,matches="x", autorange=FALSE,range=c(min_x,max_x),side = "top")
      )
  }else{
    myplot <- myplot %>%
      layout(
        yaxis2 = list(side = "right"),
        xaxis2 = list(side = "top")
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
extdata_dir <- system.file(package="moocore", "extdata")
A1 <- read_datasets(file.path(extdata_dir, "ALG_1_dat.xz"))
A2 <- read_datasets(file.path(extdata_dir, "ALG_2_dat.xz"))
interactiveeafdiffplot(x=A1,y=A2,maximise = FALSE,type = "area",legend.pos = "top",xlabel = "one",
                   ylabel = "two",title=TRUE,title1="hi",title2="bye")
