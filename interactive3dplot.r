library(mooplot)
library(readr)
library(ggplot2)
library(plotly)
library(eaf)
library(dplyr)
library(moocore)
library(ggthemes)
library(scales)
library(tidyr)
library(tidyverse)
library(akima)
library(rgl)
library(gMOIP)

interactiveeafplot <- function(
    x,
    percentiles=c(0,50,100),
    col=c("white","black"),
    type="point",
    psize=2,
    legend.pos="topright",
    xlabel=NULL,
    ylabel =NULL,
    zlabel=NULL,
    sci.notation=FALSE){
  
  eafdata<-as.data.frame(eaf(x,percentiles = percentiles))
  colnames(eafdata) <- c("x","y","z","Scenario")
  
  labels <- sapply(percentiles, function(pct) {
    if (pct == 0) return("Best")
    if (pct == 50) return("Median")
    if (pct == 100) return("Worst")
    return(paste0(pct, "%"))
  })
  
  eafdata$Scenario <- factor(eafdata$Scenario, levels = percentiles, labels = labels)
  
  uniqueScenario <- unique(eafdata$Scenario)
  
  pal<-colorRampPalette(colors=col,space="Lab")
  if (any(c("white") %in% col)){
    plotcol<-pal(length(uniqueScenario)+1)
  }
  else{
    plotcol<-pal(length(uniqueScenario))
  }
  #make separate plot for each for mesh and cube plots, put together p<-p+p1 in loop
  #PLOTTING
  if (type=="cube"){
    eafdata<-eafdata %>%
      filter(Scenario %in% "Worst")
    
    ini3D(argsPlot3d = list(xlim = c(min(eafdata$x),max(eafdata$x)),
                            ylim = c(min(eafdata$y),max(eafdata$y)),
                            zlim = c(min(eafdata$z),max(eafdata$z))))
    plotPoints3D(eafdata[1:3], argsPlot3d = list(col = plotcol[[2]], size = 5))
    plotCones3D(eafdata[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1, color = plotcol[[1]]),direction = -1)
    #finalize3D(argsAxes3d = list(edges = "bbox"))
    finalize3D()
    }
  
  else{
    if(type=="scatter"){
      myplot<- plot_ly(eafdata,x = ~x, y = ~y, z = ~z,
                       color = ~Scenario,colors=plotcol,opacity=0.8,
                       type = "scatter3d",mode="markers",size = 0.1,name = ~Scenario)
    }
    else{
      # eafdata<-eafdata %>%
      #   filter(Scenario %in% "Worst")
    
      # myplot<- plot_ly(eafdata,x = ~x, y = ~y, z = ~z,
      #                  color = ~Scenario,colors=plotcol,opacity=0.8,
      #                  type = "mesh3d",size = 0.1,name = ~Scenario)
      
      plot_list=vector("list",length(uniqueScenario))
      for (i in seq_along(uniqueScenario)){
        myscene=paste('scene',i,sep = "")
        print(myscene)
        newdata<-eafdata %>%
          filter(Scenario == uniqueScenario[i])
        
        newplot<- plot_ly(newdata,x = ~x, y = ~y, z = ~z,
                         color = ~Scenario,colors=plotcol[i],opacity=0.8,
                         type = "mesh3d",size = 0.1,name = ~Scenario,scene=paste('scene',i,sep = ""))
        newplot<-newplot%>%
          layout(myscene= list(domain = list(x=c(0,1/length(uniqueScenario)),y=c(0,1/length(uniqueScenario))), aspectmode="cube"))
        
        newplot <- newplot %>% layout(title="3d",
          myscene = list(
            xaxis = list(title = ifelse(is.null(xlabel), "Objective 1", xlabel)),
            yaxis = list(title = ifelse(is.null(ylabel), "Objective 2", ylabel)),
            zaxis = list(title = ifelse(is.null(zlabel), "Objective 3", zlabel))
          )
        )
        # newplot<-newplot %>%
        #   layout(title = "Main Plot",
        #          scene= list(domain = list(x=c(0,0.33), y=c(0.66,1)), aspectmode="cube"),
        #          scene2= list(domain = list(x=c(0.66,1), y=c(0.66,1)), aspectmode="cube"),
        #          scene3 = list(domain = list(x=c(0,0.33), y=c(0,0.33)), aspectmode="cube"))
        
        plot_list[[i]]<-newplot
      }
      num_scenarios <- length(uniqueScenario)
      nrows <- floor(num_scenarios/2)
      
      if(type=="mesh+markers")
        myplot<- myplot %>% add_markers()
      
      myplot <- subplot(plot_list, nrows = nrows,shareY = TRUE)
      #myplot<-subplot(plot_list)
    }
    return(myplot)
    }
  }

x <- moocore::read_datasets("spherical-250-10-3d.txt")
# Select only the last 3 datasets
x <- x[x[, 4] >= 8, ]

#functionalities:
#type=scatter (point)
#type=mesh, separate plot for each, grid
#type=mesh+markers
#cube, separate plot for each, grid

interactiveeafplot(x, c(0,50,100), col=c("red","orange","yellow"), type="cube",
                   psize=3, legend.pos="topright", sci.notation=FALSE, 
                   xlabel="MIN X", ylabel="MIN Y", zlabel="MIN Z")
