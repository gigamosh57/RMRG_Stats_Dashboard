##### TO DO
# Put box around "working hours"
# create one for missions vs total response 
#    - calc missions, calc response for both, divide if necessary
# reorganize ggplot labels for x and y
# change colors
# tickbox for type of plot


# dummy inputs for debug
# input = data.frame(dates = c("2000-01-01",as.character(Sys.Date())),busy = c("2000-05-01","2000-09-01"))

library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
setwd("D:/Google Drive/RMR/Response Statistics/Stats Dashboard")
#opattdata = read.csv("opattendance.csv")
runApp(shinyApp(
  ui=(fluidPage(
    
    titlePanel("RMRG Missions by Year"),
    
    sidebarPanel(
      
      dateRangeInput("dates", 
                     "Date range",
                     start = "2000-01-01", 
                     end = as.character(Sys.Date())),
      
      dateRangeInput("busy", 
                     "Busy Season",
                     start = "2000-05-01", 
                     end = "2000-09-01"),
      
      selectInput('type', 
                  'Mission Type', 
                  choices=c("None",as.character(sort(unique(opattdata$type))))),   
      
      selectInput('range', 
                  'Display Range', 
                  choices=c("Whole Year","Busy Season","Off Season")),
      
      selectInput('data', 
                  'Data to Display', 
                  choices=c("Calls","Total Response","Avg Response"))   
      
    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )),
  
  server = (function(input, output) {
    
    yearrange <- reactive({
      
      # Picks type of data to present
      if(input$data == "Total Response"){
        tosubset <- opattdata
      }else if(input$data == "Avg Response"){
        tosubset <- opattdata
      }
      else{tosubset <- operationsdata}
      
      # gets range of busy season (in case people mess with the input bars)
      busyyrst = as.Date(paste(sep="-",format(as.Date(input$busy[1]),"%Y"),1,1))
      busyyrend = as.Date(paste(sep="-",format(as.Date(input$busy[2]),"%Y"),1,1))
      busyst <- as.numeric(as.Date(input$busy[1]) - busyyrst)
      busyend <- as.numeric(as.Date(input$busy[2]) - busyyrend)
      busyrange = (tosubset$DOY >= busyst & tosubset$DOY<=busyend)

      # gets range of entire analysis 
      daterange = as.Date(tosubset$DATETIME)>=as.Date(input$dates[1]) & as.Date(tosubset$DATETIME) <= as.Date(input$dates[2])
      
      seldata <- tosubset[  daterange
                            & if(input$range == "Busy Season"){busyrange}else if(input$range == "Off Season"){!busyrange}else{TRUE}
                            & if(input$type != 'None'){tosubset$type == input$type}else{TRUE},]
      
      
      
      if(input$data == "Calls"){
        # performs aggregation for total response
        sd = count(seldata,c("Weekday","Hour")) 
        
        # calc total by weekday
        daycount <- count(seldata,c("Weekday")) 
        daycount$Hour<-"TOTAL"
        
        rspacer<-daycount
        rspacer$Hour<-" "
        rspacer$freq<-NA
        
        sd <- rbind(daycount,rspacer,sd)
        
        # calc total by Hour bin
        hrcount <- count(seldata,c("Hour")) 
        hrcount$Weekday<-"TOTAL"
        
        cspacer<-hrcount
        cspacer$Weekday<-" "
        cspacer$freq<-NA
        
        sd <- rbind(hrcount,cspacer,sd)
        
        # calc Total overall
        total <- data.frame(freq = length(seldata[,1]),Weekday = "TOTAL",Hour = "TOTAL") 
        # add all total spacers
        tspacer <- data.frame(freq = rep(NA,3),Weekday = c(" "," ","TOTAL"),Hour = c("TOTAL"," "," ")) 
        
        sd <- rbind(sd,tspacer,total)
        
        # Fix factor levels for reordering
        sd$Hour <- factor(sd$Hour,levels(as.factor(sd$Hour))[c(25,26,0:24)])
        sd$Weekday <- factor(sd$Weekday,levels(as.factor(sd$Weekday))[c(5,3,8,9,6,2,4,1,7)])
        
      }else if(input$data == "Total Response"){
        # performs aggregation for total response
        sd = count(seldata,c("Weekday","Hour")) 
        
        # calc total by weekday
        daycount <- count(seldata,c("Weekday")) 
        daycount$Hour<-"TOTAL"
                
        rspacer<-daycount
        rspacer$Hour<-" "
        rspacer$freq<-NA
        
        sd <- rbind(daycount,rspacer,sd)
        
        # calc total by Hour bin
        hrcount <- count(seldata,c("Hour")) 
        hrcount$Weekday<-"TOTAL"
        
        cspacer<-hrcount
        cspacer$Weekday<-" "
        cspacer$freq<-NA
        
        sd <- rbind(hrcount,cspacer,sd)
        
        # calc Total overall
        total <- data.frame(freq = length(seldata[,1]),Weekday = "TOTAL",Hour = "TOTAL") 
        # add all total spacers
        tspacer <- data.frame(freq = rep(NA,3),Weekday = c(" "," ","TOTAL"),Hour = c("TOTAL"," "," ")) 
        
        sd <- rbind(sd,tspacer,total)
        
        # Fix factor levels for reordering
        sd$Hour <- factor(sd$Hour,levels(as.factor(sd$Hour))[c(25,26,0:24)])
        sd$Weekday <- factor(sd$Weekday,levels(as.factor(sd$Weekday))[c(5,3,8,9,6,2,4,1,7)])
      
        
      }else if(input$data == "Avg Response"){
        
        #Aggregates data for calculating means
        sd = count(seldata,c("Weekday","Hour","DATETIME"))   
        
        # precalculates overall average
        total <- data.frame(freq = aggregate(freq~freq, FUN = mean, data=sd),Weekday = "AVERAGE",Hour = "AVERAGE")
        
        # averages # of calls per bin
        sd = aggregate(freq ~ Weekday + Hour, FUN = mean, data=sd)
        
        #calculate row and column means
        
        # calc total by weekday
        daymeans <- aggregate(sd[,"freq"],by=list(sd[,c("Weekday")]),FUN=mean,na.rm=TRUE)
        daymeans$Hour<-"AVERAGE"
        names(daymeans)<-names(sd)[c(1,3,2)]
        
        rspacer<-daymeans
        rspacer$Hour<-" "
        rspacer$freq<-NA
        
        # calc total by Hour bin
        hrmeans <- aggregate(sd[,"freq"],by=list(sd[,c("Hour")]),FUN=mean,na.rm=TRUE)
        hrmeans$Weekday<-"AVERAGE"
        names(hrmeans)<-names(sd)[c(2,3,1)]
        
        cspacer<-hrmeans
        cspacer$Weekday<-" "
        cspacer$freq<-NA
        
        
        sd <- rbind(daymeans,rspacer,sd)
        sd <- rbind(hrmeans,cspacer,sd)
        
              
        # add all total spacers
        tspacer <- data.frame(freq = rep(NA,3),Weekday = c(" "," ","AVERAGE"),Hour = c("AVERAGE"," "," ")) 
        
        sd <- rbind(sd,tspacer,total)
        
        # Fix factor levels for reordering
        sd$Hour <- factor(sd$Hour,levels(as.factor(sd$Hour))[c(25,26,0:24)])#c(26,1,2,3,14,19:25,4:13,15:18)])
        sd$Weekday <- factor(sd$Weekday,levels(as.factor(sd$Weekday))[c(6,4,8,9,7,3,5,1,2)])
        
        
        
        
      }else{
        TRUE
      }
      
      list(plotdata = sd[,c("Weekday","Hour","freq")])
      
    })
    
    output$plot <- renderPlot({
      
      plotdata = yearrange()$plotdata
    
      
      p <- ggplot(plotdata, aes(x = Weekday, y = Hour)) + geom_tile(aes(fill=freq)) +     
        scale_x_discrete(drop=FALSE) + scale_y_discrete(drop=FALSE) +
        scale_fill_continuous( low = "#56b1f7", high = "#F75660", space = "Lab", na.value = "grey50",
                                   guide = "colourbar") + 
        geom_text(data=plotdata, aes(label = ifelse((is.na(freq)),"",round(freq,1))), col="white")
      
        print(p)
      
#      # old plotting code with binning
#       melt_plotdata <- melt(plotdata,id.vars=c("Weekday","Hour"),na.rm = TRUE)
#       
#      str(melt_plotdata) 
#      
#      # plot
#      p <- ggplot(melt_plotdata, aes(x = Weekday, y = Hour)) + geom_bin2d() + 
#        scale_x_discrete(drop=FALSE) + scale_y_discrete(drop=FALSE) +
#        scale_fill_continuous( low = "#56b1f7", high = "#F75660", space = "Lab", na.value = "grey50",
#                              guide = "colourbar")
#      
#      # Get data - this includes counts and x,y coordinates 
#      newdat <- ggplot_build(p)$data[[1]]
#      
#      # add in text labels
#      p <- p + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2, 
#                                     label=count), col="white")
     

      
    }, height=700)
    
  })))

