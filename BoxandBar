##### TO DO
# Implement as 3 separate files
# Add checkbox to turn on and off values above each bar
# Add checkbox for forecasted data
# Better bar colors
# Add data labels for boxplot
# Add checkbox to turn on and off trendline and equation


library(shiny)
library(ggplot2)
setwd("D:/Google Drive/RMR/Response Statistics/Stats Dashboard")
operationsdata = read.csv("operations.csv")
runApp(shinyApp(
  ui=(fluidPage(
    
    titlePanel("RMRG Missions by Year"),
    
    sidebarPanel(
      
      dateRangeInput("dates", 
                     "Date range",
                     start = "2000-01-01", 
                     end = as.character(Sys.Date())),
      
      selectInput('type', 'Mission Type', choices=c("None",as.character(sort(unique(operationsdata$type)))))    
    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )),
   
  server = (function(input, output) {
         
      yearrange <- reactive({
        
        seldata <- operationsdata[as.Date(operationsdata$DATETIME)>=as.Date(input$dates[1]) 
                       & as.Date(operationsdata$DATETIME) <= as.Date(input$dates[2])
                       & if(input$type != 'None'){operationsdata$type == input$type}else{TRUE},]
        list(seldata = seldata)
        
      })

      output$plot <- renderPlot({
        
        seldata <- yearrange()$seldata
        #agg <- data.frame(aggregate(operationsdata$type ~ YEAR, operationsdata, length),stringsAsFactors=FALSE)
        agg <- data.frame(aggregate(seldata$type ~ YEAR, seldata, length),stringsAsFactors=FALSE)

        # coerce data frame into letting me add another year...
        names(agg)<-c("YEAR","count")
        yearst <- as.Date(paste(sep="-",format(as.Date(input$dates[2]),"%Y"),1,1))
        if(as.Date(agg$YEAR[length(agg$YEAR)]) < yearst){ 
          agg1<-data.frame(YEAR=as.character(yearst),count=as.integer(0))
          agg <- merge(agg,agg1,all.x=TRUE,all.y=TRUE)
        }

        # count how many missons happened past the current date in each year
        remain = data.frame(YR = agg$YEAR)
        remain[,"count"] = NA
        
        for(a in 1:length(remain$YR)){
          
          # count diff between current date and end of year
          # sets current date in year of loop
          yeardate = as.Date(paste(sep="-",format(as.Date(remain$YR[a]),"%Y"),format(input$dates[2],"%m"),format(input$dates[2],"%d")))
          # sets end date as end of year in loop
          yearend = as.Date(paste(sep="-",format(as.Date(remain$YR[a]),"%Y"),12,31))
          remain$count[a] = sum(as.Date(seldata$DATETIME) >= yeardate & as.Date(seldata$DATETIME) <= yearend )

        }
        
        # set boxplot values to remaining plus current for plotting
        remain$count = remain$count + agg$count[length(agg$count)]
        remain$count[length(remain$count)] = 0  

        # barplot highlighting current year
        cols <- c(rep("blue",length(agg$count)-1),"red")
        x <- barplot(agg$count,
                ylim = c(0,max(c(agg$count,remain$count)+10)),
                col=cols)
        
        # text labels
        text(cex=1, x=x+.5, y=-1.25,agg$YEAR, xpd=TRUE, srt=45, pos=2)
        
        # boxplot at max year
        boxplot(remain$count,
                add = TRUE,
                at = max(x),
                boxwex=2)

      }, height=700)
      
  })))


