# to run: 
#   setwd("D:/Google Drive/RMR/Response Statistics/Stats Dashboard/Final Dashboard")
#   runApp()
#
function(input, output) {
  yearrange <- reactive({
  
  # Picks type of data to present
  if(input$data == "Total Response (# Members at all calls)"){
    tosubset <- opattdata
  }else if(input$data == "Avg Response (Members per Call)"){
    tosubset <- opattdata
  }else if(input$data == "Man-Hours"){
    tosubset <- operationsdata
  }else{tosubset <- operationsdata}
  
  # gets range of busy season (in case people mess with the input bars)
  busyyrst = as.Date(paste(sep="-",format(as.Date(input$busy[1]),"%Y"),1,1))
  busyyrend = as.Date(paste(sep="-",format(as.Date(input$busy[2]),"%Y"),1,1))
  busyst <- as.numeric(as.Date(input$busy[1]) - busyyrst)
  busyend <- as.numeric(as.Date(input$busy[2]) - busyyrend)
  busyrange = (tosubset$DOY >= busyst & tosubset$DOY<=busyend)
  
  # re-subsets data for heatmap to incorporate busy season
  daterange = as.Date(tosubset$DATETIME)>=as.Date(input$dates[1]) & as.Date(tosubset$DATETIME) <= as.Date(input$dates[2])
  
  seldata <- tosubset[  daterange
                       & if(input$range == "Busy Season"){busyrange}else if(input$range == "Off Season"){!busyrange}else{TRUE}
                       & if(input$type != 'All'){tosubset$type == input$type}else{TRUE}
                       & if(input$subtype != 'All'){tosubset$subtype == input$subtype}else{TRUE},]
  
  years <- levels(factor(seldata$YEAR))
  years <- c(as.character(seq(as.Date(years[1]), length=2, by="-1 year")[2]),years)
  cumul <- matrix(0, ncol = length(years), nrow = 366)
  colnames(cumul) <- years
  
  # remove values before today (what is the current day of year)
  maxdoy = as.numeric(as.Date(input$dates[2])-as.Date(paste(sep="","01/01/",format(as.Date(input$dates[2]),"%Y")), format = "%d/%m/%Y"))
  
  for(a in 2:length(years)){
    for(b in 1:(366)){
      selidx = (seldata$YEAR == years[a] & seldata$DOY == b & !is.na(seldata$DOY))
      
      if(b==1){
        if(input$data == "Man-Hours"){
          cumul[b,a] = sum(seldata$hours[selidx],na.rm=TRUE)
        }else{
          cumul[b,a] = length(seldata$DOY[selidx])
        }
        
      }else{ 
        if(input$data == "Man-Hours"){
          cumul[b,a] = cumul[b-1,a] + sum(seldata$hours[selidx],na.rm=TRUE)
        }else{
          cumul[b,a] = cumul[b-1,a] + length(seldata$DOY[selidx])
        }
      }
      
      #don't include dates after the end of the period in the averaging calc
      if(a == length(years) & b >= maxdoy){
        cumul[b,a] = NA
      }
    }
  }
  
  cumul[cumul==0]<-NA
  # calc min max and median
  cumstats = data.frame(median = apply(cumul[,1:(length(cumul[1,])-1)], 1,  median,na.rm=TRUE),
                        max = apply(cumul[,1:(length(cumul[1,])-1)], 1,  max,na.rm=TRUE),
                        mean = apply(cumul[,1:(length(cumul[1,])-1)], 1,  mean,na.rm=TRUE),
                        min = apply(cumul[,1:(length(cumul[1,])-1)], 1,  min,na.rm=TRUE))
  remain = cumul[366,] - cumul[maxdoy-1,] + cumul[maxdoy-1,length(cumul[1,])]
  
  
  ##### BEGIN MISSION RATE PLOT CALCS #####
  years <- c(as.character(seq(as.Date(years[1]), length=2, by="-1 year")[2]),years)
  avgrate <- matrix(0, ncol = length(years), nrow = 366)
  colnames(avgrate) <- years
  
  win = input$win
  
  
  for(a in 2:length(years)){
    for(b in 1:(366)){
      selidx = which((seldata$YEAR == years[a-1] & seldata$DOY >= (-win + 367 + b) & !is.na(seldata$YEAR) & !is.na(seldata$DOY) & !is.na(seldata$hours)) |
              (seldata$YEAR == years[a] & seldata$DOY <= b & seldata$DOY > (b - win) & !is.na(seldata$YEAR) & !is.na(seldata$DOY) & !is.na(seldata$hours)))
      if(input$data == "Man-Hours"){
        avgrate[b,a] = sum(seldata$hours[selidx],na.rm=TRUE)
      }else if(input$data == "Avg Response (Members per Call)"){
        avgrate[b,a] = length(seldata$DOY[selidx])/length(unique(seldata$DATETIME[selidx],na.rm=TRUE))
      }else{
        avgrate[b,a] = length(seldata$DOY[selidx])
      }
      
      #don't include dates after the end of the period in the averaging calc
      if(a == length(years) & b >= maxdoy){
        avgrate[b,a] = NA
      }
    }
  }
  
  
  # calc values for display at week
  if(input$data != "Avg Response (Members per Call)"){
    avgrate = avgrate*7 / win 
  }else{
    avgrate = avgrate
  }
    
  
  # calc min max and median
  avgrate[avgrate==0]<-NA
  avgstats = data.frame(median = apply(avgrate[,1:(length(avgrate[1,])-1)], 1,  median,na.rm=TRUE),
                        max = apply(avgrate[,1:(length(avgrate[1,])-1)], 1,  max,na.rm=TRUE),
                        mean = apply(avgrate[,1:(length(avgrate[1,])-1)], 1,  mean,na.rm=TRUE),
                        min = apply(avgrate[,1:(length(avgrate[1,])-1)], 1,  min,na.rm=TRUE))
  ##### END MISSION RATE PLOT CALCS #####
  
  ##### START HEATMAP PLOT CALCS #####
  
  # re-subsets data for heatmap to incorporate busy season
  daterange = as.Date(tosubset$DATETIME)>=as.Date(input$dates[1]) & as.Date(tosubset$DATETIME) <= as.Date(input$dates[2])
  
  sdheat <- tosubset[  daterange
                       & if(input$range == "Busy Season"){busyrange}else if(input$range == "Off Season"){!busyrange}else{TRUE}
                       & if(input$type != 'All'){tosubset$type == input$type}else{TRUE}
                       & if(input$subtype != 'All'){tosubset$subtype == input$subtype}else{TRUE},]
    
  if(input$data == "Calls"){
    # performs aggregation for total response
    sd = count(sdheat,c("Weekday","Hour")) 
    
    # calc total by weekday
    daysumm <- count(sdheat,c("Weekday")) 
    
    # calc total by Hour bin
    hrsumm <- count(sdheat,c("Hour")) 
    
    # calc Total overall
    total <- data.frame(freq = length(sdheat[,1])) 
    
  }else if(input$data == "Total Response (# Members at all calls)"){
    # performs aggregation for total response
    sd = count(sdheat,c("Weekday","Hour")) 
    
    # calc total by weekday
    daysumm <- count(sdheat,c("Weekday")) 
    
    # calc total by Hour bin
    hrsumm <- count(sdheat,c("Hour")) 
    
    # calc Total overall
    total <- data.frame(freq = length(sdheat[,1])) 
    
  }else if(input$data == "Avg Response (Members per Call)"){
    
    #Aggregates data for calculating means
    sd = count(sdheat,c("Weekday","Hour","DATETIME"))   
    
    # precalculates overall average
    total <- data.frame(freq = aggregate(freq~freq, FUN = mean, data=sd))
    
    # averages # of calls per bin
    sd = aggregate(freq ~ Weekday + Hour, FUN = mean, data=sd)
    
    # calc total by weekday
    daysumm <- aggregate(sd[,"freq"],by=list(sd[,c("Weekday")]),FUN=mean,na.rm=TRUE)
    names(daysumm) <- c("Weekday","freq")
    
    # calc total by Hour bin
    hrsumm <- aggregate(sd[,"freq"],by=list(sd[,c("Hour")]),FUN=mean,na.rm=TRUE) 
    names(hrsumm) <- c("Hour","freq")
    
  }else if(input$data == "Man-Hours"){    
    
    #Aggregates data for calculating means
    sd = aggregate(hours ~ Weekday + Hour, FUN = sum, data=sdheat)
    colnames(sd) = c("Weekday","Hour","freq")
    
    # calc total by weekday
    daysumm <- aggregate(sd[,"freq"],by=list(sd[,c("Weekday")]),FUN=sum,na.rm=TRUE)
    names(daysumm) <- c("Weekday","freq")
    
    # calc total by Hour bin
    hrsumm <- aggregate(sd[,"freq"],by=list(sd[,c("Hour")]),FUN=sum,na.rm=TRUE) 
    names(hrsumm) <- c("Hour","freq")

    
  }else{
    TRUE
  }
  
  
  ##### END HEATMAP PLOT CALCS #####
  
  # OUTPUT TO PLOTS
  list(seldata = seldata, 
       cumul = cumul,cumstats = cumstats,remain = remain,
       avgrate = avgrate,avgstats = avgstats,
       plotdata = sd[,c("Weekday","Hour","freq")],daysumm=daysumm,hrsumm=hrsumm,total=total) 
  
})

output$cumplot <- renderPlot({
  
  cumul <- yearrange()$cumul
  cumstats <- yearrange()$cumstats
  remain <- yearrange()$remain
  
  plot(1:366,cumul[,1],xlim = c(0,375),ylim = c(0,max(c(remain,cumul),na.rm=TRUE)),
       lty = 1,
       xaxt="n",
       xlab=NA,
       ylab = "Cumulative Missions",
       pch=NA)
  
  # polygon of busy season
  left = as.numeric(as.Date(input$busy[1]) - as.Date("01/01/2000", format = "%d/%m/%Y"))
  right = as.numeric(as.Date(input$busy[2]) - as.Date("01/01/2000", format = "%d/%m/%Y"))
  polygon(c(left,left,right,right),c(-1,1,1,-1)*100000,col="grey")
  
  # plot labels
  labDates <- c(as.character(seq(as.Date("01/01/2010", format = "%d/%m/%Y"), as.Date("12/31/2010", format = "%m/%d/%Y"),
                                 by = "months"),format = "%m/%d"),"12/31")
  
  dateloc = c(1,32,60,91,121,152,182,213,244,274,305,335,366)
  
  # background grid
  abline(v = dateloc, col="lightgray",lty="dotted")
  grid(nx=0,ny=NULL)
  
  # Legend
  if(input$show_leg){
    legend(x = 1,y = max(c(remain,cumul),na.rm=TRUE),
           bg="white",
           legend = c("Median","Max","Mean","Min","Current","Historic"),
           col = c("black","red","orange","blue","green","black"),
           lwd = c(4,4,4,4,4,1))
  }
  
  # X axis labels
  axis(side = 1, labDates, at = dateloc,las = 2)
  
  # all lines of previous years
  if(input$show_hist){
    for(a in 2:length(cumul[1,])){
      lines(1:366,cumul[,a])
    }
  }
  
  # stats lines
  if(input$show_median){lines(1:366,cumstats$median,lwd=4,pch=NA)}
  lines(1:366,cumstats$max,lwd=4,pch=NA, col="red")
  lines(1:366,cumstats$min,lwd=4,pch=NA, col= "blue")
  if(input$show_mean){lines(1:366,cumstats$mean,lwd=4,pch=NA, col= "orange")}
  if(input$show_curr){lines(1:366,cumul[,length(cumul[1,])],lwd=4,pch=NA, col= "green")}
  
  # boxplot of forecasted end of year
  if(input$forecast){
    p <- boxplot(remain,
                 add = TRUE,
                 at = 370,
                 boxwex=10)
    
    text(x = rep(380,5),y = p$stats[c(1,3,5)],labels = p$stats[c(1,3,5)])
  }
  
}, height=700)

output$boxbar <- renderPlot({
  
  seldata <- yearrange()$seldata
  
  remain <- yearrange()$remain
  #agg <- data.frame(aggregate(operationsdata$type ~ YEAR, operationsdata, length),stringsAsFactors=FALSE)
  
  if(input$data == "Man-Hours"){
    agg <- aggregate(hours ~ YEAR, FUN = sum, data=seldata)
    
  }else if(input$data == "Avg Response (Members per Call)"){
      
    agg1 <-  aggregate(seldata$type ~ YEAR, FUN = length, data=seldata)
    agg2 <- aggregate(seldata$type ~ YEAR + opnumber , FUN = length, data=seldata)
    
    agg <- aggregate(agg2[,"seldata$type"] ~ YEAR, FUN = mean, data=agg2)
    colnames(agg) <- c("YEAR","count")
  }else{
      
    agg <- data.frame(aggregate(seldata$type ~ YEAR, seldata, length),stringsAsFactors=FALSE)   
  }
  
  # coerce data frame into letting me add another year...
  names(agg)<-c("YEAR","count")
  yearst <- as.Date(paste(sep="-",format(as.Date(input$dates[2]),"%Y"),1,1))
  if(as.Date(agg$YEAR[length(agg$YEAR)]) < yearst){ 
    agg1<- data.frame(YEAR=as.character(yearst),count=as.integer(0))
    agg <- merge(agg,agg1,all.x=TRUE,all.y=TRUE)
  }
  
  # barplot highlighting current year
  cols <- c(rep("lightblue",length(agg$count)-1),"firebrick")
  
  ymax = c(0,1)*max(c(agg$count,if(input$data != "Avg Response (Members per Call)"){remain}else{1}),na.rm=TRUE)*1.15
  
  x <- barplot(agg$count,
               ylim = ymax,
               col=cols)
  
  
  if(input$show_nums){text(x=x, y=agg$count, labels=round(agg$count,0), pos=3, xpd=NA)}
  
  
  
  box()
  # text labels
  
  text(cex=1, x=x, y=-1.25,format(as.Date(agg$YEAR),"%Y"), xpd=TRUE, srt=90, pos=2)
  

if(input$data != "Avg Response (Members per Call)"){
  if(input$forecast){
    # boxplot at max year
    p <- boxplot(remain,
                 add = TRUE,
                 at = max(x)-.25,
                 boxwex=1)
    text(x = rep(max(x)+.25,5),y = p$stats[c(1,3,5)],labels = p$stats[c(1,3,5)])
    
    #remain$count
    
  }
}
  
  # add text on mission counts
  
}, height=700)

output$rateplot <- renderPlot({
  
  avgstats <- yearrange()$avgstats
  avgrate <- yearrange()$avgrate
  
  # initialize plot
  if(input$data != "Avg Response (Members per Call)"){
    ylabel = paste(sep="",input$data," per week")
  }else{
    ylabel = paste(sep="",input$data)
  }
    
  plot(1:366,avgrate[,1],ylim = c(0,max(avgrate,na.rm=TRUE)),
       xlim = c(0,375),
       lty = 1,
       xaxt="n",
       xlab=NA,
       ylab = ylabel,
       pch=NA)
  
  # plot labels
  labDates <- c(as.character(seq(as.Date("01/01/2010", format = "%d/%m/%Y"), as.Date("12/31/2010", format = "%m/%d/%Y"),
                                 by = "months"),format = "%m/%d"),"12/31")
  dateloc = c(1,32,60,91,121,152,182,213,244,274,305,335,365)
  
  # polygon of busy season
  left = as.numeric(as.Date(input$busy[1]) - as.Date("01/01/2000", format = "%d/%m/%Y"))
  right = as.numeric(as.Date(input$busy[2]) - as.Date("01/01/2000", format = "%d/%m/%Y"))
  polygon(c(left,left,right,right),c(-1,1,1,-1)*1000000,col="grey")
  
  # plot background grid
  abline(v = dateloc, col="lightgray",lty="dotted")
  grid(nx=0,ny=NULL)
  
  # Legend
  if(input$show_leg){
    legend(x = 1,y = max(c(avgrate),na.rm=TRUE),
           bg="white",
           legend = c("Median","Max","Mean","Min","Current","Historic"),
           col = c("black","red","orange","blue","green","black"),
           lwd = c(4,4,4,4,4,1))
  }
  
  # X axis labels
  axis(side = 1, labDates, at = dateloc,las = 2)
  
  # all lines of previous years      
  if(input$show_hist){
    for(a in 2:length(avgrate[1,])){
      lines(1:366,avgrate[,a],lwd = 1)
    }
  }
  
  # stats lines
  if(input$show_median){lines(1:366,avgstats$median,lwd=4,pch=NA,col="black")}
  lines(1:366,avgstats$max,lwd=4,pch=NA, col="red")
  lines(1:366,avgstats$min,lwd=4,pch=NA, col= "blue")
  if(input$show_mean){lines(1:366,avgstats$mean,lwd=4,pch=NA, col= "orange")}
  if(input$show_curr){lines(1:366,avgrate[,length(avgrate[1,])],lwd=4,pch=NA, col= "green")}
  
  
}, height=700)

output$heatmap <- renderPlot({
  
  
  plotdata = yearrange()$plotdata
  daysumm=yearrange()$daysumm
  hrsumm=yearrange()$hrsumm
  total=yearrange()$total
  
  plotdata.s <-plotdata[!is.na(plotdata$Weekday)&!is.na(plotdata$Hour),]
  daysumm <- daysumm[!is.na(daysumm$Weekday),]
  hrsumm <- hrsumm[!is.na(hrsumm$Hour),]
  # to make scale continuous    
  #plotdata.s <- plotdata
  plotdata.s$rescale <- rescale(plotdata.s$freq)
  
  daysumm$rescale <- rescale(daysumm$freq)
  hrsumm$rescale <- rescale(hrsumm$freq)
  
  #testing gradient controls
  xycol = c("grey","blue")
  xcol = c("grey","darkorange2")
  ycol = c("grey","firebrick3")
  
  pxy <- ggplot(plotdata.s, aes(x = Weekday, y = Hour,fill=rescale)) + 
    geom_raster() +
    scale_fill_gradientn(colors = xycol,
                         oob      = identity,guide=FALSE) +       
    
    geom_text(data=plotdata.s, aes(label = ifelse((is.na(freq)),"",round(freq,1))), col="white") + 
    
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank())
  
  px <- ggplot(daysumm, aes(y = 1, x = Weekday)) + 
    geom_tile(aes(fill = rescale)) +
    scale_fill_gradientn(colors = xcol,
                         oob      = identity,guide=FALSE) +       
    
    geom_text(data=daysumm, aes(x = Weekday, y = 1,label = ifelse((is.na(freq)),"",round(freq,1))), col="white") + 
    
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank())
  
  py <- ggplot(hrsumm, aes(x = 1, y = Hour)) + 
    geom_tile(aes(fill = rescale)) +
    scale_fill_gradientn(colors = ycol,
                         oob      = identity,guide=FALSE) +       
    
    geom_text(data=hrsumm, aes(x = 1,y = Hour,label = ifelse((is.na(freq)),"",round(freq,1))), col="white") + 
    
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank())
  
  
  layt<-grid.layout(nrow=2,ncol=2,heights=c(13.5/16,1.5/16),widths=c(1/8,7/8),default.units=c('null','null'))
  #View the layout of plots
  grid.show.layout(layt)
  
  #Draw plots one by one in their positions
  grid.newpage()
  pushViewport(viewport(layout=layt))
  print(py,vp=viewport(layout.pos.row=1,layout.pos.col=1))
  print(pxy,vp=viewport(layout.pos.row=1,layout.pos.col=2))
  print(px,vp=viewport(layout.pos.row=2,layout.pos.col=2))
  
}, height=700)
}
