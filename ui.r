# to run: 
#   setwd("D:/Google Drive/RMR/Response Statistics/Stats Dashboard/Final Dashboard")
#   runApp()
#
fluidPage(
  
  titlePanel("RMRG Stats Dashboard"),
  
  sidebarPanel(
    
    # calculates the calculation period as starting 10 years previously
    
    selectInput('data', 
                'Data to Display', 
                choices=c("Calls",
                          "Total Response (# Members at all calls)",
                          "Avg Response (Members per Call)",
                          "Man-Hours")),
    dateRangeInput("dates", 
                   "Date range",
                   start = as.character(as.Date(paste(sep="-",as.numeric(format(as.Date(Sys.Date()),"%Y"))-10,1,1))), 
                   end = as.character(Sys.Date())),
    
    dateRangeInput("busy", 
                   "Busy Season",
                   start = "2000-05-15", 
                   end = "2000-09-15"),
    
    selectInput('type', 
                'Mission Type', 
                choices=c("All",as.character(sort(unique(opattdata$type))))),

    selectInput('subtype', 
                'Mission Sub-Type', 
                choices=c("All",as.character(sort(unique(opattdata$subtype))))),
    
    checkboxInput("show_nums", "Show Mission Counts (Bar plot)", TRUE),
    
    checkboxInput("forecast", "Show End-of-Year Estimate", TRUE),
    
    checkboxInput("show_curr", "Show Current Year", TRUE), 
    
    checkboxInput("show_mean", "Show Historic Mean", TRUE),
    
    checkboxInput("show_median", "Show Historic Median", FALSE),
    
    checkboxInput("show_hist", "Show Historic Traces", FALSE),
    
    checkboxInput("show_leg", "Show Legend", TRUE),
    
    numericInput("win", "Averaging Window (days)", 30),
    
    selectInput('range', 
                'Display Range (Heatmap)', 
                choices=c("Whole Year","Busy Season","Off Season"))
    
   
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel( "Cumulative Missions", plotOutput("cumplot") ),        
      tabPanel( "Mission Rate", plotOutput("rateplot") ),
      tabPanel( "Annual Total", plotOutput("boxbar") ),
      tabPanel( "Response Heatmap", plotOutput("heatmap") )
    )
  )
)
