# This code runs a local query to build a table for use within shiny.  IT WORKS!


# First, run MYSql and start the server:
#   connect rmrgmaster


setwd("D:/Google Drive/RMR/Response Statistics/Stats Dashboard")

library(RMySQL)
mydb <- dbConnect(MySQL(),
                  user='[REDACTED]', 
                  password='[REDACTED]', 
                  dbname='[REDACTED]',
                  host='localhost')

# good reference for queries:
# http://www.r-bloggers.com/accessing-mysql-through-r/

dbListTables(mydb)

query = dbSendQuery(mydb, 'SELECT * FROM opattendance')

opatt = fetch(query, n=-1)

q1 = dbSendQuery(mydb, 
                 'SELECT operation.DATETIME,operation.type,operation.opnumber,operation.YEAR FROM operation')

data = fetch(q1, n=-1)  
# converts dates from strings
data$DATETIME=as.POSIXct(data$DATETIME)
data$YEAR=as.POSIXct(data$YEAR)
write.csv(data,"operations.csv")

q2 = dbSendQuery(mydb, 
                 'SELECT FirstName,LastName,currentRankIdx,operationalPeriod.DATETIME,operation.type,operation.opnumber,operation.YEAR  
                  FROM opattendance
                  INNER JOIN operationalPeriod ON opattendance.opPeriodIdx = operationalPeriod.idx
                 INNER JOIN member ON opattendance.memberIdx = member.idx
                 INNER JOIN operation ON operationalPeriod.operationIdx = operation.idx')

data = fetch(q2, n=-1)  
# converts dates from strings
data$DATETIME=as.POSIXct(data$DATETIME)
data$YEAR=as.POSIXct(data$YEAR)

write.csv(data,"opattendance.csv")

# looks up data types for data
#data[1:5,]
#for(a in 1:length(data[1,])){
#  print(typeof(data[1,a]))
#}

