#data <- read.csv(file.choose())
View(data)
open = data$Open
high = data$High
low = data$Low
close = data$Close
dates = data$Date

x = data$Open
days = 28 #this number may have to be changed for a neural net
comparison <- array(dim=c(2,NROW(x)))
testPtPast <-function(p){# TRUE -> STOCK GROWING
  if(p >= days){
    return(x[p] > mean(x[(p-days):p]))
  }
  else{
    return(x[p] > mean(x[1:p])) 
  }
}
testPtFuture <- function(p){# TRUE -> STOCK GROWING
  if(p<=NROW(x)-days)
    return(x[p] < mean(x[p:(p+days)]))
  else
    return(x[p] < mean(x[p:NROW(x)]))
}
#change_setting <- function(){
#par(col.points = "blue")
par(mfrow=c(3,1))
#---------------------PAST------------------------
par(col.lab="red",col.main="blue")
plot(dates,x,plotTitle,ylim=c(min(x),max(x)))
for(p in 1:NROW(x))
{
  comparison[1, p] = testPtPast(p)
  if(testPtPast(p)){
    points(dates[p],x[p], col="green")
    #print("+")
  }
  else{
    points(dates[p],x[p], col="red")
    #print("-")
  }
}
title("AMZN Stock - Past","", "Date","Stock Price")
#---------------------FUTURE------------------------
par(col.lab="red",col.main="blue")
plot(dates,x,plotTitle,ylim=c(min(x),max(x)))
for(p in 1:NROW(x))
{
  comparison[2, p] = testPtFuture(p)
  if(testPtFuture(p)){
    points(dates[p],x[p], col="blue")
    #print("+")
  }
  else{
    points(dates[p],x[p], col="orange")
    #print("-")
  }
}
title("AMZN Stock - Future","", "Date","Stock Price")
#------------------------COMBO---------------------
par(col.lab="red",col.main="blue")
#print(comparison)
plot(dates,x,plotTitle,ylim=c(min(x),max(x)))
for(p in 1:NROW(x)){
  if(comparison[1,p] == TRUE && comparison[2,p] == TRUE){
    points(dates[p],x[p],col = "green")
    #buy the stock
  }
  else if(comparison[1,p] == TRUE && comparison[2,p] == FALSE){
    points(dates[p], x[p], col="orange")
    #sell the stock
  }
  else if(comparison[1,p] == FALSE && comparison[2,p] == TRUE){
    points(dates[p],x[p], col="blue")
    #buy the stock
  }
  else{
    points(dates[p],x[p],col="red")
    #sell the stock
  }
}
title("AMZN Stock - Comparison","", "Date","Stock Price")


