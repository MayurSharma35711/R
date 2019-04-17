#data <- read.csv(file.choose())
View(data)
open = data$Open
high = data$High
low = data$Low
close = data$Close
dates = data$Date

x = data$High

#change_setting <- function(){
#par(col.points = "blue")
par(col.lab="red")

plot(dates,x,plotTitle,ylim=c(min(x),max(x)))
title("AMZN Stock","", "Date","Stock Price")

#plot(x<-rnorm(15),y<-rnorm(15))

pnts <- identify(dates, x, plot = F)
points(dates[pnts],x[pnts],col="blue")
days = 28
for(p in pnts){
  print(dates[p])
  print(x[p])
  if(p>=days){
    print(x[p] > mean(x[(p-days):p]))
    print(mean(x[(p-days):p]))
    for(n in 1:days)
    {
      points(dates[p-n],x[p-n],col="red")
    }
  }
  else{
    print(x[p]>mean(x[1:p]))
    print(mean(x[1:p]))
  }
}
