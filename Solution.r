library(dplyr)
library(timeSeries)
library(TTR)
library(lubridate)
library(zoo)
library(xts)
library(mice)
library(parallel)

setwd("~/OneDrive/Documents/Work")
load("wti.brent.RData")

brent <- select(brent.dataset,time,close)
wti <- select(wti.dataset,time,close)

brent1 <- xts(brent$close, order.by=as.POSIXct(strptime(brent$time,"%Y-%m-%dT%H:%M:%S.000")))
wti1 <- xts(wti$close, order.by=as.POSIXct(strptime(wti$time,"%Y-%m-%dT%H:%M:%S.000")))

lft <- max(time(first(wti1)),time(first(brent1)))
rht <- min(time(last(wti1)),time(last(brent1)))

brent2 <- brent1[time(brent1) %in% seq(lft,rht,by=60)]
wti2 <- wti1[time(wti1) %in% seq(lft,rht,by=60)]

tickDataRaw <- as.xts(merge(brent2,wti2,))
tickDataRaw_Work <- tickDataRaw[wday(time(tickDataRaw))<=5]
tickData <- na.approx(tickDataRaw_Work)
tickData <- tickData[time(tickData)<="2015-03-23 21:14:00"]
colnames(tickData) <- c("brent","wti")

r <- tickData$brent/tickData$wti

mvPeriod <- 10
r0 <- (r-EMA(r,n=mvPeriod))/runSD(r,n=mvPeriod)
plot(r0[1:100],type="l")

thrhd <- 1
sellSig <- -as.integer(r0>thrhd)
buySig <- as.integer(r0< -thrhd)
sig <- sellSig+buySig
sigInTime <- xts(data.frame(signal=sig), order.by=time(tickData))

tradeData <- merge(tickData,sigInTime)

func2 <- function(day0,day1){
  day <- day1
  day0Book <- as.numeric(day0$wti)*as.numeric(day0$pos1)+
    as.numeric(day0$brent)*as.numeric(day0$pos2)
  day1Book <- as.numeric(day1$wti)*as.numeric(day0$pos1)+
    as.numeric(day1$brent)*as.numeric(day0$pos2)
  day$ret <- day1Book-day0Book
  if (as.numeric(day1$signal)==as.numeric(day0$signal)) {
    day$pos1 <- as.numeric(day0$pos1)
    day$pos2 <- as.numeric(day0$pos2)
  } else
  if (as.numeric(day0$signal)-as.numeric(day1$signal)==2) {
      day$pos1 <- day1Book/as.numeric(day1$signal*day1$brent)
      day$pos2 <- -day1Book/as.numeric(day1$signal*day1$wti)
    } else
  if (abs(as.numeric(day1$signal))==1) {
    day$pos1 <- 1/as.numeric(day1$signal*day1$brent)
    day$pos2 <- -1/as.numeric(day1$signal*day1$wti)
  }
  return(day)
}


myData <- tradeData
myData$pos1 <- 0; myData$pos2 <- 0; myData$return <- 0;
myData <- na.omit(myData)
ldf2 <- lapply(as.list(1:dim(myData)[1]), function(x) myData[x[1],])
tvr2 <- Reduce(func2,ldf2,accumulate=TRUE)
tvr3 <- sapply(tvr2,function(x) as.numeric(x$ret))
Reduce(function(x,y) {(1+x)*(1+y)-1},tvr3)
