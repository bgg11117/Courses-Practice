rm(list=ls(all=TRUE))
library("quantmod")
getSymbols("BABA",src="yahoo")
barChart(BABA)
chartSeries(BABA,theme="white")
chartSeries(BABA["2014-05::2016-08"],theme="white")
write.csv(BABA, "BABA.csv")


ma_20<-runMean(BABA[,4],n=20)
addTA(ma_20,on=1,col="blue")
ma_60<-runMean(BABA[,4],n=60)
addTA(ma_60,on=1,col="red")

allDateNumber = length(ma_20)
return = as.vector( c(1:allDateNumber) )
p = as.vector( c(1:allDateNumber) )
for (dateidx in 1:allDateNumber)
{
  if( dateidx < 60 )
  {
    return[dateidx] = 0
  }
  else
  {
    prePrice = as.numeric(BABA[dateidx-1, 4])
    nowPrice = as.numeric(BABA[dateidx, 4])
    return[dateidx] = (prePrice - nowPrice) / prePrice
    if( ma_20[dateidx, 1] < ma_60[dateidx, 1] )
    {
      p[dateidx] = -1
    }
    else if( ma_20[dateidx, 1] > ma_60[dateidx, 1] )
    {
      p[dateidx] = 1
    }
    else
    {
      p[dateidx] = 0
    }
    return[dateidx] = return[dateidx] * p[dateidx]
  }
}

portfolio = cumsum(return)
plot(portfolio,pch=20,col='blue')