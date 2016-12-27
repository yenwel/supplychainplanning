Forecast.Example.Data <- read.csv("Forecast-Example-Data.csv", sep="")
Forecast.Example.Data$NaiveForecast <- c(NA,Forecast.Example.Data$Demand[1:nrow(Forecast.Example.Data)-1])
Forecast.Example.Data$CummulativeMean <- (c(NA,NA,(cumsum(Forecast.Example.Data$Demand)[-1] / (seq_along(Forecast.Example.Data$Demand) + 1)[1:nrow(Forecast.Example.Data)-1])))[1:nrow(Forecast.Example.Data)]

naive <- function(x){c(NA,x[1:length(x)-1])}
naive(Forecast.Example.Data$Demand)
cumean <- function(x){ c(NA,NA,(cumsum(x)[-1] / (seq_along(x[-1]) + 1)))[-length(x)]}#[1:nrow(x)-1])))[1:nrow(x)]}
cumean(Forecast.Example.Data$Demand)
movmean <-  function(x,n=5){c(NA,as.vector(filter(x,rep(1/n,n), sides=1)))}
Supply.Chain.Planning.Quiz.Data.naive <-apply(Supply.Chain.Planning.Quiz.Data,2,naive)

Supply.Chain.Planning.Quiz.Data.cumean <- round(apply(Supply.Chain.Planning.Quiz.Data,2,cumean))

Forecast.Example.Data.MovingAverages <- read.csv("Forecast-Example-Data-MovingAverages.csv", sep="")

ma2 <- round(movmean(Forecast.Example.Data.MovingAverages,2))

ma3 <- round(movmean(Forecast.Example.Data.MovingAverages,3))
ma5 <- round(movmean(Forecast.Example.Data.MovingAverages,5))

#http://alandgraf.blogspot.be/2012/06/rounding-in-r.html

cround = function(x,n = 0){
  vorz = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*vorz
}

Forecast.Example.Data.accuracy <- read.csv("Forecast-Example-Data-accuracy.csv", sep="")

Forecast.Example.Data.accuracy$forecast <- cround(apply(Forecast.Example.Data.accuracy,2,cumean))
apply(E(Forecast.Example.Data.accuracy$Demand, Forecast.Example.Data.accuracy$forecast),2,mean,na.rm=T)
apply(SE(Forecast.Example.Data.accuracy$Demand,  Forecast.Example.Data.accuracy$forecast),2,mean,na.rm=T)
apply(APE(Forecast.Example.Data.accuracy$Demand,  Forecast.Example.Data.accuracy$forecast),2,mean,na.rm=T)


E <- function(x,y){(x-y)} #bias
SE <-function(x,y){(x-y)**2} #accuracy
APE <-function(x,y){abs((x-y)/x)} #accuracy
Supply.Chain.Planning.Quiz.Data.accuracy <-read.csv("~/supplychainforecasting/Chain-Planning-Quiz-Data.csv")
Supply.Chain.Planning.Quiz.Data.accuracy.cumean <- cround(apply(Supply.Chain.Planning.Quiz.Data.accuracy,2,cumean))
Supply.Chain.Planning.Quiz.Data.accuracy.ma3 <- cround(apply(Supply.Chain.Planning.Quiz.Data.accuracy,2,movmean,3))

apply(E(Supply.Chain.Planning.Quiz.Data.accuracy ,  Supply.Chain.Planning.Quiz.Data.accuracy.cumean),2,mean,na.rm=T)
apply(SE(Supply.Chain.Planning.Quiz.Data.accuracy ,  Supply.Chain.Planning.Quiz.Data.accuracy.cumean),2,mean,na.rm=T)
apply(APE(Supply.Chain.Planning.Quiz.Data.accuracy,  Supply.Chain.Planning.Quiz.Data.accuracy.cumean),2,mean,na.rm=T)

apply(E(Supply.Chain.Planning.Quiz.Data.accuracy ,  Supply.Chain.Planning.Quiz.Data.accuracy.ma3),2,mean,na.rm=T)
apply(SE(Supply.Chain.Planning.Quiz.Data.accuracy ,  Supply.Chain.Planning.Quiz.Data.accuracy.ma3),2,mean,na.rm=T)
apply(APE(Supply.Chain.Planning.Quiz.Data.accuracy,  Supply.Chain.Planning.Quiz.Data.accuracy.ma3),2,mean,na.rm=T)

#http://stats.stackexchange.com/questions/44984/how-do-you-use-simple-exponential-smoothing-in-r
require(forecast)
ses(1:40, h=30, alpha=0.1, initial="simple")
