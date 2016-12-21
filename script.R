Forecast.Example.Data <- read.csv("Forecast-Example-Data.csv", sep="")
Forecast.Example.Data$NaiveForecast <- c(NA,Forecast.Example.Data$Demand[1:nrow(Forecast.Example.Data)-1])
Forecast.Example.Data$CummulativeMean <- c(NA,(cumsum(Forecast.Example.Data$Demand) / seq_along(Forecast.Example.Data$Demand))[1:nrow(Forecast.Example.Data)-1])

naive <- function(x){c(NA,x[1:length(x)-1])}
naive(Forecast.Example.Data$Demand)
cumean <- function(x){c(NA,(cumsum(x) / seq_along(x))[1:length(x)-1])}
cumean(Forecast.Example.Data$Demand)
movmean <-  function(x,n=5){c(NA,as.vector(filter(x,rep(1/n,n), sides=1)))}
Supply.Chain.Planning.Quiz.Data.naive <-apply(Supply.Chain.Planning.Quiz.Data,2,naive)

Supply.Chain.Planning.Quiz.Data.cumean <- round(apply(Supply.Chain.Planning.Quiz.Data,2,cumean))

Forecast.Example.Data.MovingAverages <- read.csv("Forecast-Example-Data-MovingAverages.csv", sep="")

ma3 <- round(movmean(Forecast.Example.Data.MovingAverages,3))
ma5 <- round(movmean(Forecast.Example.Data.MovingAverages,5))

Forecast.Example.Data.accuracy <- read.csv("Forecast-Example-Data-accuracy.csv", sep="")
E <- function(x,y){(x-y)} #bias
SE <-function(x,y){(x-y)**2} #accuracy
APE <-function(x,y){abs((x-y)/x)} #accuracy
Supply.Chain.Planning.Quiz.Data.accuracy <- read.csv("Supply-Chain-Planning-Quiz-Data-accuracy.csv")
Supply.Chain.Planning.Quiz.Data.MovingAverages <- read.csv("Supply-Chain-Planning-Quiz-Data-MovingAverages.csv")

apply(SE(Supply.Chain.Planning.Quiz.Data ,  Supply.Chain.Planning.Quiz.Data.cumean),2,mean,na.rm=T)

apply(APE(Supply.Chain.Planning.Quiz.Data ,  Supply.Chain.Planning.Quiz.Data.cumean),2,mean,na.rm=T)
