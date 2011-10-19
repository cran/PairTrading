\name{PairTrading-package}
\alias{PairTrading-package}
\alias{PairTrading}
\docType{package}
\title{
  Classical pair trading methods based on cointegration analysis.
}
\description{
  This package gives classical trading strategy called "Pair trading". You can easily specify pairs for trading and do
  back-testing. Analysis are based on the idea of Cointegration that is a
  statistical feature of time series proposed by Engle and Granger.
}
\details{
\tabular{ll}{
Package: \tab PairTrading\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-10-19\cr
License: \tab BSD \cr
LazyLoad: \tab yes\cr
}
}
\author{
Shinichi Takayanagi, Kohta Ishikawa
Maintainer: Shinichi Takayanagi<teramonagi@gmail.com>, Kohta Ishikawa<quantumcorgi@gmail.com>
}
\references{
Engle, Robert F. and C.W.J. Granger, 1987, Co-integration and error correction: Representation, estimation, and testing, Econometrica 55, 251-276. 
Granger, C.W.J., 1986, Developments in the study of cointegrated economic variables, Oxford Bulletin of Economics and Statistics 48, 213-228.
}
\keyword{ package }
\seealso{
}
\examples{
#load library
library(PairTrading)

#load sample stock price data
data(stock.price)

#select 2 stocks
price.pair <- stock.price[,1:2]["2008-12-31::"]

#Estimate parameters & plot spread
reg <- EstimateParameters(price.pair, method = lm)
str(reg)
plot(reg$spread)

#check stationarity
IsStationary(reg$spread, 0.1)

#estimate parameters for back test
params <- EstimateParametersHistorically(price.pair, period = 180)

#create & plot trading signals
signal <- Simple(params$spread, 0.05)
barplot(signal,col="blue",space = 0, border = "blue",xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(params$spread)

#performance
return.pairtrading <- Return(price.pair, lag(signal), lag(params$hedge.ratio))
if(!all(is.na(return.pairtrading))){
  plot(100 * cumprod(1 + return.pairtrading))
}
}
