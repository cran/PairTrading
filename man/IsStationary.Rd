\name{IsStationary}
\alias{IsStationary}
\title{
Check the stationarity of time series (especially spread) 
}
\description{
Check the stationarity of time series by Phillips-Perron Test for Unit Roots and Augmented Dickey-Fuller Test.
}
\usage{
IsStationary(spread, threshold) 
}
\arguments{
  \item{spread}{checked time series object(xts)}
  \item{threshold}{threshhold value of p-value}
}
\value{
  the vector which have the result of each test passed or not.
}
\author{
  Shinichi Takayanagi, Kohta Ishikawa
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

#check stationarity
IsStationary(reg$spread, 0.1)
}
\keyword{ IsStationary }