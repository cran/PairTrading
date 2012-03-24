\name{Simple}
\alias{signal}
\alias{Simple}
\title{
Create trading Signal
}
\description{
 Create trading signal by the spread of stock pair. 
 This function provide you the simplest method to create signal
 in the meaning that it just create signals according to whether spread is over a threshold or not.
}
\usage{
Simple(spread, spread.entry)
}
\arguments{
  \item{spread}{the spread of stock pair. an \code{xts} object}
  \item{spread.entry}{Entry level of spread. single scalar.}
}
\details{
This function generates trading singal by spread and it's threshold.
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

#estimate parameters for back test
params <- EstimateParametersHistorically(price.pair, period = 180)

#create & plot trading signals
signal <- Simple(params$spread, 0.05)
}
\keyword{ signal }
\keyword{ Simple }