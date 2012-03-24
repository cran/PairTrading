\name{Return}
\alias{Return}
\title{
Calculate return of back-test
}
\description{
Calculate the performance of pair trading by specified trading signal and hedge ratio
}
\usage{
Return(price.pair, signal.lagged, hedge.ratio.lagged)
}
\arguments{
  \item{price.pair}{pair stock price (xts object)}
  \item{signal.lagged}{lagged signal(see details)}
  \item{hedge.ratio.lagged}{hedge ratio created by EstimateParametersHistorically function(see details)}
}
\details{
  You have to consider "lag" to signal, because you can not trade when you calculate your trade position at that time. hedge.ratio.lagged is too.
  We defined "return" as following that [Return of (Buy-Sell)portfolio = (Return of Price1) * (Investmentratio of Price1)  + (Return of Price2) * (Investment ratio of Price2)
  In this equation, as you know, "Return" is calculated as "Change ratio of price between two period".
  We defined "Investment ratio" as following that. [(Investment ratio of Price1) = 1 / (1 + abs(hedge ratio))], [(Investment ratio of Price2) = hedge ratio / (1 + abs(hedge ratio))]
}
\value{
  performance data(not price but return) as xts object
}
\author{
  Shinichi Takayanagi, Kohta Ishikawa
}
\examples{
#load library
library(PairTrading)

#load sample stock price data
data(stock.price)

#select 2 stocks & estimate parameters
price.pair <- stock.price[,1:2]["2008-12-31::"]
params <- EstimateParametersHistorically(price.pair, period = 180)

#create trading signals
signal <- Simple(params$spread, 0.05)

#Performance of pair trading
return.pairtrading <- Return(price.pair, lag(signal), lag(params$hedge.ratio))
if(!all(is.na(return.pairtrading))){
  plot(100 * cumprod(1 + return.pairtrading))
}

}
\keyword{ Return }