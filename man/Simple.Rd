\name{Simple}
\alias{signal}
\alias{Simple}
\title{
Create trading Signal
}
\description{
  Create trading signal 
%Create trading signal by the spread of stock pair
}
\usage{
Simple(spread, spread.entry)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spread}{the spread of stock pair. an \code{xts} object}
  \item{spread.entry}{Entry level of spread. single scalar.}
}
\details{
This function generates trading singal by spread and it's threshold.
}
\value{
}
\references{
}
\author{
  Shinichi Takayanagi, Kohta Ishikawa
}
\note{
}
\seealso{
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