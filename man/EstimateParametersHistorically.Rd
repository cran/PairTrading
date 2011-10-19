\name{EstimateParametersHistorically}
\alias{EstimateParametersHistorically}
\title{
Estimate parameters historicall for back test.
}
\description{
  Estimate parameters(spread, premium, cointegration coefficient) at any
  point by regression(default linear regression).
}
\usage{
EstimateParametersHistorically(price.pair, period, method = lm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{price.pair}{pair stock price (xts object)}
  \item{period}{width of rolling window(see details)}
  \item{method}{estimation method.(see details)}
}
\details{
  "method" is any regression method which returns a result with "$coef"
  attribute. "$coef[1]" should be regression intercept(means premium)
  and "$coef[2]" should be regression coefficient(headge.ratio).
}
\value{
  return a list consisting of elements below.
  \item{spread}{xts object. "residual" in regression}
  \item{hedge.ratio}{scalar value. "coefficient" in regression}
  \item{premium}{xts object. "intercept" in regression}
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

#Estimate parameters & plot spread
reg <- EstimateParameters(price.pair, method = lm)
str(reg)
plot(reg$spread)
}
\keyword{ EstimateParametersHistorically }