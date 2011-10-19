\name{EstimateParameters}
\alias{EstimateParameters}
\title{
  Estimate parameters at any point.
}
\description{
  Estimate parameters(spread, premium, cointegration coefficient) at any
  point by regression(default linear regression).
}
\usage{
EstimateParameters(price.pair, method = lm)
}
\arguments{
  \item{price.pair}{pair stock price (xts object)}
  \item{method}{estimation method.(see also)}
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
  \item{premium}{scalar value. "intercept" in regression}
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
\keyword{ EstimateParameters }