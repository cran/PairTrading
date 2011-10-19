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

#Performance of pair trading
return.pairtrading <- Return(price.pair, 1, lag(signal), lag(params$hedge.ratio))
plot(100 * cumprod(1 + return.pairtrading))