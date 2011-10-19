#Calculate the spread between two stock prices.
#Assume that log(price) is random walk
#Assume that prices has two column as matrix
EstimateParameters <- function(price.pair, method = lm)
{
  x <- log(price.pair)
  
  reg <- method(x[, 2] ~ x[, 1])
  hedge.ratio <- as.numeric(reg$coef[2])
  premium     <- as.numeric(reg$coef[1])
  spread      <- x[, 2] - (hedge.ratio * x[, 1] + premium)
  list(spread = spread, hedge.ratio = hedge.ratio, premium = premium)
}
EstimateParametersHistorically <- function(price.pair, period, method = lm)
{
  Applied <- function(price.pair){
    reg <- EstimateParameters(price.pair, method)
    c(spread = as.numeric(last(reg$spread)), hedge.ratio = reg$hedge.ratio, premium = reg$premium)
  }
  as.xts(rollapplyr(price.pair, period, Applied, by.column = FALSE))
}
#Return wether spread is stationary or not
IsStationary <- function(spread, threshold)
{
  Is.passed.PP.test  <- PP.test(as.numeric(spread))$p.value <= threshold
  Is.passed.adf.test <- adf.test(as.numeric(spread))$p.value <= threshold
  c(PP.test = Is.passed.PP.test, adf.test = Is.passed.adf.test)
}
HedgeRatio2Weight <- function(hedge.ratio)
{
  hedge.ratio <- abs(hedge.ratio) * (-1)
  #
  normalization.factor <- 1 / (1 + abs(hedge.ratio))
  return(cbind(1 * normalization.factor, hedge.ratio * normalization.factor))
}
