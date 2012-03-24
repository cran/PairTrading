Return <- function(price.pair, signal.lagged, hedge.ratio.lagged)
{
  #
  signal      <- as.xts(na.omit(cbind(signal.lagged, -1*(signal.lagged))))
  return.pair <- as.xts(na.omit(.return(price.pair, type = "discrete")))
  weight.pair <- as.xts(na.omit(HedgeRatio2Weight(hedge.ratio.lagged)))
  #
  #names(return.pair) <- names(price.pair)
  #names(signal)      <- names(price.pair)
  #names(weight.pair) <- names(price.pair) 
  #as.xts(apply(signal * weight.pair * return.pair, 1, sum) * leverage)
  x <-          as.xts(apply(merge(signal[, 1], weight.pair[, 1], return.pair[, 1], all = FALSE), 1, prod))
  x <- merge(x, as.xts(apply(merge(signal[, 2], weight.pair[, 2], return.pair[, 2], all = FALSE), 1, prod)))

  if(!length(dim(x))){
    xts(rep(NA, nrow(price.pair)), order.by = index(price.pair))
  }else{
    xts(rowSums(x), order.by = index(x))
  }
}
.return <- function(x, type = c("continuous", "discrete"), na.pad = TRUE) 
{
    type <- match.arg(type)
    if (type == "discrete") {
        result <- x/lag(x, na.pad = na.pad) - 1
    }else if (type == "continuous") {
        result <- diff(log(x), na.pad = na.pad)
    }
    return(result)
}