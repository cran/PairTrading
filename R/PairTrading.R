Return <- function(price.pair, signal.lagged, hedge.ratio.lagged)
{
  #
  signal      <- na.omit(cbind(signal.lagged, -1*(signal.lagged)))
  return.pair <- na.omit(.return(price.pair, type="discrete"))
  weight.pair <- na.omit(HedgeRatio2Weight(hedge.ratio.lagged))
  #
  names(return.pair) <- names(price.pair)
  names(signal)      <- names(price.pair)
  names(weight.pair) <- names(price.pair)
  # 
  #as.xts(apply(signal * weight.pair * return.pair, 1, sum) * leverage)
  x <- signal * weight.pair * return.pair

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