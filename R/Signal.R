Simple <- function(spread, spread.entry)
{
  signal <- ifelse(spread >=   spread.entry, -1, NA)
  signal <- ifelse(spread <=  -spread.entry,  1, signal)
  return(na.locf(signal))
}
SimpleWithTakeProfit <- function(spread, spread.entry, spread.take.profit)
{
  signal <- ifelse(spread >=   abs(spread.entry), -1, 0)
  signal <- ifelse(spread <=  -abs(spread.entry),  1, signal)
  
  take.profit.upper <-  abs(spread.take.profit)
  take.profit.lower <- -take.profit.upper

  #Hit take.profit line : 0
  #other case : continue previous position
  for(i in 2:nrow(signal))
  {
    if(signal[i] == 0){
      if(signal[i - 1] == 1){
        if(spread[i] >= take.profit.lower){
          signal[i] <- 0
        }else{
          signal[i] <- signal[i - 1]
        }
      }else if(signal[i - 1] == -1){
        if(spread[i] <= take.profit.upper){
          signal[i] <- 0
        }else{
          signal[i] <- signal[i - 1]          
        }
      }
    }
  }
  return(signal)
}