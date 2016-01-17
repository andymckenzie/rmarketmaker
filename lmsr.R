
#'

get_log_cost <- function(liquidity, trades){
  cost = liquidity * log(sum(exp(/liquidity)))
  return(cost)
}

###
get_current_prices <- function(liquidity, net_trades){
  price_each = exp(net_trades/liquidity)
  total_price = sum(exp(net_trades/liquidity))
  normalized_prices = price_each/total_price
  return(normalized_prices)
}
