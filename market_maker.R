#' Creates a continuous possibility market.
#' By default, all options
#' @param names_outcomes A character vector specifying the names of the possible outcomes in the market.
#' @param initial_probs Optional numeric vector specifying the initial probabilities of the outcomes referred to in the same order as in names_outcomes.
#'
initialize_market <- function(names_outcomes, initial_probs = NULL){
  n_outcomes = length(names_outcomes)
  if(!is.null(initial_probs)){
      probs = initial_probs
  } else {
    probs = rep(1/n_outcomes, n_outcomes)
  }
  names(probs) = names_outcomes
  return(probs)
}

#this is assuming that the baseline is uniform...
get_current_prices <- function(liquidity, net_trades){
  price_each = exp(net_trades/liquidity)
  total_price = sum(exp(net_trades/liquidity))
  normalized_prices = price_each/total_price
  return(normalized_prices)
}




#matrix of net


#current prices
#total trades for each, currently
#function to have people buy shares given current prices
#can let people buy partial shares for simplicity -- this works fine w the exponential
#can initalize with pseudotrades to get the probabilities to where you want them to start...

#each update step
#read in the probabilities assigned by the participant
#read in the current probabilities/prices
#convert those probabilities to the number of shares bought at each option (via division), and store those for later
#also store the total number of currency each participant has put into the pot
#add those shares -- fractional allowed -- to the record for each participant
#use the original probabilities for each participant (summed) to update the prices for the next step

#three objects:
#record of how much currency put into the pot per participant (named vector)
#record of how much (matrix, rows = option names, columns = participant names)
#current prices (named vector)


#'probs is a matrix, with
update_market_step <- function(probs, market, net_shares,
  liquidity, pot_record, shares_record, participants){

  #create a matrix of shares bought based on the current market prices
  shares_matrix = sweep(probs, MARGIN = 2, market, `*`)

  #aggregate the shares matrix across rows for each participant to handle multiple entrie
  shares_matrix = t(sapply(by(M, rownames(M), colSums), identity))

  #sum the shares matrix across rows for all participants to use to update the prices
  shares_total = colSums(shares_matrix)

  #update net shares
  net_shares = net_shares + shares_total

  #update market prices
  market = get_current_prices(liquidity, net_shares)

  #update the number of shares for each participant
  for(i in participants){
    #find the numeric vector of shares for each participant, add it to the record for that participant
    shares_record[rownames(shares_record) %in% i, ] = shares_matrix[rownames(shares_matrix) %in% i, ] +
      shares_record[rownames(shares_record) %in% i, ]
  }

  #update the pot amount for each participant
  for(i in participants){
    n_pots = nrow(probs[, , drop = FALSE])
    pot_record[names(pot_record) %in% i] = pot_record[names(pot_record) %in% i] + n_pots
  }

  return(list(
    market = market,
    net_shares = net_shares,
    pot_record = pot_record,
    shares_record = shares_record))

}

initialize_market(seq(1400, 1600, by = 10))

liquidity_test = 20
net_trades_vec = c(rep(1.5, 5), rep(10, 4), rep(50, 2), rep(10, 4), rep(1, 5))
