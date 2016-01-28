setwd("/Users/amckenz/Documents/rmarketmaker/")

#' Creates a continuous possibility market class
#' By default, all options
#' @param names_outcomes A character vector specifying the names of the possible outcomes in the market.
#' @param initial_shares Optional numeric vector specifying the initial shares (and thus probabilities) of the outcomes referred to in the same order as in names_outcomes.
#'
#' @export 
initialize_market <- function(names_outcomes, participants, liquidity,
  initial_shares = NULL){

  if(!is.null(initial_shares)){
      market = get_current_prices(liquidity, initial_shares)
  } else {
    initial_shares = rep(0, length(names_outcomes))
    market = get_current_prices(liquidity, initial_shares)
  }
  names(market) = names_outcomes

  shares_record = matrix(0, nrow = length(participants), ncol = length(names_outcomes))
  colnames(shares_record) = names_outcomes
  rownames(shares_record) = participants

  pot_record = numeric(length(participants))
  names(pot_record) = participants

  market = structure(list(
    market = market,
    liquidity = liquidity,
    shares_record = shares_record,
    pot_record = pot_record),
    class = "market")

  return(market)

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
update_market_step <- function(market, probs){

  #create a matrix of shares bought based on the current market prices
  shares_matrix = sweep(probs, MARGIN = 2, market[["market"]], `*`)
  rownames(shares_matrix) = rownames(probs)

  #sum the shares matrix across rows for all participants to use to update the prices
  shares_total = colSums(shares_matrix)

  #get the net shares from the shares record
  net_shares = colSums(market[["shares_record"]])

  #update net shares
  net_shares = net_shares + shares_total

  #update market prices
  market[["market"]] = get_current_prices(market[["liquidity"]], net_shares)

  shares_record = market[["shares_record"]]

  #update the number of shares for each participant
  for(i in rownames(shares_record)){
    #find the numeric vector of shares for each participant, add it to the record for that participant
    # print(i)
    # print(shares_matrix)
    # print(rownames(shares_matrix))
    # print(head(shares_record))
    # print(rownames(shares_record))
    if(i %in% rownames(shares_matrix)){
      shares_record[rownames(shares_record) %in% i, ] =
        shares_matrix[rownames(shares_matrix) %in% i, ] +
        shares_record[rownames(shares_record) %in% i, ]
    }
  }

  market[["shares_record"]] = shares_record

  pot_record = market[["pot_record"]]

  #update the pot amount for each participant
  for(i in participants){
    n_pots = nrow(probs[, , drop = FALSE])
    pot_record[names(pot_record) %in% i] =
      pot_record[names(pot_record) %in% i] + n_pots
  }

  market[["pot_record"]] = pot_record

  return(market)

}

outcomes = seq(1400, 1600, by = 10)
participants = letters[1:5]
liquidity_test = 20

test_shares = read.table("test_shares.txt", row.names = 1,
  sep = '\t', fill = TRUE)

zeroth_step = initialize_market(outcomes, participants, liquidity = liquidity_test)

first_step = update_market_step(probs = as.matrix(test_shares),
  market = zeroth_step)

second_step = update_market_step(probs = as.matrix(test_shares),
  market = first_step)

third_step = update_market_step(probs = as.matrix(test_shares),
    market = second_step)
