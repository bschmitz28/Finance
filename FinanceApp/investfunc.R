# Author: Rosie Schmitz 

#' invest.output()
#' 
#' @description 
#' returns a dataframe for budget to be used in other places of the app
#' loaded in the main application
# this is set with the goal to familiarize myself with using s4 classes 
# investfunc.R

 
#setting the investment (sub)class (for larger portfolio later) ----
setClass(
  "Investment", 
  slots = c(
    ticker = "character", 
    share = "numeric", 
    share_price = "numeric", 
    ttl_value = "numeric"
  )
)

#Getter and Setter for Ticker ----
setGeneric("getTicker", function(x) standardGeneric("getTicker"))

setMethod("getTicker", "Investment", function(x){
  x@ticker #we don't need to return x since x is declared and isn't being changed
})

setGeneric("setTicker", function(x, ...) standardGeneric("setTicker"))

setMethod("setTicker", "Investment", function(x, user_ticker){
  x@ticker = user_ticker
  x
})

#Getter and Setter for Share
setGeneric("getShare", function(x) standardGeneric("getShare"))

setMethod("getShare", "Investment", function(x){
  x@share 
})

setGeneric("setShare", function(x, ...) standardGeneric("setShare"))

setMethod("setShare", "Investment", function(x, user_share){
  x@share = user_share
  x
})

#updater for updateSharePrice ----
setGeneric("updateSharePrice", function(x) standardGeneric("updateSharePrice"))

setMethod("updateSharePrice", "Investment", function(x){
  x@share_price = quantmod::getQuote(x@ticker)$Last
  x #we call x here since we want to retrieve the operated upon value
}) 

#updater for total value ----
setGeneric("updateTotalValue", function(x) standardGeneric("updateTotalValue"))

setMethod("updateTotalValue", "Investment", function(x){
  x@ttl_value = x@share*x@share_price
  x
})

# Define print function for investment class 
setGeneric("printInvest", function(x) standardGeneric("printInvest"))

setMethod("printInvest", "Investment", function(x){
  print(paste0(x@share, " share(s) of '", x@ticker, "' at $", x@share_price, ". Total value = $", x@ttl_value))
})

# setting class for Portfolio class ----

setClass(
  "Portfolio", 
  slots = c(
    investment_list = "list",
    ttl_value = "numeric"
  )
)

# Define a function to update all investments 
setGeneric("updatePortfolio", function(x) standardGeneric("updatePortfolio"))

setMethod("updatePortfolio", "Portfolio", function(x){
  for (i in 1:length(x@investment_list)) {
    x@investment_list[[i]] <- updateSharePrice(x@investment_list[[i]])
    x@investment_list[[i]] <- updateTotalValue(x@investment_list[[i]])
  }
  
  cumulative_ttl = 0
  for (investment in x@investment_list) {
    cumulative_ttl = cumulative_ttl + investment@ttl_value
  }
  x@ttl_value = cumulative_ttl
  
  x
}) 

# Define a print function that will print out a portfolio
setGeneric("printPortfolio", function(x) standardGeneric("printPortfolio"))

setMethod("printPortfolio", "Portfolio", function(x){
  x = updatePortfolio(x)
  for (investment in x@investment_list) {
    printInvest(investment)
  }
  print(paste0("Total Portfolio = $", x@ttl_value))
  
}) 

# Define a function to add investment to portfolio and remove investment to portfolio
setGeneric("addInvestToPortfolio", function(x, ...) standardGeneric("addInvestToPortfolio"))

setMethod("addInvestToPortfolio", "Portfolio", function(x, invest){
  #if statement for if it's a valid investment class object
  if(class(invest) != "Investment") {
    warning("The object provided is not of class 'Investment'.")
    return(x)
  }
  
  # check that num of shares and ticker are not null
  if(is.null(invest@share) || is.null(invest@ticker)) {
    warning("Investment must have both 'num_shares' and 'ticker' defined!")
  }
  
  # check that ticker is not already in list
  existing_tickers <- sapply(x@investment_list, function(i) i@ticker) # retrieve a list of existing tickers in investment_list
  if(invest@ticker %in% existing_tickers) {
    warning("Investment with this ticker is already in the portfolio!")
    return(x)
  }
  
  x@investment_list = append(x@investment_list, invest)
  x 
}) 

setGeneric("removeInvestFromPortfolio", function(x, ...) standardGeneric("removeInvestFromPortfolio"))

setMethod("removeInvestFromPortfolio", "Portfolio", function(x, invest){
  #if statement for if it's a valid investment class object 
  if(class(invest) != "Investment") {
    warning("The object provided is not of class 'Investment'.")
    return(x)
  }
  
  # check that num of shares and ticker are not null
  if(is.null(invest@ticker)) {
    warning("Investment must have 'ticker' defined to remove it!")
  }
  
  # check that ticker is not already in list
  existing_tickers <- sapply(x@investment_list, function(i) i@ticker) # retrieve a list of existing tickers in investment_list
  if(!(invest@ticker %in% existing_tickers)) {
    warning("Investment with this ticker is not in the portfolio!")
    return(x)
  }
  tickers_kept <- setdiff(existing_tickers, invest@ticker)
  temp_list <- list()
  
  for (i in tickers_kept) {
    temp_list <- append(temp_list, list(getInvestmentFromPortolio(x, i)))
  }
  x@investment_list <- temp_list
  x 
}) 

#next steps:define 2 func
#get invest from portfolio (pass in ticker and it returns the investment) 
setGeneric("getInvestmentFromPortolio", function(x, ...) standardGeneric("getInvestmentFromPortolio"))
setMethod("getInvestmentFromPortolio", "Portfolio", function(x, ticker){
  for(i in x@investment_list) {
    if(ticker == i@ticker){
      return(i)
    }
  }
  warning("Check again, no matching investment associated with ticker")
  return(NULL)
})
#next func would be replace investment in portfolio (pass in investment)
setGeneric("replaceInvestmentInPortolio", function(x, ...) standardGeneric("replaceInvestmentInPortolio"))
setMethod("replaceInvestmentInPortolio", "Portfolio", function(x, investment){
  existing_tickers <- sapply(x@investment_list, function(i) i@ticker)
  if(!(investment@ticker %in% existing_tickers)) {
    warning("Investment with this ticker is not in the portfolio!")
    return(x)
  }
  x <-removeInvestFromPortfolio(x, investment)
  x <-addInvestToPortfolio(x, investment)
  x
})

# example code 
# testinv1 <- new("Investment")
# testinv2 <- new("Investment")
# 
# testinv1 = setTicker(testinv1, "SCHD")
# testinv2 = setTicker(testinv2, "VOO")
# 
# testinv1 = setShare(testinv1, 25)
# testinv2 = setShare(testinv2, 8)
# 
# testinv1 = updateSharePrice(testinv1)
# testinv2 = updateSharePrice(testinv2)
# 
# testinv1 = updateTotalValue(testinv1)
# testinv2 = updateTotalValue(testinv2)
# 
# printInvest(testinv1)
# printInvest(testinv2)
# 
# 
# 
# testport <- new("Portfolio")
# testport <- addInvestToPortfolio(testport, testinv1)
# testport <- addInvestToPortfolio(testport, testinv2)
# testport <- updatePortfolio(testport)
# 
# testport <- updatePortfolio(testport)
# printPortfolio(testport)
# testreplace <- getInvestmentFromPortolio(testport, "SCHD")
# testreplace <- setShare(testreplace, 20)
# testport <- replaceInvestmentInPortolio(testport, testreplace)
# testrm <- removeInvestFromPortfolio(testport, testinv1) #removing SCHD from testport







