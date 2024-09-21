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
  x@share #we don't need to return x since x is declared and isn't being changed
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

# setting class for Portfolio class ----











