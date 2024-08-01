# Author: Rosie Schmitz

#' retirement.growth()
#' 
#' @description 
#' returns a dataframe for retirement growth to be used in other places of the app
#' loaded in the main application

# retirementgrowthfunc.R


retirement.growth <- function(p1_age, p2_age, p1_wagegrowth, p1_emp_match, p1_emp_contrib, p1_ratereturn, p1_roth, p1_hsa, p1_projections, 
                              p1_current401k, p1_currentroth, p1_currenthsa, p2_wagegrowth, p2_emp_match, p2_contrib, p2_ratereturn, 
                              p2_roth, p2_hsa, p2_projections, p2_current401k, p2_currentroth, p2_currenthsa, household_count) {
  
  library(scales) # Load the scales package for formatting
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- seq(current_year, by = 1, length.out = p1_projections)
  p1_balance <- p1_current401k * (1 + p1_wagegrowth / 100)^(years - current_year)
  
  if (household_count == "Two Person") {
    if (!is.null(p2_age) && p2_age != "" && 
        !is.null(p2_wagegrowth) && p2_wagegrowth != "" && 
        !is.null(p2_current401k) && p2_current401k != "") {
      
      p2_balance <- p2_current401k * (1 + p2_wagegrowth / 100)^(years - current_year)
      
      df <- data.frame(
        Year = years,
        P1_Age = p1_age + (years - current_year),
        P1_Balance = dollar(p1_balance, accuracy = 0.01),
        P2_Age = p2_age + (years - current_year),
        P2_Balance = dollar(p2_balance, accuracy = 0.01)
      )
    }
  } else {
    df <- data.frame(
      Year = years,
      P1_Age = p1_age + (years - current_year),
      P1_Balance = dollar(p1_balance, accuracy = 0.01)
    )
  }
  
  return(df)
}
