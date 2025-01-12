# Author: Rosie Schmitz & David Nita

#' retirement.growth()
#' 
#' @description 
#' returns a dataframe for retirement growth to be used in other places of the app
#' loaded in the main application

# retirementgrowthfunc.R

retirement.growth <- function(age, wagegrowth, emp_match, emp_contrib, ratereturn, roth, brokerage, hsa, current_year, 
                              current401k, currentroth, currentbrokerage, currenthsa, salary) {
 
  salary <- salary * (1 + wagegrowth)
  employer_match_dollar <- emp_match * salary
  bal_401k <- (current401k) * (1 + ratereturn) + (employer_match_dollar + emp_contrib)
  bal_roth <- (currentroth) * (1 + ratereturn) + (roth)
  bal_brokerage <- (currentbrokerage) * (1 + ratereturn) + (brokerage)
  bal_hsa <- (currenthsa) * (1 + ratereturn) + (hsa)
  balance <- bal_401k + bal_roth + bal_hsa + bal_brokerage
  
  df <- data.frame(
    Year = current_year + 1,
    Age = age + 1,
    Salary = salary,
    Employer_Contrib = employer_match_dollar,
    Employee_Contrib = emp_contrib,
    Roth = bal_roth,
    Brokerage = bal_brokerage,
    HSA = bal_hsa, 
    `401k` = bal_401k,
    Balance = balance
  )
  
  return(df)
}

retirement.projection <- function(projection, starting_401k, starting_roth, starting_brokerage, starting_hsa, starting_salary,
                                  wage_growth, rate_of_return, age, employer_match, employee_contrib, roth_contrib, brokerage_contrib, hsa_contrib) {
  
  wage_growth <- wage_growth/100
  rate_of_return <- rate_of_return/100
  employer_match <- employer_match/100
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  df <- data.frame(
    Year = rep(0, projection + 1),
    Age = rep(0, projection + 1),
    Salary = rep(0, projection + 1),
    Employer_Contrib = rep(0, projection + 1),
    Employee_Contrib = rep(0, projection + 1),
    Roth = rep(0, projection + 1),
    Brokerage = rep(0, projection +1),
    HSA = rep(0, projection + 1), 
    Bal_401k = rep(0, projection + 1),
    Balance = rep(0, projection + 1)
  )
  # this is initializing the first row (previous year)
  df[1, ] <- retirement.growth(age - 2, 0, 0, 0, 0, 0, 0, 0, current_year - 2,
                               starting_401k, starting_roth, starting_brokerage, starting_hsa, 0) 
  
  
  # this is initializing the second row (current year)
  df[2, ] <- retirement.growth(age - 1, 0, employer_match, employee_contrib, rate_of_return, roth_contrib, brokerage_contrib, hsa_contrib, current_year - 1,
                              starting_401k, starting_roth, starting_brokerage, starting_hsa, starting_salary) 

  
  for( i in 3:(projection + 1)) { #rowwise
    df[i, ] <- retirement.growth(
      df[i-1, "Age"],
      wage_growth,
      employer_match,
      employee_contrib,
      rate_of_return,
      roth_contrib,
      brokerage_contrib,
      hsa_contrib,
      df[i-1, "Year"],
      df[i-1, "Bal_401k"],
      df[i-1, "Roth"],
      df[i-1, "Brokerage"],
      df[i-1, "HSA"],
      df[i-1, "Salary"]
    )
  }
  
  return(df)
}









