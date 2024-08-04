# Author: Rosie Schmitz

#' retirement.growth()
#' 
#' @description 
#' returns a dataframe for retirement growth to be used in other places of the app
#' loaded in the main application

# retirementgrowthfunc.R

retirement.growth <- function(age, wagegrowth, emp_match, emp_contrib, ratereturn, roth, hsa, current_year, 
                              current401k, currentroth, currenthsa, salary) {
  
  employer_match_dollar <- emp_match * salary
  salary <- salary * (1 + wagegrowth)
  bal_401k <- (current401k) * (1 + ratereturn) + (employer_match_dollar + emp_contrib)
  bal_roth <- (currentroth) * (1 + ratereturn) + (roth)
  bal_hsa <- (currenthsa) * (1 + ratereturn) + (hsa)
  balance <- bal_401k + bal_roth + bal_hsa #+bal_taxable
  
  df <- data.frame(
    Year = current_year + 1,
    Age = age + 1,
    Salary = salary,
    Employer_Contrib = employer_match_dollar,
    Employee_Contrib = emp_contrib,
    Roth = bal_roth,
    HSA = bal_hsa, 
    `401k` = bal_401k,
    Balance = balance
  )
  return(df)
}

retirement.projection <- function(projection, starting_401k, starting_roth, starting_hsa, starting_salary,
                                  wage_growth, rate_of_return, age, employer_match, employee_contrib, roth_contrib, hsa_contrib) {
  
  wage_growth <- wage_growth/100
  rate_of_return <- rate_of_return/100
  employer_match <- employer_match/100
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  df <- data.frame(
    Year = rep(0, projection),
    Age = rep(0, projection),
    Salary = rep(0, projection),
    Employer_Contrib = rep(0, projection),
    Employee_Contrib = rep(0, projection),
    Roth = rep(0, projection),
    HSA = rep(0, projection), 
    Bal_401k = rep(0, projection),
    Balance = rep(0, projection)
  )
  
  df[1, ] <-retirement.growth(age, wage_growth, employer_match, employee_contrib, rate_of_return, roth_contrib, hsa_contrib, current_year,
                              starting_401k, starting_roth, starting_hsa, starting_salary)
  
  for( i in 2:projection) {
    df[i, ] <-retirement.growth(
      df[i-1, "Age"],
      wage_growth,
      employer_match,
      employee_contrib,
      rate_of_return,
      roth_contrib,
      hsa_contrib,
      df[i-1, "Year"],
      df[i-1, "Bal_401k"],
      df[i-1, "Roth"],
      df[i-1, "HSA"],
      df[i-1, "Salary"]
    )
  }
  
  return(df)
}









