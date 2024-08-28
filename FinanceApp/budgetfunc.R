# Author: Rosie Schmitz 

#' budget.output()
#' 
#' @description 
#' returns a dataframe for budget to be used in other places of the app
#' loaded in the main application

# budgetfunc.R

budget.output <- function(rent_mort, groceries, home_ins, home_maint, car_ins, car_maint, 
                          utilities, subscriptions, dining_out, hobbies, savings, investing) {
  
  total <- rent_mort + groceries + home_ins + home_maint + car_ins + car_maint + utilities + subscriptions + dining_out + hobbies + savings + investing
  Needs <- rent_mort + groceries + home_ins + home_maint + car_ins + car_maint + utilities
  Needs_Perc <- round((Needs/total)*100, 2)
  Wants <- subscriptions + dining_out + hobbies
  Wants_Perc <- round((Wants/total)*100, 2)
  Savings_Investing <- savings + investing
  Savings_Investing_Perc <- round((Savings_Investing/total)*100,2)
  
  df <- data.frame(
    'Rent_Mortgage' = rent_mort, 
    'Groceries' = groceries,
    'Home_Renters_Insurance' = home_ins,
    'Home_Maintenance' = home_maint, 
    'Car_Insurance' = car_ins, 
    'Car_Maintenance' = car_maint, 
    'Utilities' = utilities, 
    'Subscriptions' = subscriptions, 
    'Dining_Out' = dining_out,
    'Hobbies' = hobbies, 
    'Savings' = savings,
    'Investing' = investing,
    'Needs' = Needs,
    'Needs_Percentage' = Needs_Perc,
    'Wants' = Wants,
    'Wants_Percentage' = Wants_Perc, 
    'Savings_Investing' = Savings_Investing,
    'Savings_Investing_Percentage' = Savings_Investing_Perc
    
  )
  
  df_renamed <- df %>%
    rename('Needs %' = Needs_Percentage,
           'Wants %' = Wants_Percentage, 
           'Savings/Investing %' = Savings_Investing_Percentage)

  return(df_renamed)
}

