# server.R

# Author: Rosie Schmitz

# Preamble ----
library(shiny)
source("global.R") # Load global variables

# Server function
function(input, output, session) {
  
  # Reactivity
  rv <- reactiveValues(
    p1_age = 0,
    p2_age = 0
  )

  observe({
    rv$p1_age <- input$p1_age
    rv$p2_age <- input$p2_age
  })
  
  limits <- reactive({
    list(
      p1_limits = calculate_limits(rv$p1_age),
      p2_limits = calculate_limits(rv$p2_age)
    )
  })
  
  
  # Render UI outputs
  output$limit_401k_display <- renderText({
    paste("Person 1 401k Contribution Limit: $", limits()$p1_limits$lim401k50)
  })
  
  output$limit_roth_ira_display <- renderText({
    paste("Person 1 Roth IRA Contribution Limit: $", limits()$p1_limits$limrothira50)
  })
  
  output$limit_roth_ira_catchup_display <- renderText({
    paste("Person 1 Roth IRA Catch-Up Contribution Limit: $",
          ifelse(rv$p1_age >= 55, limits()$p1_limits$limrothira50,limrothira))
  })
  
  output$limit_hsa_one_display <- renderText({
    paste("Person 1 HSA Contribution Limit (Individual): $", limits()$p1_limits$limhsaone55)
  })
  
  output$limit_hsa_two_display <- renderText({
    ifelse(input$household_count == "One Person", "", paste("Person 1 HSA Contribution Limit (Family): $", limits()$p1_limits$limhsatwo55))
  })
  
  output$limit_hsa_one_55_display <- renderText({
    ifelse(input$household_count == "One Person", "", paste("Person 2 HSA Contribution Limit (Individual, age <= 55): $", limits()$p2_limits$limhsaone55))
  })
  
  output$limit_hsa_two_55_display <- renderText({
    ifelse(input$household_count == "One Person", "", paste("Person 2 HSA Contribution Limit (Family, age <= 55): $", limits()$p2_limits$limhsatwo55))
  })
}
