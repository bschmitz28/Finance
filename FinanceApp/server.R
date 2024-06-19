# server.R

# Author: Rosie Schmitz

# Preamble ----
library(shiny)
source("global.R") # Load global variables

# Define server logic required to draw a histogram
# server.R

# Server function
function(input, output, session) {
  
  # Define reactiveValues to store inputs
  rv <- reactiveValues(
    p1_age = 0,
    p2_age = 0
  )
  
  # Update reactive values based on user input
  observe({
    rv$p1_age <- input$p1_age
    rv$p2_age <- input$p2_age
  })
  
  # Calculate limits based on age
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
          ifelse(rv$p1_age >= 55, limits()$p1_limits$limrothira55, limits()$p1_limits$limrothira))
  })
  
  output$limit_hsa_one_display <- renderText({
    paste("Person 1 HSA Contribution Limit (Individual): $", limits()$p1_limits$limhsaone55)
  })
  
  output$limit_hsa_two_display <- renderText({
    paste("Person 1 HSA Contribution Limit (Family): $", limits()$p1_limits$limhsatwo55)
  })
  
  output$limit_hsa_one_55_display <- renderText({
    paste("Person 2 HSA Contribution Limit (Individual, age <= 55): $", limits()$p2_limits$limhsaone55)
  })
  
  output$limit_hsa_two_55_display <- renderText({
    paste("Person 2 HSA Contribution Limit (Family, age <= 55): $", limits()$p2_limits$limhsatwo55)
  })
}
