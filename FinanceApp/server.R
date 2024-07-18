# server.R

# Author: Rosie Schmitz

# Preamble ----
library(shiny)
source("global.R") # Load global variables
source("retirementgrowthfunc.R") # Compute the retirement growth for the table and plot

# Server function ----
function(input, output, session) {
  
  # Reactivity
  rv <- reactiveValues(
    p1_age = 0,
    p2_age = 0
  )

  observe({
    rv$p1_age         = input$p1_age
    rv$p2_age         = input$p2_age
    rv$p1_wagegrowth  = input$p1_wagegrowth
    rv$p1_emp_match   = input$p1_emp_contrib
    rv$p1_emp_contrib = input$p1_emp_contrib
    rv$p1_ratereturn  = input$p1_ratereturn 
    rv$p1_roth        = input$p1_roth
    rv$p1_hsa         = input$p1_hsa
    rv$p1_projections = input$p1_projections
    rv$p1_current401k = input$p1_current401k
    rv$p1_currentroth = input$p1_currentroth
    rv$p1_currenthsa  = input$p1_currenthsa
    rv$p2_wagegrowth  = input$p2_wagegrowth
    rv$p2_emp_match   = input$p2_emp_match
    rv$p2_emp_contrib = input$p2_emp_contrib
    rv$p2_ratereturn  = input$p2_ratereturn
    rv$p2_roth        = input$p2_roth
    rv$p2_hsa         = input$p2_hsa
    rv$p2_projections = input$p2_projections
    rv$p2_current401k = input$p2_current401k
    rv$p2_currentroth = input$p2_currentroth
    rv$p2_currenthsa  = input$p2_currenthsa
  })
  
  # Calculating retirement limits based on age 
  limits <- reactive({
    list(
      p1_limits = calculate_limits(rv$p1_age),
      p2_limits = calculate_limits(rv$p2_age)
    )
  })
  
  # Reactive expression to create the dataframe based on reactive values for retirement growth
  projected_data <- reactive({
    retirement.growth(rv$p1_age, rv$p2_age, rv$p1_wagegrowth, rv$p1_emp_match, rv$p1_emp_contrib, rv$p1_ratereturn, rv$p1_roth, rv$p1_hsa, rv$p1_projections,
                      rv$p1_current401k, rv$p1_currentroth, rv$p1_currenthsa, rv$p2_wagegrowth, rv$p2_emp_match, rv$p2_emp_contrib,rv$p2_ratereturn,
                      rv$p2_roth, rv$p2_hsa, rv$p2_projections, rv$p2_current401k, rv$p2_currentroth, rv$p2_currenthsa
      )
  })
  
  
  # Retirement Input Page ----
  output$limit_401k_one_display <- renderText({
    if(input$household_count %in% c("One Person", "Two Person")) {
      paste("Person 1 401k Contribution Limit: $", limits()$p1_limits$lim401k50)
    } else {
      input$household_count
    }
       
  })
  
  output$limit_401k_two_display <- renderText({
    ifelse(input$household_count == "One Person", "",paste("Person 2 401k Contribution Limit: $", limits()$p2_limits$lim401k50))
  })
  
  output$limit_roth_ira_one_catchup_display <- renderText({
    if(input$household_count %in% c("One Person", "Two Person")) {
      paste("Person 1 Roth IRA Contribution Limit: $", limits()$p1_limits$limrothira50)
    } else {
      ""
    }
      
  })
  
  output$limit_roth_ira_two_catchup_display <- renderText({
    ifelse(input$household_count == "One Person", "", paste("Person 2 Roth IRA Contribution Limit: $", limits()$p2_limits$limrothira50))
  })
  

  output$limit_hsa_one_55_display <- renderText({
    if (input$household_count %in% c("One Person", "Two Person")) {
      paste("Person 1 HSA Contribution Limit (Individual): $", limits()$p1_limits$limhsaone55)
    } else {
      ""
    }
  })
  
  output$limit_hsa_two_55_display <- renderText({
    ifelse(input$household_count == "One Person", "", paste("Person 2 HSA Contribution Limit (Family): $", limits()$p2_limits$limhsatwo55))
  })
}

# Retirement Growth Plot ----
observeEvent(input$growthplotbtn, {
  output$retireGrowthPlot <- renderPlot({
    # ggplot(projected_data(), aes(x = Year, y = Salary)) +
    #   geom_line() +
    #   labs(title = "Salary Projection",
    #        x = "Year",
    #        y = "Projected Salary") +
    #   theme_minimal()
  })
})

# Retirement Growth Table ----
observeEvent(input$growthtablebtn, {
  output$retireGrowthTbl <- DT::renderDataTable({
    #datatable(projected_data())
  })
})
