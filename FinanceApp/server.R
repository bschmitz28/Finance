# server.R

# Author: Rosie Schmitz

# Preamble ----
library(shiny)
source("global.R") # Load global variables
source("budgetfunc.R") # Computer budget outputs for pie chart and table
source("retirementgrowthfunc.R") # Compute the retirement growth for the table and plot

# Server function ----
function(input, output, session) {
  
  # Reactivity ----
  rv <- reactiveValues(
    # budget 
    rent_mort = 0,
    groceries = 0, 
    home_ins = 0,
    home_maint = 0,
    car_ins = 0,
    car_maint = 0,
    utilities = 0,
    subscriptions = 0,
    dining_out = 0,
    hobbies = 0,
    savings = 0,
    investing = 0,
    # investment
    
    # retirement
    p1_age = 0,
    p2_age = 0,
    p1_wagegrowth = 0,
    p1_emp_contrib = 0,
    p1_ratereturn = 0,
    p1_roth = 0,
    p1_hsa = 0,
    p1_projections = 0,
    p1_current401k = 0,
    p1_currentroth = 0,
    p1_currenthsa = 0,
    p1_salary = 0,
    p2_wagegrowth = 0,
    p2_emp_contrib = 0,
    p2_ratereturn = 0,
    p2_roth = 0,
    p2_hsa = 0,
    p2_projections = 0,
    p2_current401k = 0,
    p2_currentroth = 0,
    p2_currenthsa = 0,
    p2_salary = 0,
    household_count = "One Person",
  )
  observe({
    rv$rent_mort     <- input$rent_mort
    rv$groceries     <- input$groceries
    rv$home_ins      <- input$home_ins
    rv$home_maint    <- input$car_maint
    rv$car_ins       <- input$car_ins
    rv$car_maint     <- input$car_maint
    rv$utilities     <- input$utilities
    rv$subscriptions <- input$subscriptions
    rv$dining_out    <- input$dining_out
    rv$hobbies       <- input$hobbies
    rv$savings       <- input$savings
    rv$investing     <- input$investing
  })
  observe({
    rv$p1_age          <- input$p1_age
    rv$p2_age          <- input$p2_age
    rv$p1_wagegrowth   <- input$p1_wagegrowth
    rv$p1_emp_match    <- input$p1_emp_match
    rv$p1_emp_contrib  <- input$p1_emp_contrib
    rv$p1_ratereturn   <- input$p1_ratereturn 
    rv$p1_roth         <- input$p1_roth
    rv$p1_hsa          <- input$p1_hsa
    rv$p1_projections  <- input$p1_projections
    rv$p1_current401k  <- input$p1_current401k
    rv$p1_currentroth  <- input$p1_currentroth
    rv$p1_currenthsa   <- input$p1_currenthsa
    rv$p1_salary       <- input$p1_salary
    rv$p2_wagegrowth   <- input$p2_wagegrowth
    rv$p2_emp_match    <- input$p2_emp_match
    rv$p2_emp_contrib  <- input$p2_emp_contrib
    rv$p2_ratereturn   <- input$p2_ratereturn
    rv$p2_roth         <- input$p2_roth
    rv$p2_hsa          <- input$p2_hsa
    rv$p2_projections  <- input$p2_projections
    rv$p2_current401k  <- input$p2_current401k
    rv$p2_currentroth  <- input$p2_currentroth
    rv$p2_currenthsa   <- input$p2_currenthsa
    rv$p2_salary       <- input$p2_salary
    rv$household_count <- input$household_count
  })
  
  # Reactive expression to create dataframe based on reactive values for budget
  budget_data <- reactive({
    df <- budget.output(rv$rent_mort, rv$groceries, rv$home_ins, rv$home_maint, rv$car_ins, rv$car_maint, rv$utilities, rv$subscriptions, 
                        rv$dining_out, rv$hobbies, rv$savings,  rv$investing)
  })
  
  # Reactive expression to create the dataframe based on reactive values for retirement growth
  projected_data <- reactive({
    
    if(input$household_count == "One Person") {
      
      df <- retirement.projection(rv$p1_projections, rv$p1_current401k,  rv$p1_currentroth, rv$p1_currenthsa, rv$p1_salary, rv$p1_wagegrowth, rv$p1_ratereturn,
                                  rv$p1_age, rv$p1_emp_match, rv$p1_emp_contrib, rv$p1_roth, rv$p1_hsa)
      df[,3:9] <- lapply(df[,3:9], dollar,accuracy = 0.01)
      df
      
    } else {
      temp_df1 <- retirement.projection(rv$p1_projections, rv$p1_current401k,  rv$p1_currentroth, rv$p1_currenthsa, rv$p1_salary, rv$p1_wagegrowth, rv$p1_ratereturn,
                                        rv$p1_age, rv$p1_emp_match, rv$p1_emp_contrib, rv$p1_roth, rv$p1_hsa)
      temp_df2 <- retirement.projection(rv$p2_projections, rv$p2_current401k,  rv$p2_currentroth, rv$p2_currenthsa, rv$p2_salary, rv$p2_wagegrowth, rv$p2_ratereturn,
                                        rv$p2_age, rv$p2_emp_match, rv$p2_emp_contrib, rv$p2_roth, rv$p2_hsa)
      
      names(temp_df1)[2:9] <- paste("p1_", names(temp_df1)[2:9], sep ="")
      names(temp_df2)[2:9] <- paste("p2_", names(temp_df2)[2:9], sep ="")
      df <- full_join(temp_df1, temp_df2, by = "Year") #need for taking larger of the two projected years in df (ie someone 24 years and someone 25)
      df$`Total Balance` <- df$p1_Balance + df$p2_Balance
      df[,c(3:9, 11:18)] <- lapply(df[,c(3:9, 11:18)], dollar,accuracy = 0.01)
      df
    }
    
  })
  
  # Calculating retirement limits based on age 
  limits <- reactive({
    list(
      p1_limits = calculate_limits(rv$p1_age),
      p2_limits = calculate_limits(rv$p2_age)
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
  
  # Budget Pie Chart ----
  observeEvent(input$budgetbtn, {
    output$budgetPieChart <- renderPlotly({
      
      data <- budget_data() %>%
        #select(-c('Needs %', 'Wants %', 'Savings/Investing %', 'Needs', 'Wants', 'Savings_Investing')) %>%
        select(c('Needs', 'Wants', 'Savings_Investing')) %>%
        pivot_longer(everything(), 
                     cols_vary = "slowest", 
                     names_to = "Type") 
      colors <- c('#fc8d62', '#8da0cb', '#66c2a5')
      
      fig <- plot_ly(data, labels = ~Type,
                     values = ~value,
                     type = 'pie',
                     hoverinfo = 'label+text',
                     insidetextfont = list(color = '#FFFFFF'),
                     text = ~paste('$', value),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     showlegend = TRUE)
      
      fig <- fig %>% 
        layout(title = 'Monthly Budget Expenditures',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
      fig
      
    })
    output$budgetPieChart2 <- renderPlotly({
      data <- budget_data() %>%
        select(-c('Needs %', 'Wants %', 'Savings/Investing %', 'Needs', 'Wants', 'Savings_Investing')) %>%
        pivot_longer(everything(), 
                     cols_vary = "slowest", 
                     names_to = "Type") 
      #colors <- c('#fc8d62', '#8da0cb', '#66c2a5')
      
      fig <- plot_ly(data, labels = ~Type,
                     values = ~value,
                     type = 'pie',
                     hoverinfo = 'label+text',
                     insidetextfont = list(color = '#FFFFFF'),
                     text = ~paste('$', value),
                     #marker = list(colors = colors,
                     #              line = list(color = '#FFFFFF', width = 1)),
                     showlegend = TRUE)
      
      fig <- fig %>% 
        layout(title = 'Detailed Monthly Budget Expenditures',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
  })
  
  # Budget Data Table ----
  observeEvent(input$budgetbtn, {
    output$budgetTbl <- DT::renderDataTable({
      
      selected_data <- budget_data() %>%
      select(c('Needs %', 'Wants %', 'Savings/Investing %'))
      
      datatable(selected_data,
                rownames = FALSE,
                fillContainer = TRUE,
                options = list(
                  dom = 't',
                  rowCallback = JS(
                    "function(row, data, index) {",
                    "$('td', row).css('height', '50px');",  # Custom row height
                    "}"
                  )
                )
      ) %>%
        formatStyle(
          'Needs %',
          backgroundColor = styleInterval(50, c('#91cf60', '#fc8d59'))
        ) %>%
        formatStyle(
          'Wants %',
          backgroundColor = styleInterval(30, c('#91cf60', '#fc8d59'))
        ) %>%
        formatStyle(
          'Savings/Investing %',
          backgroundColor = styleInterval(20, c('#fc8d59', '#91cf60'))
        )
      
    })
  })
  
  
  
  # Retirement Growth Plot ----
  observeEvent(input$growthplotbtn, {
    output$retireGrowthPlot <- renderPlotly({
      data <- projected_data()  # Get the reactive data frame
      
      if (input$household_count == "One Person") {
        # Convert Balance column to numeric on the fly
        data <- data %>% mutate(Balance = as.numeric(gsub("[$,]", "", Balance)))
        
        p <- ggplot(data, aes(x = Year, y = Balance, group = 1, text= paste("Year: ", Year, "<br>", 
                                                                            "Age: ", Age, "<br>",
                                                                            "Balance: ", dollar(Balance)))) +
          geom_line(color = "#c09bd5") +
          scale_y_continuous(labels = scales::dollar_format()) +
          labs(title = "Projected Balance",
               x = "Year",
               y = "Balance") +
          theme_minimal()
        
      } else {
        # Convert columns to numeric on the fly for two persons scenario
        data <- data %>%
          mutate(p1_Balance = as.numeric(gsub("[$,]", "", p1_Balance)),
                 p2_Balance = as.numeric(gsub("[$,]", "", p2_Balance)),
                 `Total Balance` = as.numeric(gsub("[$,]", "", `Total Balance`)))
        
        p <- ggplot(data, aes(x = Year)) + 
          geom_line(aes(y = p1_Balance, color = "Person 1", group = 1, text= paste("Year: ", Year, "<br>", 
                                                                                   "P1 Age: ", p1_Age, "<br>",
                                                                                   "P1 Balance: ", dollar(p1_Balance)))) + 
          geom_line(aes(y = p2_Balance, color = "Person 2", group = 1, text= paste("Year: ", Year, "<br>", 
                                                                                   "P1 Age: ", p2_Age, "<br>",
                                                                                   "P1 Balance: ", dollar(p2_Balance)))) + 
          geom_line(aes(y = `Total Balance`, color = "Total", group = 1, text= paste("Year: ", Year, "<br>",
                                                                                     "Total Balance: ", dollar(`Total Balance`)))) + 
          scale_y_continuous(labels = scales::dollar_format()) +
          labs(title = "Projected Balance",
               x = "Year",
               y = "Balance",
               color = "Household") +
          theme_minimal()
      }
      
      ggplotly(p, tooltip = c("text"))
    })
  })
  
  
  # Retirement Growth Table ----
  observeEvent(input$growthtablebtn, {
    output$retireGrowthTbl <- DT::renderDataTable({
      if(input$household_count == "One Person") {
        datatable(projected_data(),
                  fillContainer = TRUE, 
                  options = list(pageLength = 50, autoWidth = FALSE)) %>%
          formatStyle("Balance", backgroundColor = "lightblue") %>%
          formatStyle(columns = colnames(.$x$data), `font-size` = '12px')
      } else {
        datatable(projected_data(),
                  fillContainer = TRUE, 
                  options = list(pageLength = 50, autoWidth = FALSE)) %>%
          formatStyle("p1_Balance", backgroundColor = "lightblue") %>% 
          formatStyle("p2_Balance", backgroundColor = "lightblue") %>%
          formatStyle("Total Balance", backgroundColor = "lightyellow") %>%
          formatStyle(columns = colnames(.$x$data), `font-size` = '12px')
      }
      
    })
  })
}
