# ui.R

# Author: Rosie Schmitz

# Preamble ----
source("packages.R") # loads the packages in package script
packages = packages()
lapply(packages, require, character.only = TRUE)

# height of figures
figure.height = '600px'
loading.spinner.color <<- '#82DA9F'
#reactlog_enable()

# UI Definition ----
shinyUI(
  dashboardPage(
    skin = 'black',
    
    # Header ----
    dashboardHeader(title = 'Finance App for budgeting, investment, retirement', titleWidth = '500'),
    
    # Sidebar ----
    dashboardSidebar(
        useShinyjs(),
        width = 500,
        sidebarMenu(id = 'sidebarMenu',
                    tags$script(
                      "$(document).on('click', '.sidebar-toggle',
                        function () {
                          Shiny.onInputChange('SideBar_col_react', Math.random())
                        });"
                    ),
                    menuItem(
                      'Budget Quick Look',
                      tabName = 'budgetTab',
                      icon = icon('chart-pie')
                    ),
                    menuItem(
                      'Investment Breakdown',
                      tabName = 'investmentTab',
                      icon = icon('money-bill-trend-up')
                    ),
                    menuItem(
                      'Retirement Analysis',
                      tabName = 'retirementTab',
                      icon = icon('umbrella-beach')
                    ),
                    
                    hr(),
                    
                    # Conditional Panel for House Hold Income
                    menuItem('House Hold Income',
                             tabName = 'householdIncomeTab',
                             icon = icon('users'),
                             selectInput(
                               inputId = "household_count",
                               label = "House Hold Count:",
                               choices = c("One Person", "Two Person"),
                               selected = "One Person"
                             ),
                             
                             conditionalPanel(
                               condition = "input.household_count == 'One Person'",
                               
                               # Inputs for One Person Household
                               selectInput(
                                 inputId = "Person_1_age",          
                                 label = "Enter Age:",
                                 choices = c("18", "19", "20", "21", "22"),  # Example choices
                                 selected = "18"
                               ),
                               
                               # Add other inputs for One Person Household as needed
                             ),
                             
                             conditionalPanel(
                               condition = "input.household_count == 'Two Person'",
                               
                               # Inputs for Two Person Household
                               selectInput(
                                 inputId = "Person_1_age",          
                                 label = "Enter Person 1 Age:",
                                 choices = c("18", "19", "20", "21", "22"),  # Example choices
                                 selected = "18"
                               ),
                               
                               selectInput(
                                 inputId = "Person_2_age",          
                                 label = "Enter Person 2 Age:",
                                 choices = c("18", "19", "20", "21", "22"),  # Example choices
                                 selected = "18"
                               ),
                               
                               # Add other inputs for Two Person Household as needed
                             )
                    )
        )
    ),

    # Body ----
    dashboardBody(
      useShinyjs(),
      tags$style(type = 'text/css',
                 '.modal-dialog {width: fit-content !important;}')
      
    )
  )
)

