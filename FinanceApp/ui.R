# ui.R

# Author: Rosie Schmitz

# Preamble ----
source("global.R") # Load global variables
source("packages.R") # Load packages
packages <- packages()
lapply(packages, require, character.only = TRUE)

# height of figures
figure.height <- '600px'
loading.spinner.color <<- '#82DA9F'

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
                  tags$head(
                    tags$style(HTML(
                      ".skin-black .sidebar-menu > li.active > a,
                      .skin-black .sidebar-menu > li:hover > a
                      {
                        border-left-color: #5f939a;
                        background-color: #5f939a;
                      }"
                    ))
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
                  menuItem('Parameters',
                           tabName = 'ParametersTab',
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
                             numericInput(
                               inputId = "p1_age",
                               label = "Enter Age:",
                               value = 25,
                               min = 18,
                               max = 120,
                               step = 1,
                               width = NA
                             ),
                             
                             numericInput(
                               inputId = "p1_salary",
                               label = "Enter Salary $:",
                               value = 45000,
                               min = 1000,
                               max = 1000000,
                               step = 1,
                               width = NA
                             ) 
                           ),
                           
                           conditionalPanel(
                             condition = "input.household_count == 'Two Person'",
                             
                             # Inputs for Two Person Household
                             numericInput(
                               inputId = "p1_age",
                               label = "Enter Person 1 Age:",
                               value = 26,
                               min = 18,
                               max = 120,
                               step = 1,
                               width = NA
                             ),
                             
                             numericInput(
                               inputId = "p2_age",
                               label = "Enter Person 2 Age:",
                               value = 29,
                               min = 18,
                               max = 120,
                               step = 1,
                               width = NA
                             ),
                             numericInput(
                               inputId = "p1_salary",
                               label = "Enter Person 1 Salary:",
                               value = 45000,
                               min = 1000,
                               max = 1000000,
                               step = 1,
                               width = NA
                             ),
                             numericInput(
                               inputId = "p2_salary",
                               label = "Enter Person 2 Salary:",
                               value = 55000,
                               min = 1000,
                               max = 1000000,
                               step = 1,
                               width = NA
                             ) 
                           )
                  )
      )
    ),
    
    # Body ----
    dashboardBody(
      useShinyjs(),
      tags$style(type = 'text/css', '.modal-dialog {width: fit-content !important;}'),
      
      # Tabs
      tabItems(
        # Budget Quick Look Tab
        tabItem(
          tabName = 'budgetTab',
          h2('Budget Quick Look'),
          "Page is Work in Progress"
        ),
        
        # Investment Breakdown Tab
        tabItem(
          tabName = 'investmentTab',
          h2('Investment Breakdown'),
          "Page is Work in Progress"
        ),
        
        # Retirement Analysis Tab
        tabItem(
          tabName = 'retirementTab',
          h2('Retirement Analysis'),
          # textOutput("limit_401k_display"),
          # textOutput("limit_roth_ira_display"),
          # textOutput("limit_roth_ira_catchup_display"),
          # textOutput("limit_hsa_one_display"),
          # textOutput("limit_hsa_two_display"),
          # textOutput("limit_hsa_one_55_display"),
          # textOutput("limit_hsa_two_55_display"),
          
          # Inputs for Retirement Analysis
          # Inputs for Person 1
          fluidRow(
            column(width = 2,
                   numericInput(
                     inputId = "p1_wagegrowth",
                     label = "Enter Person 1 Wage Growth Rate %:",
                     value = 3,
                     min = 0,
                     max = 100,
                     step = 0.5,
                     width = NA
                   )
            ),
            column(width = 2,
                   numericInput(
                     inputId = "p1_emp_match",
                     label = "Enter Person 1 Employer Matching %:",
                     value = 5,
                     min = 0,
                     max = 100,
                     step = 0.5,
                     width = NA
                   )
            ),
            column(width = 2,
                   numericInput(
                     inputId = "p1_emp_contrib",
                     label = "Enter Person 1 Employee Contributions $:",
                     value = 10000,
                     min = 0,
                     max = calculate_limits(75)$lim401k50,
                     step = 1,
                     width = NA
                   )
            ),
            column(width = 2,
                   numericInput(
                     inputId = "p1_ratereturn",
                     label = "Enter Person 1 Rate of Return %:",
                     value = 6.5,
                     min = 0,
                     max = 1000,
                     step = .1,
                     width = NA
                   )
            ),
            column(width = 2,
                   numericInput(
                     inputId = "p1_roth",
                     label = "Enter Annual Roth Contrib $:",
                     value = 3000,
                     min = 0,
                     max = calculate_limits(75)$limrothira50,
                     step = .1,
                     width = NA
                   )
            ),
            column(width = 2,
                   numericInput(
                     inputId = "p1_hsa",
                     label = "Enter Annual HSA Contrib $ (Person 1):",
                     value = 0,
                     min = 0,
                     max = calculate_limits(75)$limhsaone55,
                     step = .1,
                     width = NA
                   )
            ),
            column(width = 2,
                   numericInput(
                     inputId = "p1_projections",
                     label = "Enter Person 1 Years to Project:",
                     value = 30,
                     min = 0,
                     max = 100,
                     step = 1,
                     width = NA
                   )
            )
          ),
          
          # Conditional panel for Two Person inputs
          conditionalPanel(
            condition = "input.household_count == 'Two Person'",
            fluidRow(
              column(width = 2,
                     numericInput(
                       inputId = "p2_wagegrowth",
                       label = "Enter Person 2 Wage Growth Rate %:",
                       value = 2,
                       min = 0,
                       max = 100,
                       step = 0.5,
                       width = NA
                     )
              ),
              column(width = 2,
                     numericInput(
                       inputId = "p2_emp_match",
                       label = "Enter Person 2 Employer Matching %:",
                       value = 5,
                       min = 0,
                       max = 100,
                       step = 0.5,
                       width = NA
                     )
              ),
              column(width = 2,
                     numericInput(
                       inputId = "p2_emp_contrib",
                       label = "Enter Person 2 Employee Contributions $:",
                       value = 10000,
                       min = 0,
                       max = calculate_limits(75)$lim401k50,
                       step = 1,
                       width = NA
                     )
              ),
              column(width = 2,
                     numericInput(
                       inputId = "p2_ratereturn",
                       label = "Enter Person 2 Rate of Return %:",
                       value = 6.5,
                       min = 0,
                       max = 1000,
                       step = .1,
                       width = NA
                     )
              ),
              column(width = 2,
                     numericInput(
                       inputId = "p2_roth",
                       label = "Enter Annual Roth Contrib $ (Person 2):",
                       value = 3000,
                       min = 0,
                       max = calculate_limits(75)$limrothira50,
                       step = .1,
                       width = NA
                     )
              ),
              column(width = 2,
                     numericInput(
                       inputId = "p2_hsa",
                       label = "Enter Annual HSA Contrib $ (Person 2):",
                       value = 0,
                       min = 0,
                       max = calculate_limits(75)$limhsatwo55,
                       step = .1,
                       width = NA
                     )
              ),
              column(width = 2,
                     numericInput(
                       inputId = "p2_projections",
                       label = "Enter Person 2 Years to Project:",
                       value = 30,
                       min = 0,
                       max = 100,
                       step = 1,
                       width = NA
                     ), 
                    
              )
            )
          ),
          br(),
          textOutput("limit_401k_display"),
          textOutput("limit_roth_ira_display"),
          textOutput("limit_roth_ira_catchup_display"),
          textOutput("limit_hsa_one_display"),
          textOutput("limit_hsa_two_display"),
          textOutput("limit_hsa_one_55_display"),
          textOutput("limit_hsa_two_55_display")
        )
      )
    )
  )
)








