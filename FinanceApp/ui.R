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
    dashboardHeader(title = 'Finance App for Budgeting, Investment, and Retirement', titleWidth = '500'),
    
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
                  menuItem('Global Parameters',
                           tabName = 'ParametersTab',
                           icon = icon('users'),
                           selectInput(
                             inputId = "household_count",
                             label = "House Hold Count:",
                             choices = c("One Person", "Two Person"),
                             selected = "One Person"
                           ),
                           
                           #Inputs for One Person Household
                           numericInput(
                             inputId = "p1_age",
                             label = "Person 1 Age:",
                             value = 26,
                             min = 18,
                             max = 120,
                             step = 1,
                             width = NA
                           ),
                           numericInput(
                             inputId = "p1_salary",
                             label = "Person 1 Salary:",
                             value = 45000,
                             min = 1000,
                             max = 1000000,
                             step = 1,
                             width = NA
                           ),
                           
                           conditionalPanel(
                             condition = "input.household_count == 'Two Person'",
                             
                             # Inputs for Two Person Household
                             numericInput(
                               inputId = "p2_age",
                               label = "Person 2 Age:",
                               value = 29,
                               min = 18,
                               max = 120,
                               step = 1,
                               width = NA
                             ),
                             numericInput(
                               inputId = "p2_salary",
                               label = "Person 2 Salary:",
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
      tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
      ),
      tags$style(type = 'text/css', '.modal-dialog {width: fit-content !important;}'),
      tags$style(
      "#p1_wagegrowth-label,
      #p1_emp_match-label,
      #p1_emp_contrib-label,
      #p1_ratereturn-label,
      #p1_roth-label,
      #p1_hsa-label,
      #p1_projections-label,
      #p2_wagegrowth-label,
      #p2_emp_match-label,
      #p2_emp_contrib-label, 
      #p2_ratereturn-label, 
      #p2_roth-label, 
      #p2_hsa-label, 
      #p2_projections-label,
      #p1_current401k-label, 
      #p1_currentroth-label, 
      #p1_currenthsa-label,
      #p2_current401k-label,
      #p2_currentroth-label,
      #p2_currenthsa-label {font-size: 0.75em;}"
      ),
      
      # Tabs for 3 sections ----
      tabItems(
        # Budget Quick Look Tab
        tabItem(
          tabName = 'budgetTab',
          h2('Budget Quick Look'),
          "Page is Work in Progress"
          # add info box
        ),
        
        # Investment Breakdown Tab ----
        tabItem(
          tabName = 'investmentTab',
          h2('Investment Breakdown'),
          "Page is Work in Progress"
          # add info box
        ),
        
        # Retirement Analysis Tab ----
        tabItem(
          tabName = 'retirementTab',
          h2('Retirement Analysis'),
          infoBox(
            title = "Welcome to the Finance App",
            value = "Please use the sidebar to enter number of earners in household and estimated salary per person. Next, fill out the inputs on this tab to view tax vehicle limits as well as retirement growth plot and table",
            icon = icon('comment-dollar'),
            fill = F, 
            width = 12
          ),
          
          # Retirement tab panels
          tabBox(
            title = "Retirement",
            width = 12,
            tabPanel(
              title = strong('Inputs'),
              # p1 column
              
              fluidRow(
                column(width = 6,
                       infoBox("","Person One",
                               icon = icon("user"),
                               width = 5, 
                               color = "light-blue"),
                       numericInput(
                         inputId = "p1_wagegrowth",
                         label = "Wage Growth Rate %:",
                         value = 3,
                         min = 0,
                         max = 100,
                         step = 0.5,
                         width = NA
                       ),
                       numericInput(
                         inputId = "p1_emp_match",
                         label = "Employer Matching %:",
                         value = 5,
                         min = 0,
                         max = 100,
                         step = 0.5,
                         width = NA
                       ),
                       numericInput(
                         inputId = "p1_emp_contrib",
                         label = "401k Employee Contributions $:",
                         value = 10000,
                         min = 0,
                         max = calculate_limits(75)$lim401k50,
                         step = 1,
                         width = NA
                       ),
                       numericInput(
                         inputId = "p1_ratereturn",
                         label = "Rate of Return %:",
                         value = 6.5,
                         min = 0,
                         max = 1000,
                         step = .1,
                         width = NA
                       ),
                       numericInput(
                         inputId = "p1_roth",
                         label = "Annual Roth Contrib $:",
                         value = 3000,
                         min = 0,
                         max = calculate_limits(75)$limrothira50,
                         step = .1,
                         width = NA
                       ),
                       numericInput(
                         inputId = "p1_hsa",
                         label = "Annual HSA Contrib $:",
                         value = 0,
                         min = 0,
                         max = calculate_limits(75)$limhsaone55,
                         step = .1,
                         width = NA
                       ),
                       numericInput(
                         inputId = "p1_projections",
                         label = "Years to Project:",
                         value = 30,
                         min = 0,
                         max = 100,
                         step = 1,
                         width = NA
                       ),
                       # add in current amount 401k
                       numericInput(
                         inputId = "p1_current401k",
                         label = "Current 401k Amount:",
                         value = 10000,
                         min = 0,
                         max = 10000000,
                         step = 0.1,
                         width = NA
                       ),
                       # add in current amount roth 
                       numericInput(
                         inputId = "p1_currentroth",
                         label = "Current Roth Amount:",
                         value = 10000,
                         min = 0,
                         max = 10000000,
                         step = 0.1,
                         width = NA
                       ),
                       # add in current amount hsa
                       numericInput(
                         inputId = "p1_currenthsa",
                         label = "Current HSA Amount:",
                         value = 10000,
                         min = 0,
                         max = 10000000,
                         step = 0.1,
                         width = NA
                       )
                ),
                
                # Column { conditional{ numeric1, numeric2, ...}}
                
                column(width = 6, 
                       conditionalPanel(
                         condition = "input.household_count == 'Two Person'",
                         infoBox("","Person Two",
                                 icon = icon("user-plus"),
                                 width = 5, 
                                 color = "teal"),
                         numericInput(
                           inputId = "p2_wagegrowth",
                           label = "Wage Growth Rate %:",
                           value = 2,
                           min = 0,
                           max = 100,
                           step = 0.5,
                           width = NA
                         ),
                         numericInput(
                           inputId = "p2_emp_match",
                           label = "Employer Matching %:",
                           value = 5,
                           min = 0,
                           max = 100,
                           step = 0.5,
                           width = NA
                         ),
                         numericInput(
                           inputId = "p2_emp_contrib",
                           label = "401k Employee Contribution $:",
                           value = 10000,
                           min = 0,
                           max = calculate_limits(75)$lim401k50,
                           step = 1,
                           width = NA
                         ),
                         numericInput(
                           inputId = "p2_ratereturn",
                           label = "Rate of Return %:",
                           value = 6.5,
                           min = 0,
                           max = 1000,
                           step = .1,
                           width = NA
                         ),
                         numericInput(
                           inputId = "p2_roth",
                           label = "Annual Roth Contrib $:",
                           value = 3000,
                           min = 0,
                           max = calculate_limits(75)$limrothira50,
                           step = .1,
                           width = NA
                         ),
                         numericInput(
                           inputId = "p2_hsa",
                           label = "Annual HSA Contrib $:",
                           value = 0,
                           min = 0,
                           max = calculate_limits(75)$limhsatwo55,
                           step = .1,
                           width = NA
                         ),
                         numericInput(
                           inputId = "p2_projections",
                           label = "Years to Project:",
                           value = 30,
                           min = 0,
                           max = 100,
                           step = 1,
                           width = NA
                         ),
                         # add in current amount 401k
                         numericInput(
                           inputId = "p2_current401k",
                           label = "Current 401k Amount:",
                           value = 10000,
                           min = 0,
                           max = 10000000,
                           step = 0.1,
                           width = NA
                         ),
                         # add in current amount roth 
                         numericInput(
                           inputId = "p2_currentroth",
                           label = "Current Roth Amount:",
                           value = 10000,
                           min = 0,
                           max = 10000000,
                           step = 0.1,
                           width = NA
                         ),
                         # add in current amount hsa
                         numericInput(
                           inputId = "p2_currenthsa",
                           label = "Current HSA Amount:",
                           value = 10000,
                           min = 0,
                           max = 10000000,
                           step = 0.1,
                           width = NA
                         )
                       )
                )
              ),
              
              
              fluidRow(
                
                # Retirement input limit outputs ----
                hr(),
                column(width = 6,
                       textOutput("limit_401k_one_display"),
                       textOutput("limit_roth_ira_one_catchup_display"),
                       textOutput("limit_hsa_one_55_display")
                ),
                column(width = 6,
                       textOutput("limit_401k_two_display"),
                       textOutput("limit_roth_ira_two_catchup_display"),
                       textOutput("limit_hsa_two_55_display")
                )
                
              )
            ), # End Tab Panel
            
            # Retirement growth plot ----
            tabPanel(
              title = strong('Growth Plot'),
              fluidRow(
                column(width = 12,offset = 6,
                       actionButton(
                         inputId = "growthplotbtn",
                         label= "Run Analysis")
                )
              ),
              fluidRow(
                plotlyOutput('retireGrowthPlot')
              )
            ),
            # Retirement growth table ----
            tabPanel(
              title = strong('Growth Table'),
              fluidRow(
                column(width = 12,offset = 6,
                       actionButton(
                         inputId = "growthtablebtn",
                         label= "Show Table")
                )
              ),
              fluidRow(
                DT::dataTableOutput('retireGrowthTbl', height = "50em")
              ) 
            ) # End of Tab Panel
          ) # End Tab Box
        ) # End Tab Item
      ) # End Tab Items
    ) # End Dashboard Body
  ) # End Dashboard Page
) # End Shiny UI

