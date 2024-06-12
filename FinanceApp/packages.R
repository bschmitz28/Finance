# Author: Rosie Schmitz

#' packages()
#' 
#' @description 
#' returns a list of packages needed for the application to be
#' loaded in the main application
#' 
#' @return vector of packages as strings

packages <- function() {
  return(c('shiny','shinydashboard','shinydashboardPlus','shinyWidgets', 'rsconnect','tidyverse','plotly','DT',
           'RODBC','readxl','reshape2','lubridate','glue','ggrepel','stringr','timeDate','readr',
           'tidyr','odbc','stats','ggthemes','shinyjs','shinyBS','praise','ggridges', 'openxlsx'))
}