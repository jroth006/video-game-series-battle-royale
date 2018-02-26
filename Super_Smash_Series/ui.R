#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringi)
library(highcharter)
library(reshape2)
library(tidyr)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("slate"),
  
  # Application title
  titlePanel("Super Smash Series Brawl"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,
           imageOutput("left_image")
           ),
    column(4,
       selectizeInput("series_name",
                   "Series:",
                   c("super mario", "zelda", "sonic"),
                   multiple = TRUE,
                   options = list(
                   minItems = 2,
                   maxItems = 2),
                   selected = c("super mario", "zelda"))
    ),
    column(4,
           imageOutput("right_image")
    )
    ),
    
    # Show a plot of the generated distribution
    fluidRow(
      column(1
             ),
      column(5,
             highchartOutput("left_res_chart")
             ),
      column(5,
             highchartOutput("right_res_chart")
      ),
      column(1
      )
    )
  )
)
