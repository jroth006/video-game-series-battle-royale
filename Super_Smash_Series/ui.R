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


shinyUI(navbarPage(theme = shinytheme("slate"), "Super Smash Series",
  tabPanel("Series Comparison",
  ## Row 1 - Images and selections
  ## Sidebar with a slider input for number of bins 
    fluidRow(
       column(4, align = "center",
           imageOutput("left_image", height = "300px")
           ),
       column(4, align = "center",
          selectizeInput("series_name",
                   "",
                   c("super mario", "zelda", "sonic", "mortal kombat", "halo",
                     "metal gear", "street fighter", "mega man", "pokemon", "kirby"),
                   multiple = TRUE,
                   options = list(
                   minItems = 2,
                   maxItems = 2),
                   selected = c("super mario", "zelda")),
          actionButton("goButton", "Choose 2 series above and click here to compare!")
    ),
        column(4, align = "center",
           imageOutput("right_image", height = "300px")
    )
    ),
    ## Row 2 - Charts
    # Show a plot of the generated distribution
    fluidRow(
      column(1
             ),
      column(4,
             highchartOutput("left_res_chart")
             ),
      column(6,
             highchartOutput("right_res_chart")
      ),
      column(1
      )
    )),
  tabPanel("Score Comparison",
    fluidRow(
      column(2),
      column(8, align = "center",
        selectizeInput("score_diff_series",
                     "Choose series to summarize - multiples welcome!",
                     c("super mario", "zelda", "sonic", "mortal kombat", "halo",
                       "metal gear", "street fighter", "mega man", "pokemon", "kirby"),
                     multiple = TRUE,
                     selected = "super mario")),
      column(2)
    ),
    fluidRow(
      column(12,
        highchartOutput("score_comp_chart", height = "600px"))
    )
  ))
)