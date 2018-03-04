library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringi)
library(highcharter)
library(reshape2)
library(tidyr)
library(shinythemes)
library(DT)



shinyUI(navbarPage(theme = shinytheme("slate"), "Super Smash Series",

#------------------------------------------------------------#  
## First Panel - Series Comparison
  
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
                   c("Super Mario", "Zelda", "Sonic", "Mortal Kombat", "Halo",
                     "Metal Gear", "Street Fighter", "Mega Man", "Pokemon", "Kirby"),
                   multiple = TRUE,
                   options = list(
                   minItems = 2,
                   maxItems = 2),
                   selected = c("Super Mario", "Zelda")),
          actionButton("goButton", "Choose 2 series above and click here to compare!")
    ),
        column(4, align = "center",
           imageOutput("right_image", height = "300px")
    )
    ),
    ## Row 2 - Charts
    # Show a plot of the generated distribution
    fluidRow(
      column(1),
      column(4,
             highchartOutput("left_res_chart")
             ),
      column(6,
             highchartOutput("right_res_chart")
      ),
      column(1)
    )),

#------------------------------------------------------------#
## Second Panel - Score Comparison

  tabPanel("Score Comparison",
    fluidRow(
      column(4),
      column(4, align = "center",
        selectizeInput("score_diff_series",
                     "Choose series to summarize - multiples welcome!",
                     c("Super Mario", "Zelda", "Sonic", "Mortal Kombat", "Halo",
                       "Metal Gear", "Street Fighter", "Mega Man", "Pokemon", "Kirby"),
                     multiple = TRUE,
                     selected = "Super Mario")),
      column(4)
    ),
    fluidRow(
      column(12,
        highchartOutput("score_comp_chart", height = "600px"))
    )
  ),

#------------------------------------------------------------#
## Third Panel - Data Table

tabPanel("Data Table", 
         fluidRow(
           column(4),
           column(4, align = "center",
                  selectizeInput("table_series",
                                 "Choose series to summarize - multiples welcome!",
                                 c("Super Mario", "Zelda", "Sonic", "Mortal Kombat", "Halo",
                                   "Metal Gear", "Street Fighter", "Mega Man", "Pokemon", "Kirby"),
                                 multiple = TRUE,
                                 selected = "Super Mario")
                  ),
           column(4)
         ),
         
         fluidRow(
           column(4, align = "center",
                  sliderInput("year_selection",
                              "Year:",
                              min = 1998,
                              max = 2016,
                              value = c(1998,2016),
                              step = 1,
                              sep = "")
                  ),
           column(4, align = "center",
                  sliderInput("user_score",
                              "User Score:",
                              min = 0.00,
                              max = 100.00,
                              value = c(0.00,100.00),
                              step = 1)
                  ),
         column(4, align = "center",
                sliderInput("critic_score",
                             "Critic Score:",
                             min = 0.00,
                             max = 100.00,
                             value = c(0.00,100.00),
                             step = 1)
                )
         ),
         
         fluidRow(
           column(1),
           column(10, align = "center",
                  dataTableOutput("mytable")
           ),
           column(1)
)
)
))