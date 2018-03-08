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
          actionButton("goButton", "Choose 2 series above and click here to compare!"),
          tags$br(p("")),
          tags$i(p("Quick tip - click in the box and press delete to clear selections"))
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
                     "Select one or more more series to explore!",
                     c("Super Mario", "Zelda", "Sonic", "Mortal Kombat", "Halo",
                       "Metal Gear", "Street Fighter", "Mega Man", "Pokemon", "Kirby"),
                     multiple = TRUE,
                     selected = "Super Mario")),
      column(4
        # radioButtons("chart_type_input", ("Choose view - combined data or individual"),
        #                          choices = list("1. Combined lines" = "combined", 
        #                                         "2. Individual points" = "scatter"), 
        #                                         selected = "combined")
        )),
    fluidRow(
      column(12, align = "center",
      p("The resulting graph is an average of all selected series."),
      p("For a more in-depth look, switch over to the Data Table tab."),
      tags$b(p("Don't worry - Your selections will transfer over!"))
    )),
    fluidRow(
      column(12,
        highchartOutput("score_comp_chart", height = "550px")))
  ),

#------------------------------------------------------------#
## Third Panel - Data Table

tabPanel("Data Table", 
         fluidRow(
           column(4),
           column(4, align = "center",
                  selectizeInput("table_series",
                                 "Select one or more more series to explore!",
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
),
tabPanel("Analysis", 
           fluidRow(
             column(6,
            
             h1("What does it all mean?"),
                    
             tags$br(h4("1. Critic scores have closely mirrored user scores, until recently.")),
             tags$ul(       
             tags$li(p("When looking at all the series combined, the score differential has remained within +/- 6% since 1998. The score differential has dropped over the last 3 years, hindered by the poorly-received releases of Street Fighter V and Mortal Kombat X."))
             ),
             tags$br(h4("2. Street Fighter and Sonic are declining in score - one with users and the other with critics.")),
             tags$ul(
             tags$li(p("Critic scores for Street Fighter have remained consistent throughout the past 2 decades, but user scores have been decreasing since 2016, resulting in the largest negative score differentials for Street Fighter V (-45% in 2016) and Street Fighter X Tekken (-36% in 2012).")),
         
             tags$li(p("Sonic has been facing harsh critic reviews since 2001, with user scores being higher than the critic scores input 12 out of last 13 years (Sonic Free Riders in 2010 was the outlier)."))
             ),
             tags$br(h4("3. Users support Pokemon despite consistently poor critic scores.")),
             tags$ul(
             tags$li(p("Critic scores have been lower than user scores 60% of the time, and also contain the largest score differential with Pokemon Mystery Dungeon: Explorers of Sky (+35%). Sales for Pokemon games still average 5.25 million units, globally."))
             )),
             column(6,
                    radioButtons("analysis_input", ("Explore the observation from the left:"),
                                 choices = list("1. Critic & User Scores" = "", 
                                                "2a. Street Fighter" = "Street Fighter",
                                                "2b. Sonic" = "Sonic",
                                                "3. Pokemon" = "Pokemon"), 
                                                 selected = ""),
                    highchartOutput("analysis_chart", height = "600px")))),
tabPanel("Music", 
         fluidRow(
           column(4),
           column(4, align = "center",
                  tags$audio(src = "Super_Mario.mp3", type = "audio/mp3", muted = TRUE, 
                             volume = 0.05, autoplay = TRUE, loop = TRUE, controls = TRUE)),
           column(4)))
))