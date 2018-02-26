#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

## Load in data
shiny_sales_df <- readRDS("shiny_sales_df.rds")

## Reactivity logic
shinyServer(function(input, output) {
#------------------------------------------------------------#
## Right Chart based on selection  
  output$right_res_chart <- renderHighchart({
    shiny_sales_df %>% 
    filter(`Series Name` == input$series_name[2]) %>% 
      hchart(., "bar", hcaes(x = variable, 
                             y = value,
                             color = variable)) %>% 
      ## Chart settings
      hc_chart(
        backgroundColor = "#272B30",
        style = list(
          fontFamily = "Helvetica")) %>% 
      ## Plot settings
      hc_plotOptions(
        bar = list(
          dataLabels = list(
            align = "right",
            allowOverlap = TRUE,
            enabled = TRUE,
            style = list(
              fontSize = "12px")))) %>% 
      ## Y Axis settings
      hc_yAxis(visible = FALSE) %>% 
      ## X Axis settings
      hc_xAxis(title = list(
        enabled = FALSE),
        labels = list(
          style = list(
            fontSize = "14px"))) %>% 
      ## Tooltip settings
      hc_tooltip(enabled = FALSE) %>%
      ## Theme to adopt some basic settings
      hc_add_theme(hc_theme_darkunica())
  
  })
  
#------------------------------------------------------------#  
  
## Image selection based on selection
  output$right_image <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./www',
                                        paste(input$series_name[2], '.png', sep = '')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         height = 400,
         width = 300)
    
  }, deleteFile = FALSE)
  
  #------------------------------------------------------------#  
  
  ## Right Chart based on selection  
  output$left_res_chart <- renderHighchart({
    shiny_sales_df %>% 
      filter(`Series Name` == input$series_name[1]) %>% 
      hchart(., "bar", hcaes(x = variable, 
                             y = value,
                             color = variable)) %>% 
      ## Chart settings
      hc_chart(
        backgroundColor = "#272B30",
        style = list(
          fontFamily = "Helvetica")) %>% 
      ## Plot settings
      hc_plotOptions(
        bar = list(
          dataLabels = list(
            align = "right",
            allowOverlap = TRUE,
            enabled = TRUE,
            style = list(
              fontSize = "12px")))) %>% 
      ## Y Axis settings
      hc_yAxis(visible = FALSE,
               reversed = TRUE) %>% 
      ## X Axis settings
      hc_xAxis(opposite = TRUE,
               title = list(
                 enabled = FALSE),
               labels = list(
                 enabled = FALSE)) %>%
      ## Tooltip settings
      hc_tooltip(enabled = FALSE) %>%
      ## Theme to adopt some basic settings
      hc_add_theme(hc_theme_darkunica())
    
  })
  
  #------------------------------------------------------------#  
  
  ## Image selection based on selection
  output$left_image <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./www',
                                        paste(input$series_name[1], '.png', sep = '')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         height = 400,
         width = 300)
    
  }, deleteFile = FALSE)
})

