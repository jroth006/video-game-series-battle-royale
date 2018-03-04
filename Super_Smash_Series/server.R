## Load in data
shiny_sales_df <- readRDS("shiny_sales_df.rds")
score_diff_df <- readRDS("shiny_diff_df.rds")
merged_sales_df <- readRDS("merged_sales_df.rds")

## Reactivity logic
shinyServer(function(input, output, session) {
  observeEvent(input[["score_diff_series"]],
               {
                 updateSelectInput(session = session,
                                   inputId = "table_series",
                                   selected = input[["score_diff_series"]])
               })
  
  observeEvent(input[["table_series"]],
               {
                 updateSelectInput(session = session,
                                   inputId = "score_diff_series",
                                   selected = input[["table_series"]])
               })
#------------------------------------------------------------#
## Right Chart based on selection  
  output$right_res_chart <- renderHighchart({
    input$goButton
    
    shiny_sales_df %>% 
    filter(`Series Name` == isolate(input$series_name[2])) %>% 
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
               tickInterval = 10,
               max = 100) %>% 
      ## X Axis settings
      hc_xAxis(title = list(
        enabled = FALSE),
        labels = list(
          style = list(
            fontSize = "14px"))) %>% 
      ## Tooltip settings
      hc_tooltip(enabled = FALSE) %>%
      ## Theme to adopt some basic settings
      hc_add_theme(hc_theme_darkunica()) %>%      
      ## Crediting the data source
      hc_credits(enabled = TRUE, 
               text = "Data courtesy of https://www.kaggle.com/kendallgillies/video-game-sales-and-ratings. Collected in Jan 2017.",
               style = list(fontSize = "10px"))
  
  })
  
#------------------------------------------------------------#  
  
## Right image selection based on selection
  output$right_image <- renderImage({
    input$goButton
    
    filename <- normalizePath(file.path('./www',
                                        paste(isolate(input$series_name[2]), '.png', sep = '')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         height = 300,
         width = 200)
    
  }, deleteFile = FALSE)
  
  #------------------------------------------------------------#  
  
  ## Left Chart based on selection  
  output$left_res_chart <- renderHighchart({
    input$goButton
    
    shiny_sales_df %>% 
      filter(`Series Name` == isolate(input$series_name[1])) %>% 
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
               reversed = TRUE,
               tickInterval = 10,
               max = 100) %>% 
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
  
  ## Left image selection based on selection
  output$left_image <- renderImage({
    input$goButton

    filename <- normalizePath(file.path('./www',
                                        paste(isolate(input$series_name[1]), '.png', sep = '')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         height = 300,
         width = 200)
    
  }, deleteFile = FALSE)

#------------------------------------------------------------#  

output$score_comp_chart <- renderHighchart({
  user_selection <- function(chart_input) {
    return(score_diff_df %>% 
             filter(grepl(paste(chart_input, collapse = '|'), series)))
  }
  
  score_diff_df <- user_selection(input$score_diff_series) 
  
  critic_score <- score_diff_df %>% 
    filter(variable == "Avg. Critic Score") %>%
    select(-c(series)) %>% 
    group_by(Year_of_Release) %>% 
    summarise("value" = mean(value)) %>% 
    filter(Year_of_Release != "N/A")
  
  user_score <- score_diff_df %>% 
    filter(variable == "Avg. User Score") %>%
    select(-c(series)) %>% 
    group_by(Year_of_Release) %>% 
    summarise("value" = mean(value)) %>% 
    filter(Year_of_Release != "N/A")
  
  score_differential <- score_diff_df %>% 
    filter(variable == "Score Differential") %>%
    select(-c(series)) %>% 
    group_by(Year_of_Release) %>% 
    summarise("value" = mean(value)) %>% 
    filter(Year_of_Release != "N/A") 
  
  total_sales <- score_diff_df %>% 
    filter(variable == "Total Global Sales") %>%
    select(-c(series)) %>% 
    group_by(Year_of_Release) %>% 
    summarise("value" = mean(value)) %>% 
    filter(Year_of_Release != "N/A")
  
  #---------------------------------------------------------------------------#
  
  score_diff_chart <- highchart() %>% 
    hc_add_series(name = "Critic Score", critic_score$value) %>% 
    hc_add_series(name = "User Score", user_score$value) %>%
    hc_add_series(type = "column", name = "Total Global Sales (Millions)", total_sales$value) %>%
    hc_add_series(name = "Score Differential", score_differential$value) %>%
    ## Chart settings
    hc_chart(
      backgroundColor = "#272B30",
      style = list(
        fontFamily = "Helvetica")) %>% 
    ## Theme to adopt some basic settings
    hc_add_theme(hc_theme_darkunica()) %>% 
    ## X-Axis settings
    hc_xAxis(categories = critic_score$Year_of_Release,
             title = list(text = "")) %>% 
    ## Y-Axis settings
    hc_yAxis(title = list(text = "Avg. Scores"),
             max = 100,
             showFirstLabel = FALSE) %>% 
             # plotBands = list(
             #   list(from = -50, to = 0, color = "#272B30",
             #        label = list(align = "center",
             #                     verticalAlign = "middle",
             #                     text = "",
             #                     style = list(color = "#ffffff",
             #                                  fontSize = "13px"))))) %>% 
    ## Tooltip settings
    hc_tooltip(crosshairs = TRUE,
               valueDecimals = 2,
               shared = TRUE) %>% 
    ## Crediting the data source
    hc_credits(enabled = TRUE, 
               text = "Data courtesy of https://www.kaggle.com/kendallgillies/video-game-sales-and-ratings. Collected in Jan 2017.",
               style = list(fontSize = "10px"))
 })

#---------------------------------------------------------------------------#

output$mytable = renderDataTable({
  
  user_selection_table <- function(table_input) {
    return(merged_sales_df %>% 
             filter(grepl(paste(table_input, collapse = '|'), Series)))
  }
  
  user_selection_df <- user_selection_table(input$table_series) 
  
  filtered_df <- user_selection_df %>% 
    filter(`Release Year` >= input$year_selection[1] & `Release Year` <= input$year_selection[2]) %>% 
    filter(`User Score` >= input$user_score[1] & `User Score` <= input$user_score[2]) %>% 
    filter(`Critic Score` >= input$critic_score[1] & `Critic Score` <= input$critic_score[2])
    
  
  datatable(filtered_df, style = "bootstrap")
})
})