## Load in data
shiny_sales_df <- readRDS("data/shiny_sales_df.RDS")
score_diff_df <- readRDS("data/shiny_diff_df.RDS")
merged_sales_df <- readRDS("data/merged_sales_df.RDS")

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

## First Tab - comparison barplot with images  
  
  # Right Chart based on selection  
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
        spacingLeft = 0,
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
                  align = "right",
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
  
# Right image selection based on selection
  output$right_image <- renderImage({
    input$goButton
    
    filename <- normalizePath(file.path('./www',
                                        paste(isolate(input$series_name[2]), '.png', sep = '')))
    
    # Return a list containing the filename and size options
    list(src = filename,
         height = 300,
         width = 200)
    
  }, deleteFile = FALSE)
  
  #------------------------------------------------------------#  
  
  # Left Chart based on selection  
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
        spacingRight = 0,
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
                 enabled = FALSE,
                 overflow = "justify")) %>%
      ## Tooltip settings
      hc_tooltip(enabled = FALSE) %>%
      ## Theme to adopt some basic settings
      hc_add_theme(hc_theme_darkunica()) %>% 
      ## Crediting the image source
      hc_credits(enabled = TRUE, 
                 text = "Images courtesy of https://www.ssbwiki.com/Super_Smash_Bros._(series)",
                 style = list(fontSize = "10px"))
    
  })
  
  #------------------------------------------------------------#  
  
  # Left image selection based on selection
  output$left_image <- renderImage({
    input$goButton

    filename <- normalizePath(file.path('./www',
                                        paste(isolate(input$series_name[1]), '.png', sep = '')))
    
    # Return a list containing the filename and size options
    list(src = filename,
         height = 300,
         width = 200)
    
  }, deleteFile = FALSE)

#------------------------------------------------------------#  

## Second tab - Score comparison chart
  output$score_comp_chart <- renderHighchart({
    # chart_type = input$chart_type_input
    # if(chart_type == "combined"){
#---------------------------------------------------------------------------#
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
    hc_add_series(name = "Critic Score", critic_score$value, color = "#8FBB58") %>% 
    hc_add_series(name = "User Score", user_score$value, color = "#FDE725") %>%
    hc_add_series(type = "column", name = "Total Global Sales (Millions)", total_sales$value, color = "#440154") %>%
    hc_add_series(name = "Score Differential", score_differential$value, color = "#21908C") %>%
    ## Chart settings
    hc_chart(
      backgroundColor = "#272B30",
      style = list(
        fontFamily = "Helvetica")) %>% 
    ## Theme to adopt some basic settings
    hc_add_theme(hc_theme_darkunica()) %>% 
    ## X-Axis settings
    hc_xAxis(categories = critic_score$Year_of_Release,
             title = list(text = ""),
             labels = list(
               style = list(
                 fontSize = "14px"))) %>% 
    ## Y-Axis settings
    hc_yAxis(title = list(enabled = FALSE),
             max = 100,
             showFirstLabel = FALSE,
             labels = list(
               style = list(
                 fontSize = "14px")),
             plotBands = list(
               list(from = -50, to = 0, color = "#373C42",
                    label = list(align = "center",
                                 verticalAlign = "middle",
                                 text = "Score differentials in this range represent Critic Score is higher than User Score!",
                                 style = list(color = "#ffffff",
                                              fontSize = "14px"))))) %>%
    ## Legend settings
    hc_legend(labels = list(
                style = list(
                  fontSize = "14px"
                ))) %>% 
    ## Tooltip settings
    hc_tooltip(crosshairs = TRUE,
               valueDecimals = 2,
               shared = TRUE)
  # } else{
  #   print("scatter!")
  # }
  })


#---------------------------------------------------------------------------#

## Third tab - table data based on input  
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

#---------------------------------------------------------------------------#

## Fourth tab - Analysis text with chart presets
output$analysis_chart <- renderHighchart({
  user_selection <- function(chart_input) {
    return(score_diff_df %>% 
             filter(grepl(paste(chart_input, collapse = '|'), series)))
  }
  
  analysis_df <- user_selection(input$analysis_input) 
  
  analysis_critic_score <- analysis_df %>% 
    filter(variable == "Avg. Critic Score") %>%
    select(-c(series)) %>% 
    group_by(Year_of_Release) %>% 
    summarise("value" = mean(value)) %>% 
    filter(Year_of_Release != "N/A")
  
  analysis_user_score <- analysis_df %>% 
    filter(variable == "Avg. User Score") %>%
    select(-c(series)) %>% 
    group_by(Year_of_Release) %>% 
    summarise("value" = mean(value)) %>% 
    filter(Year_of_Release != "N/A")
  
  analysis_score_differential <- analysis_df %>% 
    filter(variable == "Score Differential") %>%
    select(-c(series)) %>% 
    group_by(Year_of_Release) %>% 
    summarise("value" = mean(value)) %>% 
    filter(Year_of_Release != "N/A") 
  
  analysis_total_sales <- analysis_df %>% 
    filter(variable == "Total Global Sales") %>%
    select(-c(series)) %>% 
    group_by(Year_of_Release) %>% 
    summarise("value" = mean(value)) %>% 
    filter(Year_of_Release != "N/A")
  
  #---------------------------------------------------------------------------#
  
  analysis_chart <- highchart() %>% 
    hc_add_series(name = "Critic Score", analysis_critic_score$value, color = "#8FBB58") %>% 
    hc_add_series(name = "User Score", analysis_user_score$value, color = "#FDE725") %>%
    hc_add_series(type = "column", name = "Total Global Sales (Millions)", analysis_total_sales$value, color = "#440154") %>%
    hc_add_series(name = "Score Differential", analysis_score_differential$value, color = "#21908C") %>%
    ## Chart settings
    hc_chart(
      backgroundColor = "#272B30",
      style = list(
        fontFamily = "Helvetica")) %>% 
    ## Theme to adopt some basic settings
    hc_add_theme(hc_theme_darkunica()) %>% 
    ## X-Axis settings
    hc_xAxis(categories = analysis_critic_score$Year_of_Release,
             title = list(text = ""),
             labels = list(
               style = list(
                 fontSize = "14px"))) %>% 
    ## Y-Axis settings
    hc_yAxis(title = list(enabled = FALSE),
             max = 100,
             showFirstLabel = FALSE,
             labels = list(
               style = list(
                 fontSize = "14px")),
             plotBands = list(
               list(from = -50, to = 0, color = "#373C42",
                    label = list(align = "center",
                                 verticalAlign = "middle",
                                 text = "Score differentials in this range represent Critic Score is higher than User Score!",
                                 style = list(color = "#ffffff",
                                              fontSize = "14px"))))) %>%
    ## Legend settings
    hc_legend(labels = list(
      style = list(
        fontSize = "14px"
      ))) %>% 
    ## Tooltip settings
    hc_tooltip(crosshairs = TRUE,
               valueDecimals = 2,
               shared = TRUE)
})
})