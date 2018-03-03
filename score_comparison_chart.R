score_diff_df <- readRDS("shiny_diff_df.rds")

#---------------------------------------------------------------------------#

user_selection <- function(input) {
  return(score_diff_df %>% 
           filter(grepl(paste(input, collapse = '|'), series)))
}

example_list <- list("pokemon", "metal gear")

score_diff_df <- user_selection(example_list) 

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
    style = list(
      backgroundColor = "#272B30",
      fontFamily = "Helvetica")) %>% 
  ## Theme to adopt some basic settings
  hc_add_theme(hc_theme_darkunica()) %>% 
  ## X-Axis settings
  hc_xAxis(categories = critic_score$Year_of_Release,
          title = list(text = "Release Year")) %>% 
  ## Y-Axis settings
  hc_yAxis(title = list(text = "Avg. Scores"),
           max = 100,
           showFirstLabel = FALSE,
           plotBands = list(
             list(from = -50, to = 0, color = "#6d6934",
                  label = list(align = "center",
                               text = "Points in this range represent Critic Score is higher than User Score!",
                               style = list(color = "#ffffff",
                                            fontSize = "13px"))))) %>% 
  ## Tooltip settings
  hc_tooltip(valueDecimals = 2,
             shared = TRUE)


score_diff_chart
