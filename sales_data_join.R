library(dplyr)
library(ggplot2)
library(plotly)
library(stringi)
library(highcharter)
library(reshape2)
library(tidyr)

## Reading in Sales data and performing basic cleanup, including translating
## non-ASCii titles.

sales_df <- read.csv("data/vg_sales.csv")
sales_df$Name <- as.character(sales_df$Name)
sales_df$Name <- stringi::stri_trans_general(sales_df$Name, "latin-ascii")
sales_df$Name <- tolower(sales_df$Name)
sales_df$User_Score <- as.numeric(sales_df$User_Score)

#---------------------------------------------------------------------------#

## Creating dataframes for each series

## List with series info to create a dataframe

sales_sorter <- function (sales_text) {
  sales_df %>% 
    filter(grepl(paste(sales_text), Name)) %>% 
    mutate(series = as.factor(sales_text))
}

## Creating pre-sets for popular game series (will filter down to a few)
## Will try to refactor when I have more time

mario_sales_df = sales_sorter("super mario")
zelda_sales_df = sales_sorter("zelda")
smash_sales_df = sales_sorter("super smash")
mkart_sales_df = sales_sorter("mario kart")
gta_sales_df = sales_sorter("grand theft auto")
scrolls_sales_df = sales_sorter("elder scrolls")
fallout_sales_df = sales_sorter("fallout")
sonic_sales_df = sales_sorter("sonic")
mk_sales_df = sales_sorter("mortal kombat")
halo_sales_df = sales_sorter("halo")
metal_sales_df = sales_sorter("metal gear")
metroid_sales_df = sales_sorter("metroid")
ac_sales_df = sales_sorter("assassin's creed")
crash_sales_df = sales_sorter("crash bandicoot")
sf_sales_df = sales_sorter("street fighter")
megaman_sales_df = sales_sorter("mega man")
dkc_sales_df = sales_sorter("donkey kong country")
tomb_sales_df = sales_sorter("tomb raider")
cod_sales_df = sales_sorter("call of duty")
resident_sales_df = sales_sorter("resident evil")
pokemon_sales_df = sales_sorter("pokemon")
kirby_sales_df = sales_sorter("kirby")

## Merging series data

merged_sales_df <- rbind(mario_sales_df, zelda_sales_df, sonic_sales_df, mk_sales_df, 
                         halo_sales_df, metal_sales_df, sf_sales_df, megaman_sales_df, 
                         pokemon_sales_df, kirby_sales_df)

## Renaming platforms for clarity
## Will refactor when I have more time
levels(merged_sales_df$Platform) <- sub("NES", "NES", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("DS", "Nintendo DS", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("Wii", "Wii", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("SNES", "Super NES", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("GB", "Game Boy", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("N64", "Nintendo 64", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("3DS", "Nintendo 3DS", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("GC", "GameCube", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("GBA", "Game Boy Advance", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("WiiU", "Wii U", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("PS3", "PlayStation 3", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("PS2", "PlayStation 2", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("X360", "Xbox One", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("PS4", "PlayStation 4", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("PSP", "PlayStation Portable", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("XOne", "Xbox One", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("PS", "PlayStation", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("XB", "Xbox", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("PC", "PC", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("GEN", "Genesis", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("DC", "Dreamcast", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("PSV", "PlayStation Vita", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("SAT", "Sega Saturn", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("SCD", "Sega CD", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("NG", "Neo Geo", levels(merged_sales_df$Platform))
levels(merged_sales_df$Platform) <- sub("GG", "Game Gear", levels(merged_sales_df$Platform))

#---------------------------------------------------------------------------#
## Creating merged_sales_df for the Data Table
original_merged_sales_df <- readRDS("original_merged_sales_df.rds")
avg_by_platform <- original_merged_sales_df %>% 
  filter(Critic_Count > 5 & User_Count > 5) %>% 
  
  group_by(Platform) %>%
  summarise("Avg. Global Sales" = mean(Global_Sales, drop_na = TRUE),
            "Avg. Critic Score" = mean(Critic_Score, drop_na = TRUE),
            "Avg. User Score" = mean(User_Score, drop_na = TRUE))


merged_sales_df <- original_merged_sales_df %>% 
  select(-c(NA_Sales, EU_Sales, JP_Sales, Other_Sales,
            Critic_Count, User_Count, Rating)) %>%
  mutate("Name" = gsub("(?<=\\b)([a-z])", "\\U\\1", Name, perl = TRUE)) %>%
  mutate("series" = gsub("(?<=\\b)([a-z])", "\\U\\1", series, perl = TRUE)) %>% 
  transform("Score Differential" = User_Score - Critic_Score)


colnames(merged_sales_df) <- c("Title", "Platform", "Release Year", "Genre", "Publisher", "Total Global Sales (Millions)",
           "Critic Score", "User Score", "Developer", "Series", "Score Differential")

merged_sales_df$`Release Year` <- as.integer(paste(merged_sales_df$`Release Year`))
merged_sales_df <- merged_sales_df %>% 
  drop_na(`Release Year`)

saveRDS(merged_sales_df, file = "merged_sales_df.rds")

#---------------------------------------------------------------------------#
## Finding the difference between user scores and critic scores for second tab

score_diff_function <- function(diff_input){
  original_merged_sales_df %>% 
    filter(series == diff_input,
           !is.na(Critic_Score),
           !is.na(User_Score)) %>% 
    select("Year_of_Release","User_Score", "User_Count", 
           "Critic_Score", "Critic_Count", "Global_Sales", "series") %>% 
    group_by(Year_of_Release, series) %>% 
    summarise("Avg. Critic Score" = mean(Critic_Score, na.rm=TRUE),
              "Avg. User Score" = mean(User_Score, na.rm = TRUE),
              "Score Differential" = mean(User_Score,na.rm=TRUE) - 
                mean(Critic_Score),
              "Total Global Sales" = sum(Global_Sales)) %>%
    drop_na(`Score Differential`) %>%
    drop_na(`Total Global Sales`) %>% 
    melt(id = c("Year_of_Release", "series")) %>%
    filter(Year_of_Release != "N/A")
}

mario_diff_df <- score_diff_function("super mario")
zelda_diff_df <- score_diff_function("zelda")
sonic_diff_df <- score_diff_function("sonic")
mk_diff_df <- score_diff_function("mortal kombat")
halo_diff_df <- score_diff_function("halo")
metal_diff_df <- score_diff_function("metal gear")
sf_diff_df <- score_diff_function("street fighter")
megaman_diff_df <- score_diff_function("mega man")
pokemon_diff_df <- score_diff_function("pokemon")
kirby_diff_df <- score_diff_function("kirby")

score_diff_df <- rbind(mario_diff_df, zelda_diff_df, sonic_diff_df, mk_diff_df,
                       halo_diff_df, metal_diff_df, sf_diff_df, megaman_diff_df,
                       pokemon_diff_df, kirby_diff_df)

score_diff_df <- score_diff_df %>% 
  mutate("series" = gsub("(?<=\\b)([a-z])", "\\U\\1", series, perl = TRUE))

saveRDS(score_diff_df, "shiny_diff_df.RDS")

#---------------------------------------------------------------------------#
## Creating dataframe for series comparison on the first tab

text_input <- input$radio2

original_merged_sales_df <- readRDS("original_merged_sales_df.rds")

shiny_function <- function(text_input){
  original_merged_sales_df %>% 
  filter(series == text_input) %>% 
  select("Name", "Platform", "Year_of_Release", "Global_Sales",
         "User_Score", "User_Count", "Critic_Score", "Critic_Count", "series") %>% 
  summarise("Series Name" = unique(series),
            "Avg. Sales per Title (millions)" = mean(Global_Sales),
            "Number of Different Platforms" = length(unique(Platform)),
            "Number of Titles in Series" = length(unique(Name)),
            "Avg. Critic Score - Metacritic" = mean(Critic_Score[Critic_Score>1],na.rm=TRUE),
            "Avg. User Score - Metacritic" = mean(User_Score[User_Score > 1], na.rm = TRUE)) %>% 
  melt(id = "Series Name") %>% 
  mutate(value = round(value, 2)) 
}

mario_merged_df = shiny_function("super mario")
zelda_merged_df = shiny_function("zelda")
sonic_merged_df = shiny_function("sonic")
mk_merged_df = shiny_function("mortal kombat")
halo_merged_df = shiny_function("halo")
metal_merged_df = shiny_function("metal gear")
sf_merged_df = shiny_function("street fighter")
megaman_merged_df = shiny_function("mega man")
pokemon_merged_df = shiny_function("pokemon")
kirby_merged_df = shiny_function("kirby")

shiny_sales_df <- rbind(mario_merged_df, zelda_merged_df, sonic_merged_df, mk_merged_df,
                        halo_merged_df, metal_merged_df, sf_merged_df, megaman_merged_df,
                        pokemon_merged_df, kirby_merged_df)

shiny_sales_df <- shiny_sales_df %>% 
  mutate("Series Name" = gsub("(?<=\\b)([a-z])", "\\U\\1", `Series Name`, perl = TRUE))

saveRDS(shiny_sales_df, "shiny_sales_df.RDS")

#---------------------------------------------------------------------------#
## Data Visualizations for series comparison - Right side (include variable names)

right_res_chart <- hchart(merged_sales_format, "bar", hcaes(x = merged_sales_format$variable, 
                           y = merged_sales_format$value,
                           color = merged_sales_format$variable)) %>% 
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

#---------------------------------------------------------------------------#
## Data Visualizations for series comparison - Left side (no variable names)

left_res_chart <- hchart(merged_sales_format, "bar", hcaes(x = merged_sales_format$variable, 
                                                      y = merged_sales_format$value,
                                                      color = merged_sales_format$variable)) %>% 
                    ## Chart settings
                    hc_chart(
                      height = '125%',
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
                             # opposite = TRUE,
                             # min = 0,
                             # max = 350,
                             # showFirstLabel = FALSE,
                             # tickInterval = 50,
                             # title = list(
                             #     enabled = FALSE)) %>%
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

left_res_chart

#---------------------------------------------------------------------------#
## Function to allow for averaging multiple series

user_selection <- function(input) {
  return(score_diff_df %>% 
           filter(grepl(paste(input, collapse = '|'), series)))
}

## Test List
example_list <- list("pokemon", "metal gear")

## Calling function to create df
score_diff_df <- user_selection(example_list) 

#---------------------------------------------------------------------------#
## Creating individual df's for each variable to create individual series in plot
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
## Creating highchart for score comparison

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
  hc_credits(enabled = TRUE, 
             text = "Data courtesy of https://www.kaggle.com/kendallgillies/video-game-sales-and-ratings",
             style = list(fontSize = "10px"))


score_diff_chart