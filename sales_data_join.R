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
## Will refactor when I have more time

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

merged_sales_df <- rbind(mario_sales_df, zelda_sales_df, smash_sales_df, mkart_sales_df, gta_sales_df, scrolls_sales_df, 
                         fallout_sales_df, sonic_sales_df, mk_sales_df, halo_sales_df, metal_sales_df, metroid_sales_df, ac_sales_df, 
                         crash_sales_df, sf_sales_df, megaman_sales_df, dkc_sales_df, tomb_sales_df, cod_sales_df, resident_sales_df,
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

## Output file to csv

## write.csv(x = merged_sales_df, file = "merged_sales_df.csv")
saveRDS(merged_sales_df, file = "merged_sales_df.rds")
merged_sales_df <- readRDS("merged_sales_df.rds")

## Finding the difference between user scores and critic scores for second tab

score_diff_function <- function(diff_input){
  merged_sales_df %>% 
    filter(series == diff_input) %>% 
    select("Year_of_Release","User_Score", "User_Count", 
           "Critic_Score", "Critic_Count", "Global_Sales", "series") %>% 
    group_by(Year_of_Release, series) %>% 
    summarise("Avg. Critic Score" = mean(Critic_Score[Critic_Score>1],na.rm=TRUE),
              "Avg. User Score" = mean(User_Score[User_Score > 1], na.rm = TRUE),
              "Score Differential" = mean(User_Score[User_Score > 1],na.rm=TRUE) - 
                mean(Critic_Score[Critic_Score>1], na.rm = TRUE),
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

saveRDS(score_diff_df, "shiny_diff_df.RDS")

#---------------------------------------------------------------------------#

## Data Visualizations for series comparison

text_input <- input$radio2

shiny_function <- function(text_input){
  merged_sales_df %>% 
  filter(series == text_input) %>% 
  select("Name", "Platform", "Year_of_Release", "Global_Sales",
         "User_Score", "User_Count", "Critic_Score", "Critic_Count", "series") %>% 
  summarise("Series Name" = unique(series),
            "# of Platforms" = length(unique(Platform)),
            "# of Titles" = length(unique(Name)),
            "Avg. Global Units Sold (in millions)" = mean(Global_Sales),
            "Avg. Critic Score" = mean(Critic_Score[Critic_Score>1],na.rm=TRUE),
            "Avg. User Score" = mean(User_Score[User_Score > 1], na.rm = TRUE)) %>% 
  melt(id = "Series Name") %>% 
  mutate(value = round(value, 2))
}

merged_sales_format <- shiny_function("super mario")

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

saveRDS(shiny_sales_df, "shiny_sales_df.RDS")

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
score_diff_df <- score_diff_function("street fighter")
score_diff_df$Year_of_Release <- as.integer(paste(score_diff_df$Year_of_Release))
score_diff_chart <- score_diff_df %>% 
  filter(!is.na(Year_of_Release)) %>% 
  filter(!is.na(value)) %>% 
  hchart(score_diff_df, "scatter", hcaes(x = Year_of_Release,
                             y = value,
                             color = variable)) %>% 
  ## Chart settings
  hc_chart(
    style = list(
      fontFamily = "Helvetica")) %>% 
  ## Y-Axis settings
  hc_yAxis(title = list(text = "Avg. Scores"),
           max = 100,
           showFirstLabel = FALSE,
           plotBands = list(
             list(from = -50, to = 0, color = "#8e0000",
                  label = list(text = "Critic Score is higher than User Score!")))) %>% 
  ## X-Axis settings
  hc_xAxis(title = list(text = "Release Year")) %>% 
  ## Theme to adopt some basic settings
  hc_add_theme(hc_theme_darkunica())

score_diff_chart

#---------------------------------------------------------------------------#

# Using function to return titles that match the input from user on custom searches (for shiny input)

user_selection <- function(...) {
  input_list <- list(...)
  return(score_diff_function %>% 
           filter(grepl(paste(input_list, collapse = '|'), title)))
}

