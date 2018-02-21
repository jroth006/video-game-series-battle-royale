library(dplyr)
library(ggplot2)
library(plotly)
library(stringi)

## Reading in IGN data and performing basic cleanup, including translating
## non-ASCii titles. Will perfrom 

vg_df <- read.csv("data/ign.csv")
vg_df$title <- as.character(vg_df$title)
vg_df$title <- stringi::stri_trans_general(vg_df$title, "latin-ascii")
vg_df$title <- tolower(vg_df$title)
vg_df$ign_url <- vg_df$url

## Listing iconic video game series 
## (according to ranker.com, allowing users to vote - 
## Only video game franchises with at least 3 games in the series.)
## https://www.ranker.com/list/best-video-game-franchises/ranker-games
#---------------------------------------------------------------------------#
## Top 20 Series (in order)
## (Super Mario Bros., The Legend of Zelda, Super Smash Bros., Mario Kart, Grand Theft Auto,
## The Elder Scrolls, Fallout, Sonic the Hedgehog, Mortal Kombat, Halo,
## Metal Gear, Metroid, Assassin's Creed, Crash Bandicoot, Street Fighter,
## Mega Man, Donkey Kong Country, Tomb Raider, Call of Duty, Resident Evil)
#---------------------------------------------------------------------------#

## Creating dataframes for each series

## Using function to return titles that match the input from user on custom searches (for shiny input)

user_selection <- function(...) {
                  input_list <- list(...)
                  x <- vg_df %>% 
                    filter(grepl(paste(input_list, collapse = '|'), title))
                  return(x)
}

## Testing

user_selection_df <- user_selection("mega man", "mario")

## Creating pre-sets for popular game series (will filter down to a few)

mario_df = series_sorter("super mario")
zelda_df = series_sorter("zelda")
smash_df = series_sorter("super smash")
mkart_df = series_sorter("mario kart")
gta_df = series_sorter("grand theft auto")
scrolls_df = series_sorter("elder scrolls")
fallout_df = series_sorter("fallout")
sonic_df = series_sorter("sonic")
mk_df = series_sorter("mortal kombat")
halo_df = series_sorter("halo")
metal_df = series_sorter("metal gear")
metroid_df = series_sorter("metroid")
ac_df = series_sorter("assassin's creed")
crash_df = series_sorter("crash bandicoot")
sf_df = series_sorter("street fighter")
megaman_df = series_sorter("mega man")
dkc_df = series_sorter("donkey kong country")
tomb_df = series_sorter("tomb raider")
cod_df = series_sorter("call of duty")
resident_df = series_sorter("resident evil")

## List with series info to create a dataframe

series_sorter <- function (series_text) {
  vg_df %>% 
    filter(grepl(paste(series_text, collapse('|')), title)) %>% 
    mutate(series = series_text)
}

# series_dict <- list(mario_df = "super mario", zelda_df = "zelda", smash_df = "super smash", 
#                     mkart_df = "kart", gta_df = "grand theft auto", scrolls_df = "elder scrolls", 
#                     fallout_df = "fallout", sonic_df = "sonic", mk_df = "mortal kombat", 
#                     halo_df = "halo", metal_df = "metal gear", metroid_df = "metroid", 
#                     ac_df = "assassin's creed", crash_df = "crash bandicoot", 
#                     sf_df = "street fighter", megaman_df = "mega man", dkc_df = "donkey kong country", 
#                     tomb_df = "tomb raider", cod_df = "call of duty", resident_df = "resident evil")

## For loop to create dataframes from series_dict

# for(i in i:length(series_dict)) {
#   names(series_dict[i]) <- series_sorter(unlist(series_dict[i]))
# }


## Removing games that were caught by the grepl but don't belong to the series.

cod_df <- cod_df[-c(1), ]
mario_df <- mario_df[-c(21, 31), ]
sonic_df <- sonic_df[-c(8, 18, 25, 52), ]

## Merging series data

series_df <- rbind(mario_df, zelda_df, smash_df, mkart_df, gta_df, scrolls_df, 
                   fallout_df, sonic_df, mk_df, halo_df, metal_df, metroid_df, ac_df, 
                   crash_df, sf_df, megaman_df, dkc_df, tomb_df, cod_df, resident_df)

## Initial data visualization for exploratory analysis

series_plot_df <- plot_ly(merged_df, x = ~release_year, y = ~score, color = ~series,
                          type = 'scatter', mode = 'line',
                          text = ~paste("Title: ", title,
                                      "<br> Series: ", series,
                                      "<br> Year: ", release_year,
                                      "<br> Platform: ", platform))
                    
series_plot_df

series_df_by_platform <- series_df %>%
                         group_by(release_year)

x_system <- list(title = "", showticklabels = TRUE)
y_score <- list(title = "Score")

game_box_plot<- plot_ly(series_df_by_platform, y = (~score), x = ~release_year, color = ~series,
                        type = "scatter",
                        mode = "markers",
                        alpha = 0.5,
                        text = ~paste("Title: ", title,
                                      "<br> Series: ", series,
                                      "<br> Year: ", release_year,
                                      "<br> Platform: ", platform)) %>% 
  add_trace(series_df_by_platform, y = (~score), x = ~release_year,  
            type = "box",
            showlegend = FALSE) %>% 
  layout(xaxis = x_system, yaxis = y_score)

game_box_plot
warnings()
