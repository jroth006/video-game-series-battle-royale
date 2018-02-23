library(dplyr)
library(ggplot2)
library(plotly)
library(stringi)

## Reading in Sales data and performing basic cleanup, including translating
## non-ASCii titles.

sales_df <- read.csv("data/vg_sales.csv")
sales_df$Name <- as.character(sales_df$Name)
sales_df$Name <- stringi::stri_trans_general(sales_df$Name, "latin-ascii")
sales_df$Name <- tolower(sales_df$Name)

#---------------------------------------------------------------------------#

## Creating dataframes for each series

## List with series info to create a dataframe

sales_sorter <- function (sales_text) {
  sales_df %>% 
    filter(grepl(paste(sales_text), Name)) %>% 
    mutate(series = sales_text)
}

## Creating pre-sets for popular game series (will filter down to a few)

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

## Renaming platforms
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

## Output file to csv

write.csv(x = merged_sales_df, file = "merged_sales_df.csv")

## Trying a few different column adjustments

series_df$platform <- as.character(series_df$platform)
merged_sales_df$Platform <- as.character(merged_sales_df$Platform)
series_df$release_year <- as.factor(series_df$release_year)

## Attempting to join data

merged_df <- left_join(x = series_df, y = merged_sales_df, by = c("series" = "series",
                                                                  "release_year" = "Year_of_Release",
                                                                  "platform" = "Platform"))

merged_df <- left_join(x = series_df, y = merged_sales_df, by = c("title" = "Name",
                                                                  "platform" = "Platform"))

unmerged_df <- merged_df %>% 
  filter(is.na(series.y))

platforms <- as.data.frame(unique(merged_sales_df$Platform))
systems <- as.data.frame(unique(series_df$platform))
