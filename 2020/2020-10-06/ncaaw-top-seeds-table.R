# Load Libraries ----------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(scales)
library(gt)
# devtools::install_github("hadley/emo")
library(emo)


setwd("2020/2020-10-06/")

# Load Data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2020, week = 41)
tournament <- tuesdata$tournament



# Prepare Data ------------------------------------------------------------

# set a vartiable for the final four finishes
final_four <- c("NSF", "N2nd", "Champ")

# function to add trophy emoji from the emo package:
add_trophies <- function(reps) {
  a <- paste0("'") %>% paste(rep(ji("award"), reps), "'") %>% paste(sep = "", collapse = ",") %>% gsub(" ", "", .)
  return(a)
}

# create the main df to pass to gt
top_seeds <- tournament %>% 
  filter(year >= 2000) %>% 
  group_by(conference) %>% 
  summarise(number_teams_in_tourney = n(),
            avg_seed = round(mean(seed, na.rm = T), 1),
            num_champs = sum(tourney_finish == "Champ"),
            num_final_four = sum(tourney_finish %in% final_four)) %>%
  filter(number_teams_in_tourney >= 10) %>% 
  mutate(percent_final_four = scales::percent(num_final_four / number_teams_in_tourney, accuracy = 0.1)) %>% select(-num_final_four) %>%
  mutate(Championships = mapply(add_trophies, num_champs)) %>% 
  mutate(Championships = gsub("'", "", Championships) %>% gsub(",", "", .)) %>% 
  arrange(avg_seed) %>%
  head(15) %>% 
  select(Conference=conference, `
         Teams Played In Tourney`=number_teams_in_tourney, 
         `Avg Seeding`=avg_seed, 
         `Final Four %`=percent_final_four, 
         Championships)


# Create Table ------------------------------------------------------------
top_seeds_gt <- top_seeds %>%
  gt() %>%
  tab_header(
    title = md("**THE TOP 15 SEEDED CONFERENCES IN THE 21st CENTURY***"),
    subtitle = md("The Pac-12 have had the highest seeded teams (4.7) over the last 20 years, while the *American Athletic* have had the highest proportion of their tourney teams make the Final Four (45.5%).<br /><br />The  American Athletic Conference's three championships were all by the **Uconn Huskies**, which is interesting, as the same school has won seven of the eight titles won by the Big East. Their first win for the AAC was in 2014 (their first season in the AAC), where they went to win three in a row.")) %>% 
  tab_style(
    style = cell_fill(color = "yellow", alpha = 0.2),
    locations = list(
      cells_body(columns = 1, rows = Conference == "American Athletic"),
      cells_body(columns = 1, rows = Conference == "Big East"),
      cells_body(columns = 2, rows = Conference == "American Athletic"),
      cells_body(columns = 2, rows = Conference == "Big East"),
      cells_body(columns = 3, rows = Conference == "American Athletic"),
      cells_body(columns = 3, rows = Conference == "Big East"),
      cells_body(columns = 4, rows = Conference == "American Athletic"),
      cells_body(columns = 4, rows = Conference == "Big East"),
      cells_body(columns = 5, rows = Conference == "American Athletic"),
      cells_body(columns = 5, rows = Conference == "Big East")
    )
  ) %>%
  tab_options(table.background.color = alpha("black", 0.5)) %>% 
  tab_source_note("TABLE: @jaseziv | DATA: FIVETHIRTYEIGHT") %>% 
  tab_source_note(md("**Conferences must have had at least 10 teams make the tourney*")) %>% 
  tab_style(
    style = list(
      cell_text(font = "Comic Sans MS", align = "center")
    ),
    locations = list(
      cells_body(columns = gt::everything())
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = "Comic Sans MS", weight = "bold", align = "center")
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = "Comic Sans MS", size = "xx-large")
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = "Comic Sans MS", size = "large")
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  )

top_seeds_gt

gtsave(top_seeds_gt, "ncaaw-top-conferences.html")





