library(tidyverse)
library(PlayerRatings)
library(lubridate)

players <- read_csv("wta_player_db.csv", col_names = T)
results <- read_csv("wta_results_db.csv", col_names = T)

results <- results %>% 
  filter(year(tourney_date) >= 2011, surface == "Hard")

head(results)

unique(results$surface)

results_sorted <- results %>% 
  arrange(tourney_date)

base_date <- lubridate::as_date("1900-01-01")
base_date
results_sorted$period <- lubridate::as.duration(results_sorted$tourney_date - base_date)

res <- results_sorted %>% 
  select(period, winner_name, loser_name)

res$period <- as.numeric(res$period)

res$result <- 1

head(res)
tail(res)

elo_ratings <- elo(res, init = 1500, kfac = 30, history = TRUE)

elo_recent <- as_tibble(elo_ratings$ratings)

elo_1600 <- elo_recent %>% 
  filter(Lag < 100, Rating >= 1600)
elo_1600

view(elo_1600)

write_csv(elo_1600, "/home/magljo/sports_analysis/wta_tennis/elo1600_Hard.csv")
