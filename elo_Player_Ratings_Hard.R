library(tidyverse)
library(PlayerRatings)
library(lubridate)

#players <- read_csv("wta_player_db.csv", col_names = T)
results <- read_csv("wta_results_db.csv", col_names = T)

results <- results %>% 
  filter(year(tourney_date) >= 2011, surface == "Hard")

head(results)

unique(results$surface)

results_sorted <- results %>% 
  arrange(tourney_date)

base_date <- min(results_sorted$tourney_date)
results_sorted$period <- lubridate::interval(base_date, results_sorted$tourney_date) %/% weeks(1)+1

res <- results_sorted %>% 
  select(period, winner_name, loser_name)

res$period <- as.numeric(res$period)

res$result <- 1

head(res)
tail(res)

elo_ratings <- elo(res, init = 1500, kfac = 30, history = TRUE)

elo_recent <- as_tibble(elo_ratings$ratings) %>% 
  filter(Lag < 100, Rating >= 1600, Games >= 30)

view(elo_recent)

write_csv(elo_recent, "elo_recent_Hard.csv")
