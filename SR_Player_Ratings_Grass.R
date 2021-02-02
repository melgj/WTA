library(tidyverse)
library(PlayerRatings)
library(lubridate)
library(reshape2)


#players <- read_csv("wta_player_db.csv", col_names = T)
results <- read_csv("wta_results_db.csv", col_names = T)

tours = c("G", "P", "PM", "D", "I", "F", "W")

results <- results %>% 
  filter(tourney_level %in% tours, surface == "Grass")

base_date <- min(results$tourney_date)

head(results)
tail(results)

results_sorted <- results %>% 
  arrange(tourney_date)

results_sorted$period <- lubridate::interval(base_date, results_sorted$tourney_date) %/% weeks(1)+1

is.numeric(results_sorted$period)

res <- results_sorted %>% 
  select(period, winner_name, loser_name)

res$result <- 1

head(res)
tail(res)

srtng <- steph(res, init = c(1500,300), history = TRUE)

# elo_recent <- as_tibble(elo_ratings$ratings)

sr_df <- as_tibble(srtng$ratings)

head(sr_df)

sr_current <- sr_df %>% 
  filter(Lag < 5, Rating >= 1600, Games >= 10)

head(sr_current)

view(head(sr_current,30))

write_csv(sr_current, "SR_ratings_grass_jan21.csv")

sr_timeline <- as_tibble(srtng$history, rownames = "Player")
view(head(sr_timeline))

top <- sr_current$Player[1:20]

sr_timeline_current <- sr_timeline %>% 
  filter(Player %in% top) %>% 
  select(Player, ends_with("Rating")) %>% 
  select(Player, last_col(offset = 25):last_col())


sr_timeline_current

temp <- melt(sr_timeline_current, id.vars = "Player",variable.name = "Time", 
             value.name = "Rating")

head(temp, 10)
tail(temp, 10)

unique(temp$Player)
ggplot(temp,
       aes(x = Time,
           y = Rating,
           col = Player,
           group = Player
       )) +
  labs(title = "Grass Court Player Ratings Through Time - Current Top 20 Rated Players (Stephenson ELO Variant)", 
       x = "Time", y = "Rating") +
  scale_x_discrete(labels = NULL) +
  ylim(1500,2000) +
  geom_hline(yintercept = c(1600, 1800), col = "grey", lty = 2) +
  geom_line() +
  geom_smooth(lty = 2, lwd = 0.5, col = "red") +
  facet_wrap(~ Player, nrow = 4)
