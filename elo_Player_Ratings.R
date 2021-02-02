library(tidyverse)
library(PlayerRatings)
library(lubridate)
library(reshape2)

#players <- read_csv("wta_player_db.csv", col_names = T)
results <- read_csv("wta_results_db.csv", col_names = T)

tours = c("G", "P", "PM", "D", "I", "F", "W")

results <- results %>% 
 filter(results$tourney_level %in% tours)

base_date <- min(results$tourney_date)

head(results)
tail(results)

results_sorted <- results %>% 
  arrange(tourney_date)

results_sorted$period <- lubridate::interval(base_date, results_sorted$tourney_date) %/% weeks(1)+1

is.numeric(results_sorted$period)

res <- results_sorted %>% 
  select(period, winner_name, loser_name)

res$period <- as.numeric(res$period)

res$result <- 1

head(res)
tail(res)

elo_ratings <- elo(res, init = 1500, kfac = 30, history = TRUE)

elo_recent <- as_tibble(elo_ratings$ratings) %>% 
  filter(Lag < 50, Rating >= 1600, Games >= 30)

elo_recent

view(elo_recent)

write_csv(elo_recent, "elo_ratings_jan21.csv")

elo_timeline <- as_tibble(elo_ratings$history, rownames = "Player")
view(head(elo_timeline))

elo_timeline <- elo_timeline %>% 
  filter(Player %in% elo_recent$Player)

elo_timeline

colnames(elo_timeline)

pl = elo_recent$Player[1:20]

elo_timeline_current <- elo_timeline %>% 
  filter(Player %in% pl) %>% 
  select(Player, ends_with("Rating")) %>% 
  select(Player, last_col(offset = 99):last_col())

elo_timeline_current

#temp <- melt(elo_timeline_current, id.vars = "Player")
temp <- melt(elo_timeline_current, id.vars = "Player",variable.name = "Time", 
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
  labs(title = "Player Ratings Through Time - Current Top 20 Rated Players by ELO", 
       x = "Time", y = "Rating") +
  scale_x_discrete(labels = NULL) +
  ylim(1600,2200) +
  geom_hline(yintercept = c(1800, 2000), col = "grey", lty = 2) +
  geom_line() +
  geom_smooth(lty = 2, lwd = 0.5, col = "red") +
  facet_wrap(~ Player, nrow = 4)

