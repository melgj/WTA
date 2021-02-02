# Load required libraries

library(tidyverse)
library(PlayerRatings)
library(lubridate)
library(reshape2)

### import results from csv (Jan 1976 to Jan 2021)

#players <- read_csv("wta_player_db.csv", col_names = T)
results <- read_csv("wta_results_db.csv", col_names = T)

summary(results)

### filter matches by valid tour levels (ignore exhibitions etc.)
tours = c("G", "P", "PM", "I", "F", "W", "D")

results <- results %>% 
 filter(tourney_level %in% tours)

### Select earliest date in dataframe as base date
base_date <- min(results$tourney_date)

head(results)
tail(results)

### Sort by Tournament Date
results_sorted <- results %>% 
  arrange(tourney_date)

### Create column which holds week value (from base date)
results_sorted$period <- lubridate::interval(base_date, results_sorted$tourney_date) %/% weeks(1)+1

is.numeric(results_sorted$period)

### Create dataframe of four columns for use by PlayerRatings 'steph' funaction to calculate ratings
res <- results_sorted %>% 
  select(period, winner_name, loser_name)

res$result <- 1

head(res)
tail(res)

### calculate Stephenson ELO Ratings

srtng <- steph(res, init = c(1500,300), history = TRUE)

# elo_recent <- as_tibble(elo_ratings$ratings)

### Convert steph ratings to dataframe

sr_df <- as_tibble(srtng$ratings)

head(sr_df)
tail(sr_df)   

### Remove players absent from tour for approximately 1 year+, players 
### with low ratings and and players with insufficient games.

sr_current <- sr_df %>% 
  filter(Lag < 50, Rating >= 1600, Games >= 30)

#view(head(sr_current,30))

### Write ratings to csv file
write_csv(sr_current, "SR_ratings_jan21.csv")

### Convert historical ratings to dataframe
sr_timeline <- as_tibble(srtng$history, rownames = "Player")
#view(head(sr_timeline))

### Create variable holding a list of current top 20 ranked players
top <- sr_current$Player[1:20]

### filter historical ratings: Top 20 players, retain only Player and 
### 100 most recent Period rating columns

sr_timeline_current <- sr_timeline %>% 
  filter(Player %in% top) %>% 
  select(Player, ends_with("Rating")) %>% 
  select(Player, last_col(offset = 99):last_col())
  
  
sr_timeline_current

### Reshape dataframe for plotting
temp <- melt(sr_timeline_current, id.vars = "Player",variable.name = "Time", 
             value.name = "Rating")

head(temp, 10)
tail(temp, 10)

### Create ggplot of top 20 players Rating variations through time

unique(temp$Player)

ggplot(temp,
       aes(x = Time,
           y = Rating,
           col = Player,
           group = Player
           )) +
  labs(title = "Player Ratings Through Time - Current Top 20 Rated Players (Stephenson ELO Variant)", 
       x = "Time", y = "Rating") +
  scale_x_discrete(labels = NULL) +
  ylim(1600,2200) +
  geom_hline(yintercept = c(1800, 2000), col = "grey", lty = 2) +
  geom_line() +
  geom_smooth(lty = 2, lwd = 0.5, col = "red") +
  facet_wrap(~ Player, nrow = 4)
  
  
  
  
  
