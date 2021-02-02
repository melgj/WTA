library(tidyverse)
library(PlayerRatings)
library(lubridate)
library(reshape2)

players <- read_csv("wta_player_db.csv", col_names = T)
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
  filter(Lag < 50, Rating >= 1600)

elo_recent

# elo_1600 <- elo_recent %>% 
#   filter(Lag < 50, Rating >= 1600)
# elo_1600

view(elo_recent)

write_csv(elo_recent, "/home/magljo/sports_analysis/wta_tennis/elo1600.csv")

elo_timeline <- as_tibble(elo_ratings$history, rownames = "Player")
view(head(elo_timeline))

elo_timeline <- elo_timeline %>% 
  filter(Player %in% elo_recent$Player)

elo_timeline

colnames(elo_timeline)

pl = elo_recent$Player[1:15]

elo_timeline_current <- elo_timeline %>% 
  filter(Player %in% pl) %>% 
  select(Player, `1400.Rating`:`1548.Rating`)

elo_timeline_current

temp <- melt(elo_timeline_current, id.vars = "Player")

head(temp, 10)
tail(temp, 10)

unique(temp$Player)
ggplot(temp,
       aes(x = variable,
           y = value,
           col = Player,
           group = Player
       )) +
  labs(title = "Player Ratings Through Time - Top 15", 
       x = "Time", y = "Rating") +
  scale_x_discrete(labels = NULL) +
  ylim(1600,2200) +
  geom_line() +
  facet_wrap(~ Player, nrow = 3)


#pl = c("Ashleigh Barty", "Naomi Osaka", "Simona Halep", "Aryna Sabalenka", "Bianca Andreescu")

# plot(elo_ratings, players = pl, t0 = 300)
# legend("topleft", legend = pl, lwd = 3, lty = 1:5, cex = 0.4, y.intersp = 0.2, col = 1:5,
#        text.width = 12)
# text(c(300,350,400,450, 500), rep(1500, 5), 
#      c("2017","2018","2019","2020", "2021"))
# 
# dim(elo_timeline)

# temp <- melt(elo_1600[elo_1600$Player %in% pl,], id.vars = "Player")
# 
# head(temp, 10)
# tail(temp, 10)
# 
# unique(temp$Player)
# ggplot(temp,
#        aes(x = variable,
#            y = value,
#            col = Player,
#            group = Player
#            )) +
#   labs(title = "Player ELO Ratings 2017 to January 2021", 
#        x = "Time", y = "Rating") +
#   scale_x_discrete(labels = NULL) + 
#   scale_y_continuous(breaks = seq(1500, 2100, by = 50)) +
#   geom_line()
#   