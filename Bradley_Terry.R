library(tidyverse)
library(BradleyTerry2)
library(lubridate)

#players <- read_csv("wta_player_db.csv")
results <- read_csv("wta_results_db_11_21.csv")

tours = c("G", "P", "PM", "D", "I", "F", "W")

#summary(results$loser_seed)

results <- results %>% 
  filter(tourney_level %in% tours)

head(results)

# BTM(outcome(Player1), player1(factor), player2(factor))

results_sorted <- results %>% 
  arrange(tourney_date)

base_date <- min(results_sorted$tourney_date)
base_date

results_sorted$period <- lubridate::interval(base_date, results_sorted$tourney_date) %/% weeks(1)+1

results_sorted$result <- 1

res <- results_sorted %>% 
  select(result, winner_name, loser_name)
str(res)

res_train <- res %>% 
  mutate(result = as.numeric(result),
         winner_name = factor(winner_name, levels = unique(c(winner_name,loser_name))),
         loser_name = factor(loser_name, levels = levels(winner_name))
         )

levels(res_train$winner_name)

fit <- BTm(result, winner_name, loser_name, refcat = "Andrea Petkovic", data = res_train)


abilities <- BTabilities(fit)

abilities <- as_tibble(abilities, rownames = "Player")

abilities <- abilities %>% 
  mutate(ability = round(ability * 100, 2))

ranked <- abilities %>% 
  arrange(desc(ability)) %>% 
  filter(s.e. < 0.5)

top50 <- ranked[1:50,]

view(top50)


