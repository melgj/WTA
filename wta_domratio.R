library(tidyverse)
library(PlayerRatings)
library(lubridate)


#players <- read_csv("wta_player_db.csv", col_names = T)
wta_results <- read_csv("wta_results_db_18_21.csv", col_names = T) %>% 
  filter(year(tourney_date) < 2021)

wta_tours = c("G", "P", "PM", "D", "I", "F", "W")

wta_results <- wta_results %>% 
  filter(tourney_level %in% wta_tours)

base_date_db <- min(wta_results$tourney_date)


wta_res_sorted <- wta_results %>% 
  arrange(tourney_date)

wta_res_sorted$period <- lubridate::interval(base_date_db, wta_res_sorted$tourney_date) %/% weeks(1)+1

head(wta_res_sorted)

results_dr <- wta_res_sorted %>% 
  mutate(WDR = round(((w_1stwon + w_2ndwon)/w_svpt) / ((l_1stwon + l_2ndwon)/l_svpt),4),
         LDR = round(((l_1stwon + l_2ndwon)/l_svpt) / ((w_1stwon + w_2ndwon)/w_svpt),4))

mean(results_dr$WDR, na.rm = T)
mean(results_dr$LDR, na.rm = T)

results_dr2 <- results_dr %>% 
  mutate(Best_Rank = if_else(winner_rank < loser_rank, winner_rank, loser_rank),
         Worst_Rank = if_else(winner_rank > loser_rank, winner_rank, loser_rank),
         Best_Rank_Won = if_else(winner_rank == Best_Rank, 1, 0),
         Rank_Diff = Worst_Rank - Best_Rank,
         Best_Rank_DR = if_else(Best_Rank_Won == 1, WDR, LDR),
         Worst_Rank_DR = if_else(Best_Rank_Won == 0, WDR, LDR))

summary(results_dr2)

rd1 <- results_dr2 %>% 
  filter(Best_Rank <= 100) %>% 
  drop_na(Best_Rank, Worst_Rank, WDR, LDR) %>% 
  group_by(Best_Rank) %>% 
  summarise(median_DR = median(Best_Rank_DR),
            Rank_Win_Pct = mean(Best_Rank_Won)) %>% 
  arrange(Best_Rank)
  
view(rd1)

qplot(Best_Rank, median_DR, data = rd1)+
  geom_smooth()

t5 <- 1:20
b5 <- 81:100

gap <- results_dr2 %>% 
  filter(Best_Rank %in% t5, Worst_Rank %in% b5)

gap1 <- gap %>% 
  drop_na(Best_Rank, Worst_Rank, WDR, LDR) %>% 
  group_by(Best_Rank) %>% 
  summarise(median_DR = median(Best_Rank_DR)) %>% 
  arrange(Best_Rank)

gap1 

mean(gap1$median_DR)

rd2 <- results_dr2 %>% 
  filter(Rank_Diff <= 100) %>% 
  drop_na(Best_Rank, Worst_Rank, WDR, LDR) %>% 
  group_by(Rank_Diff) %>% 
  summarise(median_opp_DR = median(Worst_Rank_DR),
            median_DR = median(Best_Rank_DR)) %>% 
  arrange(Rank_Diff)

median(rd2$median_DR)

rd2 %>% 
  ggplot(aes(Rank_Diff, median_DR)) +
  geom_point(color = "red")+
  geom_smooth()+
  labs(title = "Dominance Ratio by Ranking Difference") +
  xlab("Rank Difference") +
  ylab("Median Dominance Ratio")
    
linear_mod <- lm(median_DR ~ Rank_Diff, data = rd2)

summary(linear_mod)
