#Loading the packages
library(dplyr)
library(tidyr)
library(forcats)
library(readr)
library(ggplot2)
library(janitor)

#Importing the dataset
prem_matches <- read.csv("soccer21-22.csv")

#Exploring the dataset
tibble(prem_matches)
str(prem_matches)
colnames(prem_matches)

#Checking for and removing duplicates
prem_matches <- distinct(prem_matches)

#Changing the date from character to date format
prem_matches$Date <- dmy(prem_matches$Date)

#Cleaning and replacing the column names in the dataset
#Changes all column names to lower case
prem_matches <- clean_names(prem_matches)

#replacing abbreviations in the column names to make it more readable
prem_matches_cleaned <- prem_matches %>%
  select(date, home_team, away_team, ft_home_goals = fthg, ft_away_goals = ftag,
         ft_result = ftr, ht_home_goals = hthg, ht_away_goals = htag, ht_results = htr,
         referee, home_shots = hs, away_shots = as, home_shots_on_target = hst,
         away_shots_on_target = ast, home_fouls = hf, away_fouls = af, home_corners = hc,
         away_corners = ac, home_yellows = hy, away_yellows = ay, home_reds = hr, away_reds = ar)

#Combining home and away stats by matches
prem_matches_combined <- prem_matches %>%
  summarise(date, home_team, away_team, goals_scored = fthg + ftag, result = ftr,
            referee, total_shots = hs + as, shots_on_target = hst + ast,
            fouls_committed = hf + af, corner_count = hc + ac, bookings = hy+ay+hr+ar, yellow_cards = hy+ay, red_cards = hr+ar)

#List All the Teams in the League
distinct(prem_matches,football_clubs = home_team) %>%
  arrange(football_clubs)

#Goal and shots breakdown
prem_matches_combined %>% 
  summarise(total_goals = sum(goals_scored),total_shots = sum(total_shots),
            total_shots_on_target = sum(shots_on_target),
            conversion_rate = total_goals/total_shots*100)

#Team goals Visualization
ggplot(data = prem_matches_combined, aes(x = date, y = goals_scored)) +
  geom_col(width = 4, fill = "#38003c") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  xlab("Month") + ylab("Goals Scored") + labs(title = "Goals by Month")

#Referee Breakdown
prem_matches_combined %>%
  summarise(referees_used = sum(count(distinct(prem_matches, referee))),
            total_fouls = sum(fouls_committed),
            total_bookings = sum(bookings),
            total_yellow_cards = sum(yellow_cards),
            total_red_cards = sum(red_cards))

#Referee data visualization(s)
#Games Officiated
ggplot(data = prem_matches_combined, aes(x = referee)) + 
  geom_bar(fill = "#38003c", color = "#38003c") + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Referee") + ylab("Games Officiated") + 
  labs(title = "Games Officiated by Referee")

#Bookings Issued
ggplot(data = prem_matches_combined, aes(x = referee, y = bookings, fill = bookings)) + 
  geom_col(fill = "#38003c", color = "#38003c") + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Referee") + ylab("Cards Issued") + 
  labs(title = "Bookings Issued by Referees")

#Red Cards Issued
ggplot(data = prem_matches_combined, aes(x = referee, y = red_cards, fill = bookings)) + 
  geom_col(fill = "red", color = "#38003c") + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Referee") + ylab("Card Count") + 
  labs(title = "Red Cards Issued by Referees")

#Teams Goals, wins and losses
#Calculating Wins
wins <- prem_matches_cleaned %>%
  mutate(ft_result = case_when(
    ft_result == "H" ~ home_team,
    ft_result == "A" ~ away_team,
    ft_result == "D" ~ "Draw"))
wins <- wins %>% filter(ft_result != "Draw")

#Visualizing Wins
ggplot(data = wins, aes(x = ft_result)) +
  geom_bar(fill = "#00ff85", color = "#38003c") +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Teams") + ylab("Wins") + labs(title = "Total Games won by each Football team")

#Calculating Losses
losses <- prem_matches_cleaned %>%
  mutate(ft_result = case_when(
    ft_result == "H" ~ away_team,
    ft_result == "A" ~ home_team,
    ft_result == "D" ~ "Draw"))
losses <- losses %>% filter(ft_result != "Draw")

#Visualizing Losses
ggplot(data = losses, aes(x = ft_result)) +
  geom_bar(fill = "#e90052", color = "#38003c") +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Teams") + ylab("Losses") + labs(title = "Total Games lost by each Football Team")

#Calculating Draws
draws <- prem_matches_cleaned %>%
  select(home_team,away_team,ft_result) %>%
  mutate(ft_result = case_when(
    ft_result == "H" ~ home_team,
    ft_result == "A" ~ away_team,
    ft_result == "D" ~ "Draw"))
draws <- draws %>% filter(ft_result == "Draw")
draws <- cbind(draws[3], stack(draws[1:2]))

#Visualizing Draws
ggplot(data = draws, aes(x = values)) +
  geom_bar(fill = "#ffffff", color = "#38003c") +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Teams") + ylab("Draws") + labs(title = "Total Games Drawn by each Football Team")