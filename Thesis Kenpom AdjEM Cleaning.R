library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)

# ------------------ KENPOM EFFICIENCY DATA -------------------

kenpom_overall_table <- read_excel("/Users/viktorostlund/Desktop/Entire_Kenpom_Dataset.xlsx")

kenpom_year_counts <- kenpom_overall_table %>%
  count(TeamName)

write.csv(kenpom_all_year_teams, "/Users/viktorostlund/Desktop/KenpomAllYearTeams")

kenpom_all_year_teams <- kenpom_year_counts %>%
  filter(n == 21) %>%
  select(TeamName)

kenpom_num_teams_year <- kenpom_overall_table %>% 
  count(Season)

# This is a table that includes the adjusted efficiency margin and AdjEM rank
# per year for every team that has been in the dataset every year since 2002
# season (aka. 2001-2002)
kenpom_useful <- kenpom_overall_table %>%
  select(Season, TeamName, AdjEM, RankAdjEM) %>%
  arrange(Season, RankAdjEM) %>%
  filter(TeamName %in% kenpom_all_year_teams$TeamName) %>%
  left_join(kenpom_num_teams_year, by = "Season")

kenpom_useful$RankAdjEMPercentile <- ((kenpom_useful$n - kenpom_useful$RankAdjEM) / kenpom_useful$n) * 100

kenpom_dataset_final <- kenpom_useful %>% select(Season, TeamName, AdjEM, RankAdjEMPercentile)

Northwestern_data <- kenpom_dataset_final %>% filter(TeamName == "Northwestern") %>% view()

# DONE
