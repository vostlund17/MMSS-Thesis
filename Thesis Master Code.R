library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(plm)
library(stats)
library(stargazer)

# ----------------- Kenpom Efficiency Dataset -------------------

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

kenpom_dataset_final <- kenpom_useful %>% 
  select(Season, TeamName, AdjEM, RankAdjEMPercentile) %>%
  filter(Season >= 2005)

# ----------------- Retrieving Education Dataset -----------------------

total_education_dataset <- read_csv("/Users/viktorostlund/Desktop/TotalEducationDataset.csv") %>%
  select(c(-1)) # to remove that first column that shows the row number in the table

# ----------------- Retrieving SportsRef Dataset -------------

NCAAT_to_champ <- function(NCAAT_string) {
  if (is.na(NCAAT_string)) {
    return(0)
  } else if (grepl("Won NCAA Tournament National Final", NCAAT_string)) {
    return(1)
  } else {
    return(0)
  }
}

NCAAT_to_dancing <- function(NCAAT_string) {
  if (is.na(NCAAT_string)) {
    return(0)
  } else {
    return(1)
  }
}

NCAAT_to_NCAAT_Wins <- function(NCAAT_string) {
  if (is.na(NCAAT_string)) {
    return(0)
  } else if (grepl("Lost NCAA Tournament First Four", NCAAT_string)) {
    return(0)
  } else if (grepl("Playing NCAA Tournament First Round", NCAAT_string)) {
    return(0)
  } else if (grepl("Lost NCAA Tournament Opening Round", NCAAT_string)) {
    return(0)
  } else if (grepl("Lost NCAA Tournament First Round", NCAAT_string)) {
    return(0)
  } else if (grepl("Lost NCAA Tournament Second Round", NCAAT_string)) {
    return(1)
  } else if (grepl("Lost NCAA Tournament Third Round", NCAAT_string)) {
    return(2)
  } else if (grepl("Lost NCAA Tournament Regional Semifinal", NCAAT_string)) {
    return(2)
  } else if (grepl("Lost NCAA Tournament Regional Final", NCAAT_string)) {
    return(3)
  } else if (grepl("Lost NCAA Tournament National Semifinal", NCAAT_string)) {
    return(4)
  } else if (grepl("Lost NCAA Tournament National Final", NCAAT_string)) {
    return(5)
  } else if (grepl("Won NCAA Tournament National Final", NCAAT_string)) {
    return(6)
  }
}

AP_High_to_Ranked <- function(AP_High) {
  if (is.na(AP_High)) {
    return(0)
  } else {
    return(1)
  }
}

sports_ref_total_dataset <- read_csv("/Users/viktorostlund/Desktop/sports_ref_total_dataset.csv") %>%
  select(-c(1)) %>% # to remove that first column that shows the number of rows in the table
  mutate(Dancing = as.numeric(lapply(NCAAT, NCAAT_to_dancing)),
         NCAAT_Wins = as.numeric(lapply(NCAAT, NCAAT_to_NCAAT_Wins)),
         National_Champion = as.numeric(lapply(NCAAT, NCAAT_to_champ)),
         CBB_Ranked = as.numeric(lapply(AP_High, AP_High_to_Ranked)))

# NCAA_filtered_sports_ref <- read_csv("/Users/viktorostlund/Desktop/sports_ref_total_dataset.csv") %>%
#   filter(!is.na(NCAAT)) %>%
#   filter(NCAAT != "Lost NCAA Tournament First Four") %>%
#   filter(NCAAT != "Playing NCAA Tournament First Round") %>%
#   filter(NCAAT != "Lost NCAA Tournament Opening Round") %>%
#   filter(NCAAT != "Lost NCAA Tournament First Round") %>%
#   filter(NCAAT != "Lost NCAA Tournament First Round*") %>%
#   filter(NCAAT != "Lost NCAA Tournament Second Round") %>%
#   filter(NCAAT != "Lost NCAA Tournament Second Round*") %>%
#   filter(NCAAT != "Lost NCAA Tournament Third Round") %>%
#   filter(NCAAT != "Lost NCAA Tournament Third Round*") %>%
#   filter(NCAAT != "Lost NCAA Tournament Regional Semifinal") %>%
#   filter(NCAAT != "Lost NCAA Tournament Regional Semifinal*") %>%
#   filter(NCAAT != "Lost NCAA Tournament Regional Final") %>%
#   filter(NCAAT != "Lost NCAA Tournament Regional Final*") %>%
#   filter(NCAAT != "Lost NCAA Tournament National Semifinal") %>%
#   filter(NCAAT != "Lost NCAA Tournament National Semifinal*") %>%
#   filter(NCAAT != "Lost NCAA Tournament National Final") %>%
#   filter(NCAAT != "Lost NCAA Tournament National Final*") %>%
#   filter(NCAAT != "Won NCAA Tournament National Final") %>%
#   filter(NCAAT != "Won NCAA Tournament National Final*")
# view(NCAA_filtered_sports_ref)

# ----------------- Retrieving ESPN.com Coverage Dataset -------------------

espn_coverage_total_dataset <- read_csv("/Users/viktorostlund/Desktop/espn_coverage_total_dataset.csv") %>%
  mutate(National_TV = ABC + CBS + FOX) %>%
  mutate(ESPN_Main = ESPN + ESPN2) %>%
  select(Team_Name, Season, National_TV, everything())

# ----------------- Retrieving US News & World Report Rankings -------------------

nat_uni_manip <- function(ranking) {
  if (is.na(ranking)) {
    return(0)
  } else {
    return(151 - ranking)
  }
}

lib_art_manip <- function(ranking) {
  if (is.na(ranking)) {
    return(0)
  } else {
    return(71 - ranking)
  }
}

is_ranked <- function(ranking) {
  if (is.na(ranking)) {
    return(0)
  } else {
    return(1)
  }
}

national_university_rankings_dataset <- read_csv("/Users/viktorostlund/Desktop/National_University_Rankings_Dataset.csv") %>%
  select(Year_Start, IPEDS, Nat_Uni_Ranking)
liberal_arts_rankings_dataset <- read_csv("/Users/viktorostlund/Desktop/Liberal_Arts_Rankings_Dataset.csv") %>%
  select(Year_Start, IPEDS, Lib_Art_Ranking)

# ----------------- Master Dataset (merge kenpom, education, sportsref, espn.com) ------------------

master_dataset <- kenpom_dataset_final %>%
  left_join(total_education_dataset,
            by = c("TeamName" = "Team_Name",
                   "Season" = "Year_End")) %>%
  left_join(sports_ref_total_dataset,
            by = c("TeamName" = "Team_Name",
                   "Season" = "Season",
                   "Year_Start" = "Year_Start")) %>% 
  left_join(espn_coverage_total_dataset,
            by = c("TeamName" = "Team_Name",
                   "Season" = "Season")) %>%
  left_join(national_university_rankings_dataset,
            by = c("UNITID" = "IPEDS",
                   "Year_Start" = "Year_Start")) %>%
  left_join(liberal_arts_rankings_dataset,
            by = c("UNITID" = "IPEDS",
                   "Year_Start" = "Year_Start")) %>%
  mutate(Year_End = Season) %>%
  mutate(Years_Since_2011 = Season - 2011) %>%
  mutate(Nat_Uni_Ranked = as.numeric(lapply(Nat_Uni_Ranking, is_ranked)),
         Nat_Uni_Ranking_Inv = as.numeric(lapply(Nat_Uni_Ranking, nat_uni_manip))) %>%
  mutate(Lib_Art_Ranked = as.numeric(lapply(Lib_Art_Ranking, is_ranked)),
         Lib_Art_Ranking_Inv = as.numeric(lapply(Lib_Art_Ranking, lib_art_manip))) %>%
  select(Year_Start, Year_End, Season, TeamName, Conference, INSTNM, UNITID, 
         Nat_Uni_Ranking, Nat_Uni_Ranked, Nat_Uni_Ranking_Inv,
         Lib_Art_Ranking, Lib_Art_Ranked, Lib_Art_Ranking_Inv, everything()) %>%
  arrange(desc(Season), TeamName) %>%
  filter(Season >= 2011)

# ----------------- PANEL DATA & LAGS CREATION ------------------

master_dataset_with_lags <- master_dataset %>%
  group_by(TeamName) %>%
  # --- Application Numbers ---
  # Total
  mutate(APPLCN_plus_one = dplyr::lag(APPLCN, n = 1, default = NA)) %>%
  mutate(APPLCN_plus_two = dplyr::lag(APPLCN, n = 2, default = NA)) %>%
  mutate(APPLCN_plus_two_minus_plus_one = APPLCN_plus_two - APPLCN_plus_one) %>%
  # Male
  mutate(APPLCNM_plus_one = dplyr::lag(APPLCNM, n = 1, default = NA)) %>%
  mutate(APPLCNM_plus_two = dplyr::lag(APPLCNM, n = 2, default = NA)) %>%
  mutate(APPLCNM_plus_two_minus_plus_one = APPLCNM_plus_two - APPLCNM_plus_one) %>%
  # Female
  mutate(APPLCNW_plus_one = dplyr::lag(APPLCNW, n = 1, default = NA)) %>%
  mutate(APPLCNW_plus_two = dplyr::lag(APPLCNW, n = 2, default = NA)) %>%
  mutate(APPLCNW_plus_two_minus_plus_one = APPLCNW_plus_two - APPLCNW_plus_one) %>%
  # --- Acceptance Rates ---
  # Total
  mutate(DVADM01_plus_one = dplyr::lag(DVADM01, n = 1, default = NA)) %>%
  mutate(DVADM01_plus_two = dplyr::lag(DVADM01, n = 2, default = NA)) %>%
  mutate(DVADM01_plus_two_minus_plus_one = DVADM01_plus_two - DVADM01_plus_one) %>%
  # Male
  mutate(DVADM02_plus_one = dplyr::lag(DVADM02, n = 1, default = NA)) %>%
  mutate(DVADM02_plus_two = dplyr::lag(DVADM02, n = 2, default = NA)) %>%
  mutate(DVADM02_plus_two_minus_plus_one = DVADM02_plus_two - DVADM02_plus_one) %>%
  # Female
  mutate(DVADM03_plus_one = dplyr::lag(DVADM03, n = 1, default = NA)) %>%
  mutate(DVADM03_plus_two = dplyr::lag(DVADM03, n = 2, default = NA)) %>%
  mutate(DVADM03_plus_two_minus_plus_one = DVADM03_plus_two - DVADM03_plus_one) %>%
  # --- Yield ---
  # Total
  mutate(DVADM04_plus_one = dplyr::lag(DVADM04, n = 1, default = NA)) %>%
  mutate(DVADM04_plus_two = dplyr::lag(DVADM04, n = 2, default = NA)) %>%
  mutate(DVADM04_plus_two_minus_plus_one = DVADM04_plus_two - DVADM04_plus_one) %>%
  # Male
  mutate(DVADM05_plus_one = dplyr::lag(DVADM05, n = 1, default = NA)) %>%
  mutate(DVADM05_plus_two = dplyr::lag(DVADM05, n = 2, default = NA)) %>%
  mutate(DVADM05_plus_two_minus_plus_one = DVADM05_plus_two - DVADM05_plus_one) %>%
  # Female
  mutate(DVADM06_plus_one = dplyr::lag(DVADM06, n = 1, default = NA)) %>%
  mutate(DVADM06_plus_two = dplyr::lag(DVADM06, n = 2, default = NA)) %>%
  mutate(DVADM06_plus_two_minus_plus_one = DVADM06_plus_two - DVADM06_plus_one) %>%
  # --- ACT Scores 25th percentile ---
  # Composite
  mutate(ACTCM25_plus_one = dplyr::lag(ACTCM25, n = 1, default = NA)) %>%
  mutate(ACTCM25_plus_two = dplyr::lag(ACTCM25, n = 2, default = NA)) %>%
  mutate(ACTCM25_plus_two_minus_plus_one = ACTCM25_plus_two - ACTCM25_plus_one) %>%
  # English
  mutate(ACTEN25_plus_one = dplyr::lag(ACTEN25, n = 1, default = NA)) %>%
  mutate(ACTEN25_plus_two = dplyr::lag(ACTEN25, n = 2, default = NA)) %>%
  mutate(ACTEN25_plus_two_minus_plus_one = ACTEN25_plus_two - ACTEN25_plus_one) %>%
  # Math
  mutate(ACTMT25_plus_one = dplyr::lag(ACTMT25, n = 1, default = NA)) %>%
  mutate(ACTMT25_plus_two = dplyr::lag(ACTMT25, n = 2, default = NA)) %>%
  mutate(ACTMT25_plus_two_minus_plus_one = ACTMT25_plus_two - ACTMT25_plus_one) %>%
  # --- ACT Scores 75th Percentile ---
  # Composite
  mutate(ACTCM75_plus_one = dplyr::lag(ACTCM75, n = 1, default = NA)) %>%
  mutate(ACTCM75_plus_two = dplyr::lag(ACTCM75, n = 2, default = NA)) %>%
  mutate(ACTCM75_plus_two_minus_plus_one = ACTCM75_plus_two - ACTCM75_plus_one) %>%
  # English
  mutate(ACTEN75_plus_one = dplyr::lag(ACTEN75, n = 1, default = NA)) %>%
  mutate(ACTEN75_plus_two = dplyr::lag(ACTEN75, n = 2, default = NA)) %>%
  mutate(ACTEN75_plus_two_minus_plus_one = ACTEN75_plus_two - ACTEN75_plus_one) %>%
  # Math
  mutate(ACTMT75_plus_one = dplyr::lag(ACTMT75, n = 1, default = NA)) %>%
  mutate(ACTMT75_plus_two = dplyr::lag(ACTMT75, n = 2, default = NA)) %>%
  mutate(ACTMT75_plus_two_minus_plus_one = ACTMT75_plus_two - ACTMT75_plus_one) %>%
  # --- SAT Scores 25th Percentile ---
  # Math
  mutate(SATMT25_plus_one = dplyr::lag(SATMT25, n = 1, default = NA)) %>%
  mutate(SATMT25_plus_two = dplyr::lag(SATMT25, n = 2, default = NA)) %>%
  mutate(SATMT25_plus_two_minus_plus_one = SATMT25_plus_two - SATMT25_plus_one) %>%
  # Verbal Reasoning
  mutate(SATVR25_plus_one = dplyr::lag(SATVR25, n = 1, default = NA)) %>%
  mutate(SATVR25_plus_two = dplyr::lag(SATVR25, n = 2, default = NA)) %>%
  mutate(SATVR25_plus_two_minus_plus_one = SATVR25_plus_one - SATVR25_plus_one) %>%
  # -- SAT Scores 75th Percentile ---
  # Math
  mutate(SATMT75_plus_one = dplyr::lag(SATMT75, n = 1, default = NA)) %>%
  mutate(SATMT75_plus_two = dplyr::lag(SATMT75, n = 2, default = NA)) %>%
  mutate(SATMT75_plus_two_minus_plus_one = SATMT75_plus_two - SATMT75_plus_one) %>%
  # Verbal Reasoning
  mutate(SATVR75_plus_one = dplyr::lag(SATVR75, n = 1, default = NA)) %>%
  mutate(SATVR75_plus_two = dplyr::lag(SATVR75, n = 2, default = NA)) %>%
  mutate(SATVR75_plus_two_minus_plus_one = SATVR75_plus_one - SATVR75_plus_one) %>%
  # --- Basketball & TV Success Variables ---
  # Win Percentage
  mutate(WPerc_1 = dplyr::lead(WPerc, n = 1, default = NA)) %>%
  mutate(WPerc_diff = WPerc - WPerc_1) %>%
  # Conference Win Percentage
  mutate(CWPerc_1 = dplyr::lead(CWPerc, n = 1, default = NA)) %>%
  mutate(CWPerc_diff = CWPerc - CWPerc_1) %>%
  # Rank Adjusted Efficiency Margin Percentile
  mutate(RankAdjEMPercentile_1 = dplyr::lead(RankAdjEMPercentile, n = 1, default = NA)) %>%
  mutate(RankAdjEMPercentile_diff = RankAdjEMPercentile - RankAdjEMPercentile_1) %>%
  # Dancing
  mutate(Dancing_1 = dplyr::lead(Dancing, n = 1, default = NA)) %>%
  mutate(Dancing_diff = Dancing - Dancing_1) %>%
  # NCAAT_Wins
  mutate(NCAAT_Wins_1 = dplyr::lead(NCAAT_Wins, n = 1, default = NA)) %>%
  mutate(NCAAT_Wins_diff = NCAAT_Wins - NCAAT_Wins_1) %>%
  # National Champion
  mutate(National_Champion_1 = dplyr::lead(National_Champion, n = 1, default = NA)) %>%
  mutate(National_Champion_diff = National_Champion - National_Champion_1) %>%
  # National TV
  mutate(National_TV_1 = dplyr::lead(National_TV, n = 1, default = NA)) %>%
  mutate(National_TV_diff = National_TV - National_TV_1) %>%
  # --- Control Variables ---
  # Testing Requirements
  mutate(Test_Req_plus_one = dplyr::lag(Test_Req, n = 1, default = NA)) %>%
  mutate(Test_Req_plus_two = dplyr::lag(Test_Req, n = 2, default = NA)) %>%
  mutate(Test_Req_plus_two_minus_plus_one = Test_Req_plus_two - Test_Req_plus_one) %>%
  # Rankings
  mutate(Nat_Uni_Ranked_plus_one = dplyr::lag(Nat_Uni_Ranked, n = 1, default = NA)) %>%
  mutate(Nat_Uni_Ranked_plus_one_minus_normal = Nat_Uni_Ranked_plus_one - Nat_Uni_Ranked) %>%
  mutate(Lib_Art_Ranked_plus_one = dplyr::lag(Nat_Uni_Ranked, n = 1, default = NA)) %>%
  mutate(Lib_Art_Ranked_plus_one_minus_normal = Lib_Art_Ranked_plus_one - Lib_Art_Ranked) %>%
  # Inverted Rankings
  mutate(Nat_Uni_Ranking_Inv_plus_one = dplyr::lag(Nat_Uni_Ranking_Inv, n = 1, default = NA)) %>%
  mutate(Nat_Uni_Ranking_Inv_plus_one_minus_normal = Nat_Uni_Ranking_Inv_plus_one - Nat_Uni_Ranking_Inv) %>%
  mutate(Lib_Art_Ranking_Inv_plus_one = dplyr::lag(Lib_Art_Ranking_Inv, n = 1, default = NA)) %>%
  mutate(Lib_Art_Ranking_Inv_plus_one_minus_normal = Lib_Art_Ranking_Inv_plus_one - Lib_Art_Ranking_Inv) %>%
  # Tuition
  mutate(TUITION1_plus_one = dplyr::lag(TUITION1, n = 1, default = NA)) %>%
  mutate(TUITION1_plus_one_minus_normal = TUITION1_plus_one - TUITION1) %>%
  mutate(TUITION2_plus_one = dplyr::lag(TUITION2, n = 1, default = NA)) %>%
  mutate(TUITION2_plus_one_minus_normal = TUITION2_plus_one - TUITION2) %>%
  mutate(TUITION3_plus_one = dplyr::lag(TUITION3, n = 1, default = NA)) %>%
  mutate(TUITION3_plus_one_minus_normal = TUITION3_plus_one - TUITION3) %>%
  # Enrollment
  mutate(ENRLT_plus_one = dplyr::lag(ENRLT, n = 1, default = NA)) %>%
  mutate(ENRLT_plus_one_minus_normal = ENRLT_plus_one - ENRLT) %>%
  mutate(ENRLM_plus_one = dplyr::lag(ENRLM, n = 1, default = NA)) %>%
  mutate(ENRLM_plus_one_minus_normal = ENRLM_plus_one - ENRLM) %>%
  mutate(ENRLW_plus_one = dplyr::lag(ENRLW, n = 1, default = NA)) %>%
  mutate(ENRLW_plus_one_minus_normal = ENRLW_plus_one - ENRLW) %>%
  # --- Ordering of Variables ---
  select(Year_Start, Year_End, Season, TeamName, Conference, INSTNM, UNITID, AdjEM, RankAdjEMPercentile,
         APPLCN, APPLCN_plus_one, APPLCN_plus_two, APPLCN_plus_two_minus_plus_one,
         APPLCNM, APPLCNM_plus_one, APPLCNM_plus_two,
         DVADM01, DVADM01_plus_one, DVADM01_plus_two, DVADM01_plus_two_minus_plus_one, 
         ACTCM25, ACTCM25_plus_one, ACTCM25_plus_two, everything())

master_panel_data <- pdata.frame(master_dataset_with_lags, index = c("TeamName", "Season"))

# ----------------- Initial Regressions ------------

# Application Number Regressions

regression_APPLCN <- plm(APPLCN_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                         + Dancing + NCAAT_Wins + National_Champion 
                         + National_TV + ESPN_Main
                         + Nat_Uni_Ranking_Inv_plus_one 
                         + TUITION3 + Test_Req_plus_two + ENRLT,
                         data = master_panel_data, model = "within")
summary(regression_APPLCN)

regression_APPLCNM <- plm(APPLCNM_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion 
                          + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one 
                          + TUITION3 + Test_Req_plus_two + ENRLM,
                          data = master_panel_data, model = "within")
summary(regression_APPLCNM)

regression_APPLCNW <- plm(APPLCNW_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion 
                          + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one 
                          + TUITION3 + Test_Req_plus_two + ENRLW,
                          data = master_panel_data, model = "within")
summary(regression_APPLCNW)

# Acceptance Rate Regressions

regression_DVADM01 <- plm(DVADM01_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion 
                          + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one 
                          + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")
summary(regression_DVADM01)

regression_DVADM02 <- plm(DVADM02_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion 
                          + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one 
                          + TUITION3 + Test_Req_plus_two + ENRLM,
                          data = master_panel_data, model = "within")
summary(regression_DVADM02)

regression_DVADM03 <- plm(DVADM03_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion 
                          + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one 
                          + TUITION3 + Test_Req_plus_two + ENRLW,
                          data = master_panel_data, model = "within")
summary(regression_DVADM03)

# Yield Regressions

regression_DVADM04 <- plm(DVADM04_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion 
                          + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one 
                          + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")
summary(regression_DVADM04)

regression_DVADM05 <- plm(DVADM05_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion 
                          + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one 
                          + TUITION3 + Test_Req_plus_two + ENRLM,
                          data = master_panel_data, model = "within")
summary(regression_DVADM05)

regression_DVADM06 <- plm(DVADM06_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion 
                          + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one 
                          + TUITION3 + Test_Req_plus_two + ENRLW,
                          data = master_panel_data, model = "within")
summary(regression_DVADM06)

# Overall Admissions Statistics Stargazer

stargazer(regression_APPLCN, regression_DVADM01, regression_DVADM04, 
          type = "latex",
          title = "Effect of College Basketball Success and TV Exposure on Overall Admissions Statistics",
          dep.var.labels = c("Total Applications", "Acceptance Rate", "Admissions Yield"),
          digits = 3,
          align = TRUE)

# Application Numbers Stargazer

stargazer(regression_APPLCN, regression_APPLCNM, regression_APPLCNW,
          type = "latex",
          title = "Effect of College Basketball Success and TV Exposure on Application Numbers",
          dep.var.labels = c("Total Applications", "Male Applications", "Female Applications"),
          digits = 3,
          align = TRUE)

# Acceptance Rates Stargazer

stargazer(regression_DVADM01, regression_DVADM02, regression_DVADM03,
          type = "latex",
          title = "Effect of College Basketball Success and TV Exposure on Acceptance Rates",
          dep.var.labels = c("Overall Acceptance Rate", "Male Acceptance Rate", "Female Acceptance Rate"),
          digits = 3,
          align = TRUE)

# Yield Stargazer

stargazer(regression_DVADM04, regression_DVADM05, regression_DVADM06,
          type = "latex",
          title = "Effect of College Basketball Success and TV Exposure on Admissions Yield",
          dep.var.labels = c("Overall Yield", "Male Yield", "Female Yield"),
          digits = 3,
          align = TRUE)

# Regression of Only Win Percentages

regression_WPercentages <- plm(APPLCN_plus_two ~ WPerc + CWPerc + Nat_Uni_Ranking_Inv_plus_one 
                               + TUITION3 + Test_Req_plus_two + ENRLT,
                               data = master_panel_data, model = "within")
summary(regression_WPerc)

# Regression of Only TV Channels # Should do this with the 15-20 most popular networks that aren't super regional

regression_TV_Channels <- plm(APPLCN_plus_two ~ ABC + ACCN + ACCNX + BIG12 + BTN + BTN2 + CBS + CBSSN + CCSN
                              + ESPN + ESPN_PLUS + ESPN2 + ESPN3 + ESPNN + ESPNU
                              + FOX + FOXCS + FS1 + FS2 + FSAZ + FSW2 + FSN + FULLCT + LHN + NBATV + NBCSN + PAC12
                              + Peacock + ROOT + SECN + SECN_ALT + SECN_PLUS + TBS + TMTN + TNT + truTV + USA_Net
                              + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                              data = master_panel_data, model = "within")
summary(regression_TV_Channels)

stargazer(regression_TV_Channels, type = "text")

# ----------------- Test Score Regressions -----------------

# ACT Regressions

regression_ACTCM25 <- plm(ACTCM25_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

regression_ACTCM75 <- plm(ACTCM75_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

regression_ACTEN25 <- plm(ACTEN25_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

regression_ACTEN75 <- plm(ACTEN75_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

regression_ACTMT25 <- plm(ACTMT25_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

regression_ACTMT75 <- plm(ACTMT75_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

# SAT Regressions

regression_SATVR25 <- plm(SATVR25_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

regression_SATVR75 <- plm(SATVR75_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

regression_SATMT25 <- plm(SATMT25_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

regression_SATMT75 <- plm(SATMT75_plus_two ~ WPerc + RankAdjEMPercentile + CBB_Ranked
                          + Dancing + NCAAT_Wins + National_Champion + National_TV + ESPN_Main
                          + Nat_Uni_Ranking_Inv_plus_one + TUITION3 + Test_Req_plus_two + ENRLT,
                          data = master_panel_data, model = "within")

# ACT Test Scores Stargazer
stargazer(regression_ACTCM25, regression_ACTCM75, regression_ACTEN25, regression_ACTEN75, regression_ACTMT25, regression_ACTMT75, 
          type = "latex",
          title = "Effect of College Basketball Success and TV Exposure on The ACT Scores of Future Classes",
          dep.var.labels = c("25th %ile ACT Comp", "75th %ile ACT Comp", 
                             "25th %ile ACT English", "75th %ile ACT English", 
                             "25th %ile ACT Math", "25th %ile ACT Math"),
          digits = 3,
          align = TRUE)

# SAT Test Scores Stargazer
stargazer(regression_SATVR25, regression_SATVR75, regression_SATMT25, regression_SATMT75, 
          type = "latex",
          title = "Effect of College Basketball Success and TV Exposure on The SAT Scores of Future Classes",
          dep.var.labels = c("25th %ile SAT VR", "75th %ile SAT VR",
                             "25th %ile SAT Math", "75th %ile SAT Math"),
          digits = 3,
          align = TRUE)

# ----------------- Differencing Regressions ------------------

diff_regression_APPLCN <- plm(APPLCN_plus_two_minus_plus_one ~ WPerc_diff
                              + CWPerc_diff + RankAdjEMPercentile_diff
                              + Dancing_diff + NCAAT_Wins_diff + National_Champion_diff
                              + National_TV_diff + Test_Req_plus_two_minus_plus_one
                              + Nat_Uni_Ranking_Inv_plus_one_minus_normal
                              + TUITION3_plus_one_minus_normal
                              + ENRLT_plus_one_minus_normal,
                              data = master_panel_data, model = "within")

diff_regression_DVADM01 <- plm(DVADM01_plus_two_minus_plus_one ~ WPerc_diff
                             + CWPerc_diff + RankAdjEMPercentile_diff
                             + Dancing_diff + NCAAT_Wins_diff + National_Champion_diff
                             + National_TV_diff + Test_Req_plus_two_minus_plus_one
                             + Nat_Uni_Ranking_Inv_plus_one_minus_normal
                             + TUITION3_plus_one_minus_normal
                             + ENRLT_plus_one_minus_normal,
                             data = master_panel_data, model = "within")

diff_regression_DVADM04 <- plm(DVADM04_plus_two_minus_plus_one ~ WPerc_diff
                               + CWPerc_diff + RankAdjEMPercentile_diff
                               + Dancing_diff + NCAAT_Wins_diff + National_Champion_diff
                               + National_TV_diff + Test_Req_plus_two_minus_plus_one
                               + Nat_Uni_Ranking_Inv_plus_one_minus_normal
                               + TUITION3_plus_one_minus_normal
                               + ENRLT_plus_one_minus_normal,
                               data = master_panel_data, model = "within")

diff_regression_SATMT75 <- plm(SATMT75_plus_two_minus_plus_one ~ WPerc_diff
                               + CWPerc_diff + RankAdjEMPercentile_diff
                               + Dancing_diff + NCAAT_Wins_diff + National_Champion_diff
                               + National_TV_diff + Test_Req_plus_two_minus_plus_one
                               + Nat_Uni_Ranking_Inv_plus_one_minus_normal
                               + TUITION3_plus_one_minus_normal
                               + ENRLT_plus_one_minus_normal,
                               data = master_panel_data, model = "within")

summary(diff_regression_APPLCN)

summary(diff_regression_DVADM01)

summary(diff_regression_DVADM04)

summary(diff_regression_SATMT75)

# ----------------- Other Regressions ----------------

stargazer(current_regression, type = "text",
          title = "Effect of College Basketball Success and TV Exposure on Acceptance Rates",
          dep.var.labels = c("Acceptance Rate with Two Year Lag"),
          digits = 3,
          align = TRUE)

regression_1 <- plm(APPLCN_plus_two ~ CWPerc + Dancing + National_TV + ENRLT, data = master_panel_data, model = "within")
regression_2 <- plm(APPLCNM_plus_two ~ Dancing + National_TV + ENRLM, data = master_panel_data, model = "within")
regression_3 <- plm(APPLCNW_plus_two ~ Dancing + National_TV + ENRLW, data = master_panel_data, model = "within")

stargazer(regression_1, regression_2, regression_3, type = "text",
          title = "Effect of NCAA Tournament Invitation and National TV Appearances on Application Rates",
          dep.var.labels = c("Total Applications", "Male Applications", "Female Applications"),
          digits = 3,
          align = TRUE)

regression_4 <- plm(APPLCN_plus_two ~ WPerc + AdjEM + Dancing + National_Champion + National_TV + ENRLT, data = master_panel_data, model = "within")
regression_5 <- plm(DVADM01_plus_two ~ WPerc + AdjEM + Dancing + National_Champion + National_TV + ENRLT, data = master_panel_data, model = "within")
regression_6 <- plm(ACTCM25_plus_two ~ WPerc + AdjEM + Dancing + National_Champion + National_TV + ENRLT, data = master_panel_data, model = "within")
regression_7 <- plm(DVADM04_plus_two ~ WPerc + AdjEM + Dancing + National_Champion + National_TV + ENRLT, data = master_panel_data, model = "within")

stargazer(regression_4, regression_5, regression_6, regression_7, type = "text",
          title = "Effect of Win Percentage, Kenpom Efficiency, National Championship, Dancing, and National TV Appearances on Admissions Metrics",
          dep.var.labels = c("Application Number", "Acceptance Rate", "25th Percentile Comp ACT Score", "Yield"),
          digits = 3,
          align = TRUE)

# master_panel_data <- transform(master_panel_data, APPLCN_plus_one = dplyr::lag(APPLCN, 1)) %>%
#   select(Year_Start, Year_End, Season, TeamName, Conference, INSTNM, UNITID, AdjEM, RankAdjEMPercentile,
#          APPLCN, APPLCN_plus_one, everything())

# ----------------- Interesting Graphs for Intro & Appendix

# Best of Both Worlds

master_dataset_2017 <- filter(master_dataset, Season == 2017)

best_of_both_worlds <-
  ggplot(data = master_dataset_2017, aes(x = RankAdjEMPercentile, y = DVADM01)) +
  geom_point(color = "#87CEEB") +
  labs(x = "Percentile in Kenpom.com Adjusted Efficiency Rating",
       y = "Acceptance Rate (%)",
       title = "The Best of Both Worlds: Athletic vs Academic Success in 2017") +
  geom_text(data = filter(master_dataset_2017, TeamName %in% c("Northwestern", "Harvard", "Cornell", "Brown", "Notre Dame",
                                                               "Michigan", "UNC", "Ohio State", "Rice", "Southern", "UTEP", "Coastal Carolina",
                                                               "Kansas", "Oregon", "Gonzaga", "Florida", "Connecticut", "Liberty", "Colgate",
                                                               "Chicago State", "Howard", "Florida Atlantic", "Arizona State", "Alabama",
                                                               "George Washington", "Quinnipiac", "Tulane", "Yale", "Maine", "Dartmouth",
                                                               "San Diego", "Hawaii", "Montana", "Marshall", "Iowa", "Old Dominion")), 
            aes(label = TeamName, vjust = -1)) +
  geom_text(data = filter(master_dataset_2017, TeamName %in% c("Stanford", "Vanderbilt")), 
            aes(label = TeamName, vjust = 2)) +
  geom_text(data = filter(master_dataset_2017, TeamName %in% c("Duke")), 
            aes(label = TeamName, vjust = -1, hjust = -0.25)) +
  theme_bw()
best_of_both_worlds

best_of_both_worlds_2 <-
  ggplot(data = master_dataset_2017, aes(x = RankAdjEMPercentile, y = DVADM01)) +
  geom_point(color = "#87CEEB") +
  labs(x = "Percentile in Kenpom.com Adjusted Efficiency Rating",
       y = "Acceptance Rate",
       title = "The Best of Both Worlds: Athletic vs Academic Success in 2017") +
  geom_text(aes(label = TeamName, vjust = -1))
theme_bw()
best_of_both_worlds_2

# National TV Appearances by Conference for the 2017 Season

tv_apps_conference_dataset_2017 <- master_dataset %>%
  filter(Season == 2017) %>%
  group_by(Conference) %>%
  summarize(Total_National_TV_Appearances = sum(National_TV)) %>%
  arrange(desc(Total_National_TV_Appearances)) %>%
  slice_head(n = 14)

ESPN_apps_conference_dataset_2017 <- master_dataset %>%
  filter(Season == 2017) %>%
  group_by(Conference) %>%
  summarise(Total_ESPN_or_ESPN2_Appearances = sum(ESPN_Main)) %>%
  arrange(desc(Total_ESPN_or_ESPN2_Appearances)) %>%
  slice_head(n = 26)

National_TV_Distribution <-
  ggplot(data = tv_apps_conference_dataset_2017, aes(x = reorder(Conference, -Total_National_TV_Appearances), y = Total_National_TV_Appearances)) +
  geom_col(fill = "#87CEEB") +
  labs(x = "Conference",
       y = "Number of Times a Team from the Conference Appeared on National Television",
       title = "National TV Appearances by Conference during the 2016-2017 Season") +
  geom_text(aes(label = Total_National_TV_Appearances), vjust = -0.4) +
  theme_bw()
National_TV_Distribution

ESPN_TV_Distribution <-
  ggplot(data = ESPN_apps_conference_dataset_2017, aes(x = reorder(Conference, -Total_ESPN_or_ESPN2_Appearances), y = Total_ESPN_or_ESPN2_Appearances)) +
  geom_col(fill = "#FF0033") +
  labs(x = "Conference",
       y = "Number of Times a Team from the Conference Appeared on ESPN or ESPN2",
       title = "ESPN or ESPN2 Appearances by Conference during 2016-2017 Season") +
  geom_text(aes(label = Total_ESPN_or_ESPN2_Appearances), vjust = -0.4) +
  theme_bw()
ESPN_TV_Distribution

# Average Applications Over Time

applications_over_time <- master_dataset %>%
  filter(!is.na(APPLCN)) %>%
  group_by(Season) %>%
  summarise(Average_APPLCN = mean(APPLCN)) %>%
  arrange(desc(Season))

applications_over_time_plot <-
  ggplot(data = applications_over_time, aes(x = Season, y = Average_APPLCN)) +
  geom_line() +
  labs(x = "Year",
       y = "Average Number of Applications Across All Schools in the Dataset",
       title = "Rise in Applications Across the Board Since 2010") +
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
                                2021, 2022),
                     labels = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                                "2020", "2021", "2022"))
  theme_bw()
applications_over_time_plot

# TV Appearances vs Enrollment

apps_enrlt_dataset <- master_dataset %>%
  group_by(TeamName) %>%
  summarise(Total_Nat_TV_Apps = sum(National_TV),
            Total_ESPN_Main_Apps = sum(ESPN_Main),
            Average_ENRLT = mean(ENRLT))

nat_TV_enrlt_plot <-
  ggplot(data = apps_enrlt_dataset, aes(x = Average_ENRLT, y = Total_Nat_TV_Apps)) +
  geom_point() +
  labs(x = "Average Per Class Enrollment",
       y = "Total Number of Appearances on National Television",
       title = "Average Incoming Class Size vs Appearances on National Television") +
  theme_bw()
nat_TV_enrlt_plot

ESPN_enrlt_plot <-
  ggplot(data = apps_enrlt_dataset, aes(x = Average_ENRLT, y = Total_ESPN_Main_Apps)) +
  geom_point(color = "#FF0033") +
  labs(x = "Average Per Class Enrollment",
       y = "Total Number of Appearances on ESPN or ESPN2",
       title = "Average Incoming Class Size vs Appearances on ESPN & ESPN2") +
  theme_bw()
ESPN_enrlt_plot

# ----------------- ggplot2 work ----------------

northwestern_data <- kenpom_ipeds_merged %>% filter(TeamName == "Northwestern")
colgate_data <- kenpom_ipeds_merged %>% filter(TeamName == "Colgate")
loyola_chicago_data <- kenpom_ipeds_merged %>% filter(TeamName == "Loyola Chicago")

northwestern_application_trends <-
  ggplot(data = northwestern_data, aes(x = Year_Start, y = APPLCN)) +
  geom_col(fill = "Purple") +
  labs(x = "Year",
       y = "Number of Applicants",
       title = "Northwestern Applications over the Years") +
  theme_bw()

colgate_application_trends <-
  ggplot(data = colgate_data, aes(x = Year_Start, y = APPLCN)) +
  geom_col(fill = "Maroon") +
  labs(x = "Year",
       y = "Number of Applicants",
       title = "Colgate Applications over the Years") +
  theme_bw()

loyola_chicago_application_trends <-
  ggplot(data = loyola_chicago_data, aes(x = Year_Start, y = APPLCN)) +
  geom_col(fill = "Gold") +
  labs(x = "Year",
       y = "Number of Applicants",
       title = "Loyola Chicago Applications over the Years") +
  theme_bw()

northwestern_application_trends
colgate_application_trends
loyola_chicago_application_trends

# Histogram of Enrollment Size

enrollment_distribution_dataset <- name_ipeds_dataset %>%
  left_join(ADM2021, by = "UNITID") %>%
  select(Team_Name, UNITID, ENRLT)

enrollment_distribution_graph <-
  ggplot(enrollment_distribution_dataset, aes(ENRLT)) +
  geom_histogram(binwidth = 250, fill = "darkcyan", 
                 linetype = "dashed") +
  labs(x = "Enrollment Number", y = "Frequency",
       title = "Histogram of Enrollment Size (of incoming class)") +
  theme_bw()

enrollment_distribution_graph

# Bar Graph of Schools per State

state_distribution_dataset <- name_ipeds_dataset %>%
  left_join(HD2021, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM, STABBR) %>%
  mutate(Year = 2021) %>%
  select(Year, everything()) %>%
  count(STABBR) %>%
  filter(is.na(STABBR) == FALSE)

state_distribution_graph <- 
  ggplot(state_distribution_dataset, aes(x = STABBR, y = n)) +
  geom_col(fill = "grey") +
  labs(x = "State", 
       y = "Number of Schools in Dataset", 
       title = "School Count by State") +
  geom_text(aes(label = n), vjust = -0.4) +
  theme_bw()

state_distribution_graph
