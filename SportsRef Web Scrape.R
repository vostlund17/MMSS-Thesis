library(readxl)
library(xlsx)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(xml2)
library(rvest)
library(data.table)

# ------------- Generalizable Code Attempt --------------

sportsref_function <- function(sr_name, kenpom_name) {
  school_site <- paste0("https://sports-reference.com/cbb/schools/", sr_name, "/men/")
  school_table <- read_html(school_site) %>%
    html_nodes(".center+ .poptip , .center+ .poptip , td") %>%
    html_text() %>% 
    t() %>%
    as.data.frame()
  school_matrix <- matrix(school_table, nrow = 18, ncol = ncol(school_table) / 18)
  school_data_frame <- as.data.frame(school_matrix) %>%
    t() %>%
    as.data.frame()
  school_tibble <- as_tibble(school_data_frame) %>%
    filter(row_number() != 1) %>%
    mutate(Year_Start = NA,
           Season = V1,
           Team_Name = kenpom_name,
           Conference = as.character(V2),
           W = as.numeric(V3),
           L = as.numeric(V4),
           WPerc = as.numeric(V5),
           CW = as.numeric(V6),
           CL = as.numeric(V7),
           CWPerc = as.numeric(V8),
           SRS = as.numeric(V9),
           SOS = as.numeric(V10),
           PPG = as.numeric(V11),
           PAPG = as.numeric(V12),
           AP_Pre = as.numeric(V13),
           AP_High = as.numeric(V14),
           AP_Final = as.numeric(V15),
           NCAAT = as.character(V16),
           NCAAT_Seed = as.numeric(V17),
           Coach = as.character(V18)) %>%
    select(Year_Start, Season, Team_Name, Conference, W, L, WPerc, CW, CL, CWPerc, SRS, SOS,
           PPG, PAPG, AP_Pre, AP_High, AP_Final, NCAAT, NCAAT_Seed, Coach)
  for (i in 1:nrow(school_tibble)) {
    school_tibble$Year_Start[[i]] <- strsplit(school_tibble$Season[[i]], "-")[[1]][1]
  }
  school_tibble <- school_tibble %>%
    mutate(Year_Start = as.numeric(Year_Start)) %>%
    mutate(Season = Year_Start + 1) %>%
    select(Year_Start, Season, everything()) %>%
    filter(Season >= 2005 & Season <= 2022)
  return(school_tibble)
}

base_tibble <- data.frame()

sportsref_schools_csv <- read_csv("/Users/viktorostlund/Desktop/sportsref_schools.csv")

for (i in 301:310) {
  print(sportsref_schools_csv$sr_name[i])
  print(sportsref_schools_csv$kenpom_name[i])
  one_school_tibble <- sportsref_function(sportsref_schools_csv$sr_name[i], sportsref_schools_csv$kenpom_name[i])
  base_tibble <- rbind(base_tibble, one_school_tibble)
}

base_tibble <- base_tibble %>%
  arrange(desc(Season)) %>%
  mutate(Conference = as.character(Conference),
         Coach = as.character(Coach))
 
view(base_tibble)

write_csv(base_tibble, "/Users/viktorostlund/Desktop/sr_tibble_301-310.csv")

# ------------- Merging of all the datasets --------------

sr_tibble_1_15 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_1-15.csv")
sr_tibble_16_30 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_16-30.csv")
sr_tibble_31_45 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_31-45.csv")
sr_tibble_46_60 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_46-60.csv")
sr_tibble_61_75 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_61-75.csv")
sr_tibble_76_90 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_76-90.csv")
sr_tibble_91_105 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_91-105.csv")
sr_tibble_106_120 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_106-120.csv")
sr_tibble_121_135 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_121-135.csv")
sr_tibble_136_150 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_136-150.csv")
sr_tibble_151_165 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_151-165.csv")
sr_tibble_166_180 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_166-180.csv")
sr_tibble_181_195 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_181-195.csv")
sr_tibble_196_210 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_196-210.csv")
sr_tibble_211_225 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_211-225.csv")
sr_tibble_226_240 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_226-240.csv")
sr_tibble_241_255 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_241-255.csv")
sr_tibble_256_270 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_256-270.csv")
sr_tibble_271_285 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_271-285.csv")
sr_tibble_286_300 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_286-300.csv")
sr_tibble_301_310 <- read_csv("/Users/viktorostlund/Desktop/sr_tibble_301-310.csv")

sports_ref_total_dataset <- rbind(sr_tibble_1_15,
                                  sr_tibble_16_30,
                                  sr_tibble_31_45,
                                  sr_tibble_46_60,
                                  sr_tibble_61_75,
                                  sr_tibble_76_90,
                                  sr_tibble_91_105,
                                  sr_tibble_106_120,
                                  sr_tibble_121_135,
                                  sr_tibble_136_150,
                                  sr_tibble_151_165,
                                  sr_tibble_166_180,
                                  sr_tibble_181_195,
                                  sr_tibble_196_210,
                                  sr_tibble_211_225,
                                  sr_tibble_226_240,
                                  sr_tibble_241_255,
                                  sr_tibble_256_270,
                                  sr_tibble_271_285,
                                  sr_tibble_286_300,
                                  sr_tibble_301_310) %>%
  arrange(desc(Season), Team_Name)

write.csv(sports_ref_total_dataset, "/Users/viktorostlund/Desktop/sports_ref_total_dataset")

# ------------- Northwestern Specific Code ---------------

northwestern_site <- "https://sports-reference.com/cbb/schools/northwestern/men/"

northwestern_table <- read_html(northwestern_site) %>% 
  html_nodes(".center+ .poptip , .center+ .poptip , td") %>%
  html_text() %>% 
  t() %>%
  as.data.frame()

northwestern_matrix <- matrix(northwestern_table, nrow = 18, ncol = ncol(northwestern_table) / 18)

northwestern_data_frame <- as.data.frame(northwestern_matrix) %>% 
  t() %>% 
  as.data.frame()

northwestern_tibble <- as_tibble(northwestern_data_frame) %>%
  filter(row_number() != 1) %>%
  mutate(Year_Start = NA,
         Season = V1,
         Team_Name = "Northwestern",
         Conference = V2,
         W = as.numeric(V3),
         L = as.numeric(V4),
         WPerc = as.numeric(V5),
         CW = as.numeric(V6),
         CL = as.numeric(V7),
         CWPerc = as.numeric(V8),
         SRS = as.numeric(V9),
         SOS = as.numeric(V10),
         PPG = as.numeric(V11),
         PAPG = as.numeric(V12),
         AP_Pre = as.numeric(V13),
         AP_High = as.numeric(V14),
         AP_Final = as.numeric(V15),
         NCAAT = V16,
         NCAAT_Seed = as.numeric(V17),
         Coach = V18) %>%
  select(Year_Start, Season, Team_Name, Conference, W, L, WPerc, CW, CL, CWPerc, SRS, SOS,
         PPG, PAPG, AP_Pre, AP_High, AP_Final, NCAAT, NCAAT_Seed, Coach)

for (i in 1:nrow(northwestern_tibble)) {
  northwestern_tibble$Year_Start[[i]] <- strsplit(northwestern_tibble$Season[[i]], "-")[[1]][1]
}

northwestern_tibble <- northwestern_tibble %>%
  mutate(Year_Start = as.numeric(Year_Start)) %>%
  mutate(Season = Year_Start + 1) %>%
  select(Year_Start, Season, everything()) %>%
  filter(Season >= 2005)

view(northwestern_tibble)
