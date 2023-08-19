library(readxl)
library(xlsx)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(xml2)
library(rvest)
library(data.table)
library(stringr)
library(assertthat)

# -------- FUNCTION retrieve TV from game  -------

retrieve_TV_from_game <- function(individual_game_link) {
  try_catch_result <- tryCatch({read_html(individual_game_link)}, error = function(e) {return(NA)})
  if (is.na(try_catch_result)) {return(NA)} 
  else {
  individual_game_info_tibble <- read_html(individual_game_link) %>%
    html_nodes(".GameInfo :nth-child(1)") %>%
    html_text() %>% 
    as_tibble()
  info_long_string <- as.character(individual_game_info_tibble$value[5])
  coverage_string_loc <- gregexpr("Coverage", info_long_string)[[1]][1]
  if (coverage_string_loc == -1) {
    return(NA)
  } else {
  coverage_string <- substring(info_long_string, coverage_string_loc + 10, nchar(info_long_string))
  return(coverage_string)
  }
  }
}

# -------- FUNCTION retrieve TV count from season link -------

retrieve_channel_counts_from_szn <- function(individual_season_link) {
  score_links_tibble <- read_html(individual_season_link) %>%
    html_nodes(".ml4 .AnchorLink") %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename("Link" = "value")
  for (i in 1:nrow(score_links_tibble)) {
    score_links_tibble$Coverage[i] <- retrieve_TV_from_game(score_links_tibble$Link[i])
  }
  channel_tibble <- score_links_tibble %>% select(Coverage)
  if (nrow(channel_tibble) == 0) {
    return(data.frame(Coverage = NA, n = NA))
  } else {
  for (i in 1:nrow(channel_tibble)) {
    if (is.na(channel_tibble$Coverage[i])) {}
    else {
      slash_loc <- gregexpr("/ESPN+", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "ESPN+"))
      }
      slash_loc_2 <- gregexpr("\\|ESPN+", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc_2 != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_2 - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "ESPN+"))
      }
      slash_loc_3 <- gregexpr("/BTN2", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc_3 != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_3 - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "BTN2"))
      }
      slash_loc_4 <- gregexpr("/FULLCT", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc_4 != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_4 - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "FULLCT"))
      }
      slash_loc_5 <- gregexpr("/ACCN", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc_5 != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_5 - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "ACCN"))
      }
      slash_loc_6 <- gregexpr("/FOXCS", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc_6 != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_6 - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "FOXCS"))
      }
      slash_loc_7 <- gregexpr("/FSAZ", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc_7 != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_7 - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "FSAZ"))
      }
      slash_loc_8 <- gregexpr("/FSW2", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc_8 != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_8 - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "FSW2"))
      }
      slash_loc_9 <- gregexpr("/LHN", channel_tibble$Coverage[i])[[1]][1]
      if (slash_loc_9 != -1) {
        channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_9 - 1)
        channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "LHN"))
      }
    }}
  channel_counts <- channel_tibble %>% count(Coverage)
  return(channel_counts)}
}

# -------- lapply FUNCTION retrieve TV count from season link -------

lapply_retrieve_channel_counts_from_szn <- function(individual_season_link) {
  score_links_tibble <- read_html(individual_season_link) %>%
    html_nodes(".ml4 .AnchorLink") %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename("Link" = "value")
  score_links_tibble <- score_links_tibble %>%
    mutate(Coverage = lapply(Link, retrieve_TV_from_game))
  # for (i in 1:nrow(score_links_tibble)) {
  #   score_links_tibble$Coverage[i] <- retrieve_TV_from_game(score_links_tibble$Link[i])
  # }
  channel_tibble <- score_links_tibble %>% select(Coverage)
  if (nrow(channel_tibble) == 0) {
    return(data.frame(Coverage = NA, n = NA))
  } else {
    for (i in 1:nrow(channel_tibble)) {
      #print(paste0("channel_tibble$Coverage[i] is: ", channel_tibble$Coverage[i]))
      if (is.na(channel_tibble$Coverage[i])) {}
      else {
        slash_loc <- gregexpr("/ESPN+", channel_tibble$Coverage[i])[[1]][1]
        if (slash_loc != -1) {
          channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc - 1)
          channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "ESPN+"))
        }
      }}
    for (i in 1:nrow(channel_tibble)) {
      if (is.na(channel_tibble$Coverage[i])) {}
      else {
        slash_loc_2 <- gregexpr("\\|ESPN+", channel_tibble$Coverage[i])[[1]][1]
        if (slash_loc_2 != -1) {
          channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_2 - 1)
          channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "ESPN+"))
        }
      }}
    for (i in 1:nrow(channel_tibble)) {
      if (is.na(channel_tibble$Coverage[i])) {}
      else {
        slash_loc_3 <- gregexpr("/BTN2", channel_tibble$Coverage[i])[[1]][1]
        if (slash_loc_3 != -1) {
          channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_3 - 1)
          channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "BTN2"))
        }
      }}
    for (i in 1:nrow(channel_tibble)) {
      if (is.na(channel_tibble$Coverage[i])) {}
      else {
        slash_loc_4 <- gregexpr("/FULLCT", channel_tibble$Coverage[i])[[1]][1]
        if (slash_loc_4 != -1) {
          channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_4 - 1)
          channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "FULLCT"))
        }
      }}
    channel_counts <- channel_tibble %>% count(Coverage)
    return(channel_counts)}
}

# -------- FUNCTION retrieve channel vector (horizontal vector) for one season  (input name and season) ----------

retrieve_horizontal_vector <- function(name, season) {
  name_szn_empty_data_frame <- data.frame(Team_Name = name,
                                          Season = season,
                                          ABC = 0,
                                          ACCN = 0,
                                          ACCNX = 0,
                                          AXS = 0,
                                          BIG12 = 0,
                                          BTN = 0,
                                          BTN2 = 0,
                                          CBS = 0,
                                          CBSSN = 0,
                                          CCSN = 0,
                                          ESPN = 0,
                                          ESPN_PLUS = 0,
                                          ESPN2 = 0,
                                          ESPN3 = 0,
                                          ESPNN = 0,
                                          ESPNU = 0,
                                          FOX = 0,
                                          FOXCS = 0,
                                          FS1 = 0,
                                          FS2 = 0,
                                          FSAZ = 0,
                                          FSW2 = 0,
                                          FSN = 0,
                                          FULLCT = 0,
                                          LHN = 0,
                                          NBATV = 0,
                                          NBCSN = 0,
                                          PAC12 = 0,
                                          Peacock = 0,
                                          ROOT = 0,
                                          SECN = 0,
                                          SECN_ALT = 0,
                                          SECN_PLUS = 0,
                                          TBS = 0,
                                          TMTN = 0,
                                          TNT = 0,
                                          truTV = 0,
                                          USA_Net = 0,
                                          No_Coverage = 0)
  team_espn_id <- espn_id_table$ESPN_ID[which(espn_id_table$Team_Name == name)]
  vertical_channel_table <- retrieve_channel_counts_from_szn(paste0("https://www.espn.com/mens-college-basketball/team/schedule/_/id/",team_espn_id,"/season/", season))
  if ("ABC" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ABC[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ABC")]
  }
  if ("ACCN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ACCN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ACCN")]
  }
  if ("ACCNX" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ACCNX[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ACCNX")]
  }
  if ("AXS" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$AXS[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "AXS")]
  }
  if ("BIG12" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$BIG12[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "BIG12")]
  }
  if ("BTN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$BTN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "BTN")]
  }
  if ("BTN2" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$BTN2[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "BTN2")]
  }
  if ("CBS" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$CBS[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "CBS")]
  }
  if ("CBSSN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$CBSSN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "CBSSN")]
  }
  if ("CCSN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$CCSN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "CCSN")]
  }
  if ("ESPN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ESPN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ESPN")]
  }
  if ("ESPN+" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ESPN_PLUS[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ESPN+")]
  }
  if ("ESPN2" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ESPN2[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ESPN2")]
  }
  if ("ESPN3" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ESPN3[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ESPN3")]
  }
  if ("ESPNN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ESPNN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ESPNN")]
  }
  if ("ESPNU" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ESPNU[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ESPNU")]
  }
  if ("FOX" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$FOX[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "FOX")]
  }
  if ("FOXCS" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$FOXCS[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "FOXCS")]
  }
  if ("FS1" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$FS1[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "FS1")]
  }
  if ("FS2" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$FS2[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "FS2")]
  }
  if ("FSAZ" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$FSAZ[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "FSAZ")]
  }
  if ("FSW2" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$FSW2[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "FSW2")]
  }
  if ("FSN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$FSN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "FSN")]
  }
  if ("FULLCT" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$FULLCT[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "FULLCT")]
  }
  if ("LHN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$LHN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "LHN")]
  }
  if ("NBA TV" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$NBATV[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "NBA TV")]
  }
  if ("NBCSN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$NBCSN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "NBCSN")]
  }
  if ("PAC12" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$PAC12[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "PAC12")]
  }
  if ("Peacock" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$Peacock[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "Peacock")]
  }
  if ("ROOT" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$ROOT[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "ROOT")]
  }
  if ("SECN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$SECN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "SECN")]
  }
  if ("SECN Alt" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$SECN_ALT[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "SECN Alt")]
  }
  if ("SECN+" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$SECN_PLUS[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "SECN+")]
  }
  if ("TBS" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$TBS[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "TBS")]
  }
  if ("TMTN" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$TMTN[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "TMTN")]
  }
  if ("TNT" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$TNT[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "TNT")]
  }
  if ("truTV" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$truTV[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "truTV")]
  }
  if ("USA Net" %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$USA_Net[1] <- vertical_channel_table$n[which(vertical_channel_table$Coverage == "USA Net")]
  }
  if (NA %in% vertical_channel_table$Coverage) {
    name_szn_empty_data_frame$No_Coverage[1] <- vertical_channel_table$n[which(is.na(vertical_channel_table$Coverage))]
  }
  return(name_szn_empty_data_frame)
}

# Goal: a 1 x n vector where n is the number of possible channels that could have been broadcasted on 
# I think start with that vector (need to find out all possible channels first but we'll see)
# and for each season of each team, add a similar vector where most will be 0 but some will be nonzero
# start by getting the channel table from the function I have already
# then go through every channel and do the following
# -- use %in% to determine if that team played on a certain channel
# -- if they did, then find the row number of that channel in the table using which
# -- get the number of times they played on that channel and add that to the correct column in the big table

# -------- Northwestern total coverage attempt --------

overall_base_data_frame <- data.frame(Team_Name = character(),
                                      Season = numeric(),
                                      ABC = numeric(),
                                      ACCN = numeric(),
                                      ACCNX = numeric(),
                                      BIG12 = numeric(),
                                      BTN = numeric(),
                                      CBS = numeric(),
                                      CBSSN = numeric(),
                                      ESPN = numeric(),
                                      ESPN_PLUS = numeric(),
                                      ESPN2 = numeric(),
                                      ESPN3 = numeric(),
                                      ESPNN = numeric(),
                                      ESPNU = numeric(),
                                      FOX = numeric(),
                                      FS1 = numeric(),
                                      FS2 = numeric(),
                                      LHN = numeric(),
                                      PAC12 = numeric(),
                                      Peacock = numeric(),
                                      SECN = numeric(),
                                      SECN_PLUS = numeric(),
                                      TBS = numeric(),
                                      TNT = numeric(),
                                      truTV = numeric(),
                                      USA_Net = numeric(),
                                      No_Coverage = numeric())

for (i in 2005:2022) {
  print(paste0("We are on season --- ", i))
  overall_base_data_frame <- rbind(overall_base_data_frame, retrieve_horizontal_vector("Northwestern", i))
}
view(overall_base_data_frame)

# -------- All teams coverage code ----------

all_teams_base_tibble <- data.frame(Team_Name = character(),
                                    Season = numeric(),
                                    ABC = numeric(),
                                    ACCN = numeric(),
                                    ACCNX = numeric(),
                                    AXS = numeric(),
                                    BIG12 = numeric(),
                                    BTN = numeric(),
                                    BTN2 = numeric(),
                                    CBS = numeric(),
                                    CBSSN = numeric(),
                                    CCSN = numeric(),
                                    ESPN = numeric(),
                                    ESPN_PLUS = numeric(),
                                    ESPN2 = numeric(),
                                    ESPN3 = numeric(),
                                    ESPNN = numeric(),
                                    ESPNU = numeric(),
                                    FOX = numeric(),
                                    FOXCS = numeric(),
                                    FS1 = numeric(),
                                    FS2 = numeric(),
                                    FSAZ = numeric(),
                                    FSW2 = numeric(),
                                    FSN = numeric(),
                                    FULLCT = numeric(),
                                    LHN = numeric(),
                                    NBATV = numeric(),
                                    NBCSN = numeric(),
                                    PAC12 = numeric(),
                                    Peacock = numeric(),
                                    ROOT = numeric(),
                                    SECN = numeric(),
                                    SECN_ALT = numeric(),
                                    SECN_PLUS = numeric(),
                                    TBS = numeric(),
                                    TMTN = numeric(),
                                    TNT = numeric(),
                                    truTV = numeric(),
                                    USA_Net = numeric(),
                                    No_Coverage = numeric())

# Basic Loop (w/ no restart)

for (i in 84:nrow(espn_id_table)) {
  for (j in 2005:2022) {
    print(paste0("Team: ", i, " (",espn_id_table$Team_Name[i],") --- Season: ", j, " --- Time is: ", format(Sys.time(),
                                                                                                       "%H:%M:%S")))
    team_i_szn_j_tibble <- retrieve_horizontal_vector(espn_id_table$Team_Name[i],j)
    all_teams_base_tibble <- rbind(all_teams_base_tibble, team_i_szn_j_tibble)
  }
}

# Loop that should restart on its own when an error is reached (only 2011 - 2022)

for (i in 231:275) {
  for (j in 2011:2022) {
    print(paste0("Team: ", i, " (",espn_id_table$Team_Name[i],") --- Season: ", j, " --- Time is: ", format(Sys.time(),
                                                                                                            "%H:%M:%S")))
    repeat {
      team_i_szn_j_tibble <- tryCatch({retrieve_horizontal_vector(espn_id_table$Team_Name[i],j)},
                                      error = function(e) {return("There was an error")})
      if (!is.string(team_i_szn_j_tibble)) {
        break
      }
      print(paste0("---------------- Error in the code. Retrying team: ", i, " and Season: ", j, " ---------------------"))
    }
    all_teams_base_tibble <- rbind(all_teams_base_tibble, team_i_szn_j_tibble)
  }
}

cleaned_all_teams_base_tibble <- unique(all_teams_base_tibble) %>%
  filter(Season >= 2011) %>%
  filter(Team_Name != "Temple")

write_csv(cleaned_all_teams_base_tibble, "/Users/viktorostlund/Desktop/coverage_231_249.csv")

# -------- CSV FILE AGGREGATION INTO TOTAL DATASET --------

espn_coverage_1_230 <- read_csv("/Users/viktorostlund/Desktop/coverage_1_230.csv")
espn_coverage_231_249 <- read_csv("/Users/viktorostlund/Desktop/coverage_231_249.csv")
espn_coverage_250_253 <- read_csv("/Users/viktorostlund/Desktop/coverage_250_253.csv")
espn_coverage_254_275 <- read_csv("/Users/viktorostlund/Desktop/coverage_254_275.csv")
espn_coverage_276_300 <- read_csv("/Users/viktorostlund/Desktop/coverage_276_300.csv")
espn_coverage_301_310 <- read_csv("/Users/viktorostlund/Desktop/coverage_301_310.csv")

espn_coverage_total_dataset <- rbind(espn_coverage_1_230,
                                     espn_coverage_231_249,
                                     espn_coverage_250_253,
                                     espn_coverage_254_275,
                                     espn_coverage_276_300,
                                     espn_coverage_301_310)

# Testing that the dataset makes sense
manipulations <- espn_coverage_total_dataset %>%
  unique() %>%
  count(Team_Name) %>%
  filter(n != 12)

write_csv(espn_coverage_total_dataset, "/Users/viktorostlund/Desktop/espn_coverage_total_dataset.csv")

# -------- Single year total channel retrieval (to find the slash needs) --------

espn_id_table <- read_csv("/Users/viktorostlund/Desktop/espn_team_ids.csv") %>%
  mutate(ESPN_ID = as.character(ESPN_ID))

base_channel_tibble <- data.frame()
for (i in 181:nrow(espn_id_table)) {
  print(paste0("I am currently at team: ", i, " --- and the time is: ", format(Sys.time(),
                                                                               "%H:%M:%S")))
  team_szn_link <- paste0("https://www.espn.com/mens-college-basketball/team/schedule/_/id/",
                         espn_id_table$ESPN_ID[i],
                         "/season/2023")
  team_i_channel_tibble <- lapply_retrieve_channel_counts_from_szn(team_szn_link)
  base_channel_tibble <- rbind(base_channel_tibble, team_i_channel_tibble)
}
cleaned_base_channel_tibble <- aggregate(n ~ Coverage, data = base_channel_tibble, sum)
view(cleaned_base_channel_tibble)

# -------- DEBUG HELP One team one season scrape (step by step return vertical vector) -------

team_schedule_page <- "https://www.espn.com/mens-college-basketball/team/schedule/_/id/2065/season/2022"
team_HTML <- read_html(team_schedule_page)
score_nodes <- html_nodes(team_HTML, ".ml4 .AnchorLink")
score_links_tibble <- html_attr(score_nodes, "href") %>%
  as_tibble() %>%
  rename("Link" = "value") %>%
  mutate(Coverage = NA)
for (i in 1:nrow(score_links_tibble)) {
  score_links_tibble$Coverage[i] <- retrieve_TV_from_game(score_links_tibble$Link[i])
}
channel_tibble <- score_links_tibble %>% select(Coverage)
for (i in 1:nrow(channel_tibble)) {
  if (is.na(channel_tibble$Coverage[i])) {}
  else {
    slash_loc <- gregexpr("/ESPN+", channel_tibble$Coverage[i])[[1]][1]
    if (slash_loc != -1) {
      channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc - 1)
      channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "ESPN+"))
    }
  }}
for (i in 1:nrow(channel_tibble)) {
  if (is.na(channel_tibble$Coverage[i])) {}
  else {
    slash_loc_2 <- gregexpr("\\|ESPN+", channel_tibble$Coverage[i])[[1]][1]
    if (slash_loc_2 != -1) {
      channel_tibble$Coverage[i] <- substring(channel_tibble$Coverage[i], 1, slash_loc_2 - 1)
      channel_tibble <- rbind(channel_tibble, data.frame(Coverage = "ESPN+"))
    }
  }}
channel_counts <- channel_tibble %>% count(Coverage)
view(channel_counts)

# -------- NOTES ------
# Need to get all the channel options from all of the years (maybe will just have to stick with the 40 mins per season and suck it up)
# one I need to add is ROOT, along with FULLCT (including accomodating for the slash), and NBCSN, and BTN2 (with slash), CCSN
# Then once i have all of the possible channels in the list and in that base dataframe and within the code, I go team by team and make
# the dataset using the retrieve_horizontal_vector function

# What am I looking for?
# I think for every year I want a count of the times that team had a game broadcasted on
# National television (NBC, CBS, ABC, FOX)
# Cable television (TNT, TBS, truTV)
# ESPN networks (ESPN, ESPN2, ESPNN, ESPNU)
# Other sports networks (CBSSN, FS1, FS2, NBCSN)
# Conference networks (BTN, BTN2, ACCN, BIG12, SECN)
# School specific channels (LHN)
# Streaming (ESPN3, ESPN+, BTN+, ACCNX, )
# And then if I end up aggregating only, I want the number of games played on national television
# (those top four networks) along with number of games played 
# on cable sports channels that are not conference-specific
# want to include streaming
# I think initially each row of the dataset should have season, team name, and then a bunch of columns
# each of which shows the number of times that team played on those channels

# First: Maybe go through all of the ESPN pages and all of the years and get every single channel that a game
# could have been on across all years for all teams
# then write code to filter through all of that and split the double channels and such...?
# or I could just do it progressively
# seems like this webscraping is going take a while

# Plan of action
# - figure out code to strip the ESPN/ESPN+ of the slash and second channel

# One team entire history (produce number of times on a certain channel per year for a team)

# combine all of those into a huge database
