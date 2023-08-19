library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(plm)
library(stats)
library(stargazer)

# National Universities --------

national_university_rankings <- read_csv("/Users/viktorostlund/Desktop/US-News-Rankings-Universities-Through-2023.csv") %>%
  select(`University Name`, `IPEDS ID`, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`, `2014`, `2013`, `2012`, `2011`, `2010`)

base_nat_uni_table <- data.frame(Year_Start = numeric(),
                                 University_Name = character(),
                                 IPEDS = numeric(),
                                 Nat_Uni_Ranking = numeric())

filter_function <- function(ranking, max) {
  if (is.na(ranking)) {
    return(ranking)
  } else if (ranking <= max) {
    return(ranking)
  } else {
    return(NA)
  }
}

for (i in 1:nrow(national_university_rankings)) {
  for (j in 3:ncol(national_university_rankings)) {
    current_row <- data.frame(Year_Start = 2021 + 3 - j,
                              University_Name = national_university_rankings$`University Name`[i],
                              IPEDS = national_university_rankings$`IPEDS ID`[i],
                              Nat_Uni_Ranking = filter_function(national_university_rankings[i,j][[1]], 150))
    base_nat_uni_table <- rbind(base_nat_uni_table, current_row)
  }
}

write_csv(base_nat_uni_table,"/Users/viktorostlund/Desktop/National_University_Rankings_Dataset.csv")

# Liberal Arts Colleges -------

liberal_arts_rankings <- read_csv("/Users/viktorostlund/Desktop/US-News-Rankings-Liberal-Arts-Colleges-Through-2023.csv") %>%
  select(`College Name`, `IPEDS ID`, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`, `2014`, `2013`, `2012`, `2011`, `2010`) %>%
  mutate(`2021` = as.numeric(`2021`),
         `2020` = as.numeric(`2020`),
         `2019` = as.numeric(`2019`),
         `2018` = as.numeric(`2018`),
         `2017` = as.numeric(`2017`),
         `2016` = as.numeric(`2016`),
         `2015` = as.numeric(`2015`),
         `2014` = as.numeric(`2014`),
         `2013` = as.numeric(`2013`),
         `2012` = as.numeric(`2012`),
         `2011` = as.numeric(`2011`),
         `2010` = as.numeric(`2010`))

base_lib_art_table <- data.frame(Year_Start = numeric(),
                                 Lib_Art_Name = character(),
                                 IPEDS = numeric(),
                                 Lib_Art_Ranking = numeric())

for (i in 1:nrow(liberal_arts_rankings)) {
  for (j in 3:ncol(liberal_arts_rankings)) {
    current_row <- data.frame(Year_Start = 2021 + 3 - j,
                              Lib_Art_Name = liberal_arts_rankings$`College Name`[i],
                              IPEDS = liberal_arts_rankings$`IPEDS ID`[i],
                              Lib_Art_Ranking = filter_function(liberal_arts_rankings[i,j][[1]], 70))
    base_lib_art_table <- rbind(base_lib_art_table, current_row)
  }
}

write_csv(base_lib_art_table,"/Users/viktorostlund/Desktop/Liberal_Arts_Rankings_Dataset.csv")



