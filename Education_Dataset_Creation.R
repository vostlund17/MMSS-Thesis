library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)

name_ipeds_dataset <- read.csv("/Users/viktorostlund/Desktop/KenpomAllYearTeamsIPEDS.csv")

#------------------ 2021 -------------------

HD2021 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2021.xlsx", 1)
ADM2021 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2021.xlsx", 2)
DRVADM2021 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2021.xlsx", 3)

education_data_2021 <- name_ipeds_dataset %>%
  left_join(HD2021, by = "UNITID") %>%
  left_join(ADM2021, by = "UNITID") %>%
  left_join(DRVADM2021, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2021, Year_End = 2022) %>%
  select(Year_Start, Year_End, everything())

#------------------ 2020 -------------------

HD2020 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2020.xlsx", 1)
ADM2020 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2020.xlsx", 2)
DRVADM2020 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2020.xlsx", 3)

education_data_2020 <- name_ipeds_dataset %>%
  left_join(HD2020, by = "UNITID") %>%
  left_join(ADM2020, by = "UNITID") %>%
  left_join(DRVADM2020, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2020, Year_End = 2021) %>%
  select(Year_Start, Year_End, everything())

# ----------------- 2019 --------------------

HD2019 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2019.xlsx", 1)
ADM2019 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2019.xlsx", 2)
DRVADM2019 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2019.xlsx", 3)

education_data_2019 <- name_ipeds_dataset %>%
  left_join(HD2019, by = "UNITID") %>%
  left_join(ADM2019, by = "UNITID") %>%
  left_join(DRVADM2019, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2019, Year_End = 2020) %>%
  select(Year_Start, Year_End, everything())

# ----------------- 2018 --------------------

HD2018 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2018.xlsx", 1)
ADM2018 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2018.xlsx", 2)
DRVADM2018 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2018.xlsx", 3)

education_data_2018 <- name_ipeds_dataset %>%
  left_join(HD2018, by = "UNITID") %>%
  left_join(ADM2018, by = "UNITID") %>%
  left_join(DRVADM2018, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2018, Year_End = 2019) %>%
  select(Year_Start, Year_End, everything())

# ----------------- 2017 --------------------

HD2017 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2017.xlsx", 1)
ADM2017 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2017.xlsx", 2)
DRVADM2017 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2017.xlsx", 3)

education_data_2017 <- name_ipeds_dataset %>%
  left_join(HD2017, by = "UNITID") %>%
  left_join(ADM2017, by = "UNITID") %>%
  left_join(DRVADM2017, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2017, Year_End = 2018) %>%
  select(Year_Start, Year_End, everything())


# ----------------- 2016 --------------------

HD2016 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2016.xlsx", 1)
ADM2016 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2016.xlsx", 2)
DRVADM2016 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2016.xlsx", 3)

education_data_2016 <- name_ipeds_dataset %>%
  left_join(HD2016, by = "UNITID") %>%
  left_join(ADM2016, by = "UNITID") %>%
  left_join(DRVADM2016, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2016, Year_End = 2017) %>%
  select(Year_Start, Year_End, everything())

# ----------------- 2015 --------------------

HD2015 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2015.xlsx", 1)
ADM2015 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2015.xlsx", 2)
DRVADM2015 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2015.xlsx", 3)

education_data_2015 <- name_ipeds_dataset %>%
  left_join(HD2015, by = "UNITID") %>%
  left_join(ADM2015, by = "UNITID") %>%
  left_join(DRVADM2015, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2015, Year_End = 2016) %>%
  select(Year_Start, Year_End, everything())

# ----------------- 2014 --------------------

HD2014 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2014.xlsx", 1)
ADM2014 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2014.xlsx", 2)
DRVADM2014 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2014.xlsx", 3)

education_data_2014 <- name_ipeds_dataset %>%
  left_join(HD2014, by = "UNITID") %>%
  left_join(ADM2014, by = "UNITID") %>%
  left_join(DRVADM2014, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2014, Year_End = 2015) %>%
  select(Year_Start, Year_End, everything())

# ----------------- 2013 --------------------
# Merging but from a different type of dataset

HD2013 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2013.xlsx", 1)
IC2013 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2013.xlsx", 2)
DRVIC2013 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2013.xlsx", 3)

education_data_2013 <- name_ipeds_dataset %>%
  left_join(HD2013, by = "UNITID") %>%
  left_join(IC2013, by = "UNITID") %>%
  left_join(DRVIC2013, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2013, Year_End = 2014) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")


# ----------------- 2012 ---------------------

HD2012 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2012.xlsx", 1)
IC2012 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2012.xlsx", 2)
DRVIC2012 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2012.xlsx", 3)

education_data_2012 <- name_ipeds_dataset %>%
  left_join(HD2012, by = "UNITID") %>%
  left_join(IC2012, by = "UNITID") %>%
  left_join(DRVIC2012, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2012, Year_End = 2013) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- 2011 ---------------------

HD2011 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2011.xlsx", 1)
IC2011 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2011.xlsx", 2)
DRVIC2011 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2011.xlsx", 3)

education_data_2011 <- name_ipeds_dataset %>%
  left_join(HD2011, by = "UNITID") %>%
  left_join(IC2011, by = "UNITID") %>%
  left_join(DRVIC2011, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2011, Year_End = 2012) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- 2010 ---------------------

HD2010 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2010.xlsx", 1)
IC2010 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2010.xlsx", 2)
DRVIC2010 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2010.xlsx", 3)

education_data_2010 <- name_ipeds_dataset %>%
  left_join(HD2010, by = "UNITID") %>%
  left_join(IC2010, by = "UNITID") %>%
  left_join(DRVIC2010, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2010, Year_End = 2011) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- 2009 ---------------------

HD2009 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2009.xlsx", 1)
IC2009 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2009.xlsx", 2)
DRVIC2009 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2009.xlsx", 3)

education_data_2009 <- name_ipeds_dataset %>%
  left_join(HD2009, by = "UNITID") %>%
  left_join(IC2009, by = "UNITID") %>%
  left_join(DRVIC2009, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2009, Year_End = 2010) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- 2008 ---------------------

HD2008 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2008.xlsx", 1)
IC2008 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2008.xlsx", 2)
DRVIC2008 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2008.xlsx", 3)

education_data_2008 <- name_ipeds_dataset %>%
  left_join(HD2008, by = "UNITID") %>%
  left_join(IC2008, by = "UNITID") %>%
  left_join(DRVIC2008, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2008, Year_End = 2009) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- 2007 ---------------------

HD2007 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2007.xlsx", 1)
IC2007 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2007.xlsx", 2)
DRVIC2007 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2007.xlsx", 3)

education_data_2007 <- name_ipeds_dataset %>%
  left_join(HD2007, by = "UNITID") %>%
  left_join(IC2007, by = "UNITID") %>%
  left_join(DRVIC2007, by = "UNITID") %>%
  mutate(ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2007, Year_End = 2008) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- 2006 ---------------------

HD2006 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2006.xlsx", 1)
IC2006 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2006.xlsx", 2)
DRVIC2006 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2006.xlsx", 3)

education_data_2006 <- name_ipeds_dataset %>%
  left_join(HD2006, by = "UNITID") %>%
  left_join(IC2006, by = "UNITID") %>%
  left_join(DRVIC2006, by = "UNITID") %>%
  mutate(ACTWR25 = NA, ACTWR75 = NA) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2006, Year_End = 2007) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- 2005 ---------------------

HD2005 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2005.xlsx", 1)
IC2005 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2005.xlsx", 2)

education_data_2005 <- name_ipeds_dataset %>%
  left_join(HD2005, by = "UNITID") %>%
  left_join(IC2005, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  mutate(DVIC01 = round((ADMSSN / APPLCN) * 100),
         DVIC02 = round((ADMSSNM / APPLCNM) * 100),
         DVIC03 = round((ADMSSNW / APPLCNW) * 100),
         DVIC04 = round((ENRLT / ADMSSN) * 100),
         DVIC05 = round((ENRLM / ADMSSNM) * 100),
         DVIC06 = round((ENRLW / ADMSSNW) * 100)) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2005, Year_End = 2006) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- 2004 ---------------------

HD2004 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2004.xlsx", 1)
IC2004 <- read_excel("/Users/viktorostlund/Desktop/IPEDS2004.xlsx", 2)

education_data_2004 <- name_ipeds_dataset %>%
  left_join(HD2004, by = "UNITID") %>%
  left_join(IC2004, by = "UNITID") %>%
  mutate(SATWR25 = NA, SATWR75 = NA, ACTWR25 = NA, ACTWR75 = NA) %>%
  mutate(DVIC01 = round((ADMSSN / APPLCN) * 100),
         DVIC02 = round((ADMSSNM / APPLCNM) * 100),
         DVIC03 = round((ADMSSNW / APPLCNW) * 100),
         DVIC04 = round((ENRLT / ADMSSN) * 100),
         DVIC05 = round((ENRLM / ADMSSNM) * 100),
         DVIC06 = round((ENRLW / ADMSSNW) * 100)) %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = 2004, Year_End = 2005) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")
# ----------------- IC Template Code ---------------------
# Merging but from a different type of dataset
# The differences are that the names of the datasets are
# IC & DRVIC as opposed to ADM and DRVADM
# along with inclusion of SATWR and ACTWR

HDYEAR <- read_excel("/Users/viktorostlund/Desktop/IPEDSYEAR.xlsx", 1)
ICYEAR <- read_excel("/Users/viktorostlund/Desktop/IPEDSYEAR.xlsx", 2)
DRVICYEAR <- read_excel("/Users/viktorostlund/Desktop/IPEDSYEAR.xlsx", 3)

education_data_YEAR <- name_ipeds_dataset %>%
  left_join(HDYEAR, by = "UNITID") %>%
  left_join(ICYEAR, by = "UNITID") %>%
  left_join(DRVICYEAR, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVIC01, DVIC02, DVIC03,
         ENRLT, ENRLM, ENRLW,
         DVIC04, DVIC05, DVIC06,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75) %>%
  mutate(Year_Start = YEAR, Year_End = YEAR_PLUS_ONE) %>%
  select(Year_Start, Year_End, everything()) %>%
  rename("DVADM01" = "DVIC01",
         "DVADM02" = "DVIC02",
         "DVADM03" = "DVIC03",
         "DVADM04" = "DVIC04",
         "DVADM05" = "DVIC05",
         "DVADM06" = "DVIC06")

# ----------------- ADM Template Code --------------------

HDYEAR <- read_excel("/Users/viktorostlund/Desktop/IPEDSYEAR.xlsx", 1)
ADMYEAR <- read_excel("/Users/viktorostlund/Desktop/IPEDSYEAR.xlsx", 2)
DRVADMYEAR <- read_excel("/Users/viktorostlund/Desktop/IPEDSYEAR.xlsx", 3)

education_data_YEAR <- name_ipeds_dataset %>%
  left_join(HDYEAR, by = "UNITID") %>%
  left_join(ADMYEAR, by = "UNITID") %>%
  left_join(DRVADMYEAR, by = "UNITID") %>%
  select(Team_Name, UNITID, INSTNM,
         APPLCN, APPLCNM, APPLCNW,
         ADMSSN, ADMSSNM, ADMSSNW,
         DVADM01, DVADM02, DVADM03,
         ENRLT, ENRLM, ENRLW,
         DVADM04, DVADM05, DVADM06,
         SATVR25, SATVR75, SATMT25, SATMT75,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75) %>%
  mutate(Year_Start = YEAR, Year_End = YEAR) %>%
  select(Year_Start, Year_End, everything())


# --------------- TOTAL EDUCATION DATASET ---------------

total_education_dataset <- rbind(education_data_2021,
                                 education_data_2020,
                                 education_data_2019,
                                 education_data_2018,
                                 education_data_2017,
                                 education_data_2016,
                                 education_data_2015,
                                 education_data_2014,
                                 education_data_2013,
                                 education_data_2012,
                                 education_data_2011,
                                 education_data_2010,
                                 education_data_2009,
                                 education_data_2008,
                                 education_data_2007,
                                 education_data_2006,
                                 education_data_2005,
                                 education_data_2004)

write.csv(total_education_dataset, "/Users/viktorostlund/Desktop/TotalEducationDataset.csv")
