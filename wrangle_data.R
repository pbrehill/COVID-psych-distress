
library(tidyverse)
library(haven)

library(tidyverse)
library(magrittr)
library(haven)

# Read in OxCGRT data
source('wide_data.R')

# Read in linked ANU Poll data
anupoll_data <- read_dta('linked.dta')


# Specify survey start dates manually
survey_dates <- c(
  "20/01/2020",
  "17/02/2020",
  "14/04/2020",
  "11/05/2020",
  "10/08/2020",
  "12/10/2020",
  "9/11/2020",
  "18/01/2021",
  "12/04/2021",
  "10/08/2021",
  "12/10/2021",
  "17/01/2022",
  "11/04/2022",
  "23/05/2022",
  "8/08/2022",
  "10/10/2022"
) %>% as.Date("%d/%m/%Y")


# Add the survey dates to wave numbers
survey_cutoff <- data.frame(date = survey_dates, wave =  anupoll_data$wave %>% unique() %>% sort())




# Link ANUpoll to OxCGRT
states <- c('AUS_NSW', 'AUS_VIC', 'AUS_QLD', 'AUS_WA', 'AUS_SA', 'AUS_TAS', 'AUS_NT', 'AUS_ACT')

big_df <- raw_df %>%
  select(RegionCode, CityCode, date, `C1`, `C6`, Jurisdiction) %>%
  mutate(noncap = ifelse(RegionCode == 'AUS_ACT',
                         0,
                         as.numeric(str_detect(CityCode, "^AUS_[:digit:]R"))
  )) %>%
  filter(Jurisdiction == "CITY_TOTAL" | 
           (RegionCode == "AUS_ACT" & Jurisdiction == "STATE_TOTAL"))

anupoll_data$RegionCode <- states[anupoll_data$p_state]

all_data <- anupoll_data %>%
  left_join(survey_cutoff, by = "wave") %>%
  left_join(big_df)


# Create variables for helping assign treatment
did_data <- all_data %>%
  group_by(anu_id) %>%
  arrange(anu_id, wave) %>%
  mutate(
    last_C6 = lag(`C6`),
    last_C1 = lag(`C1`),
    next_C6 = lead(`C6`),
    next_C1 = lead(`C1`),
    valid_did_treat_C1 = (last_C1 < 2) & (C1 >= 2),
    valid_did_treat_C6 = (last_C6 < 2) & (C6 >= 2),
    valid_did_con_C1 = (C1 < 2) & (next_C1 >= 2),
    valid_did_con_C6 = (C6 < 2) & (next_C6 >= 2),
    valid_did_end_C1 = (last_C1 >= 2) & (C1 < 2),
    valid_did_end_C6 = (last_C6 >= 2) & (C6 < 2),
    locked_down_C6 = C6 >= 2,
    locked_down_C1 = C1 >= 2,
    distressed = long_k6 >= 19
  ) %>%
  ungroup()
