library(tidyverse)
library(magrittr)
library(haven)

# This function just prepends a d to allow variables to be wide_format in R and Stata.
put_d_infront_of_date <- function (data) {
  data_names <- names(data)
  digit_detected <- str_detect(data_names, "^\\d")
  
  new_names <- c('uid', paste0('d', data_names[digit_detected])) %>%
    str_replace_all('-', '_')
  
  names(data) <- new_names
  
  data
}

# Load in OxCGRT data
url1 <- 'https://aus01.safelinks.protection.outlook.com/?url=https%3A%2F%2Foxcgrtportal.azurewebsites.net%2Fapi%2Fcsvdownload%3Ftype%3Dsubnational_australia_imputed&data=04%7C01%7CPatrick.Rehill%40anu.edu.au%7C1185ce043de94f59cb1a08d9f4d2bc6a%7Ce37d725cab5c46249ae5f0533e486437%7C0%7C0%7C637810008199779170%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000&sdata=hJ1SelP64uMZoO21jnZ%2FJvR7CaBPbQ%2BAHZUXYB7brdw%3D&reserved=0'
url2 <- 'https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/Australia/OxCGRT_AUS_latest.csv'

df <- read_csv(url1
               # col_types = 'cccccccnnccnccnccnccnccnccnccncnccncncncnccncncncncnccnccnccncncncncnnnnnnnnnnnnn'
)

names(df) %<>%
  str_replace("(?<=[:upper:][:digit:][:upper:]{0,2})_.{7,}", "")

df %<>%
  mutate(date = as.Date(as.character(Date), '%Y%m%d'),
         uid = paste(Jurisdiction, RegionCode, CityCode, sep = ' '),
         uid = str_replace_all(uid, "NA", ""),
         uid = str_replace(uid, "^T_", "NAT_")
         ) %>%
  group_by(uid) %>%
  mutate(new_cases = ConfirmedCases - lag(ConfirmedCases),
         new_deaths = ConfirmedDeaths - lag(ConfirmedDeaths)) %>%
  ungroup() %>%
  select(-Date) %>%
  select(-contains('_Notes'))

names(df) <- str_trunc(names(df), width = 32, ellipsis = "")

# write_csv(df, 'rawfile.csv')

raw_df <- df

# Write out data for use in another script
write_dta(raw_df, 'latest_data.dta')

