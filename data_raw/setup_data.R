# Setup Raw data





# SETUP -------------------------------------------------------------------

library(tidyverse)
library(readr)
library(lubridate)
library(uscovid19vacc)

source("data_raw/data_setup_functions.R")



# Vaccination rates -------------------------------------------------------

# YYG data for the first couple months
vacc_us_states_yyg <- readr::read_csv("https://raw.githubusercontent.com/youyanggu/covid19-cdc-vaccination-data/main/aggregated_adjusted.csv")
usethis::use_data(vacc_us_states_yyg)





# Population Age Proportions - By state -----------------------------------

state_pop <- readr::read_csv("data_raw/geodata_territories_2019_statelevel.csv")
state_pop <- state_pop %>%
    bind_rows(tibble(USPS = "US", geoid="00000", pop2019est=sum(state_pop$pop2019est, na.rm=TRUE)))
usethis::use_data(state_pop, overwrite = TRUE)


# Get age breakdown by state
age_5yr <- tidycensus::get_estimates(geography = "state",
                                     product = "characteristics",
                                     breakdown = c("AGEGROUP"),
                                     breakdown_labels = TRUE)




# --->  Need to figure out the territories at some point

max_age_min <- 85
age_l_ <- seq(0,80, by=5)
age_r_ <- seq(4,84, by=5)
ages_ <- c(paste0("Age ",age_l_, " to ", age_r_, " years"), "Age 85 years and older")
age_groups <- paste0(c(age_l_,85),"_", c(age_r_,100))

state_pop_age5yr <- age_5yr %>%
    filter(AGEGROUP %in% ages_) %>%
    full_join(tibble(AGEGROUP=ages_, age=age_groups, age_l=c(age_l_,85), age_r=c(age_r_,100))) %>%
    mutate(geoid=paste0(GEOID, "000")) %>%
    full_join(state_pop) %>%
    filter(!is.na(pop2019est)) %>%
    rename(pop=value) %>%
    dplyr::group_by(geoid) %>%
    dplyr::mutate(prop = round(pop / sum(pop),3)) %>%
    dplyr::ungroup() %>% as_tibble() %>%
    mutate(pop2019 = round(pop2019est*prop,0)) %>%
    mutate(age_mid = (age_l+age_r)/2 + .5)

age_5yr_us <- state_pop_age5yr %>%
    filter(!is.na(age)) %>%
    dplyr::group_by(age, age_l, age_r, age_mid, AGEGROUP) %>%
    dplyr::summarise(pop = sum(pop),
                     pop2019 = sum(pop2019),
                     pop2019est = sum(pop2019est)) %>%
    as_tibble() %>%
    dplyr::mutate(prop = round(pop2019 / sum(pop2019),3))

state_pop_age5yr <- state_pop_age5yr %>%
    bind_rows(age_5yr_us %>% mutate(GEOID="00", geoid="00000", USPS="US",NAME="United States"))

usethis::use_data(state_pop_age5yr, overwrite = TRUE)





# ~10 year age groups
state_pop_age10yr <- transform_pop_agegroups(age_l_ = c(0, 12, 16, seq(25,85, by=10)), max_age = 100)
state_pop_age10yr <- state_pop_age10yr %>% 
    left_join(state_pop_age5yr %>% select(USPS, pop2019est) %>% distinct()) %>%
    mutate(pop2019 = round(pop2019est * prop))
state_pop_age10yr <- state_pop_age10yr %>% select(colnames(state_pop_age5yr))

usethis::use_data(state_pop_age10yr, overwrite = TRUE)






# Age-specific Vaccination, pre-CCI ---------------------------------------


# current data
daily_state_vacc <- get_state_vacc() %>%
    filter(!is.na(dose1))


# From CDC: https://www.cdc.gov/mmwr/volumes/70/wr/mm7005e1.htm - National level
jan_vacc <- tibble::tibble(age=c("0_18","18_29","30_39","40_49","50_64","65_74","75_100"),
                   dose = c(4837,1433086,2207222,2175305,3350610,1732522,2020534),
                   population=NA) %>%
    dplyr::mutate(percent_of_vac = paste0(round(dose/sum(dose)*100,2),"%"),
           percent_coverage=NA,
           date_released = lubridate::as_date("2021-01-14"),
           date_added = lubridate::as_date("2021-01-14"),
           dose = NA) %>%
    dplyr::mutate(link = "https://www.cdc.gov/mmwr/volumes/70/wr/mm7005e1.htm")

feb_vacc <- readr::read_csv("data_raw/state_vacc_2-18.csv") %>% filter(!is.na(age)) %>%
    mutate(date_released = lubridate::mdy(date_released), date_added = lubridate::mdy(date_added))
mar_vacc <- readr::read_csv("data_raw/state_vacc_3-05.csv") %>% filter(!is.na(age)) %>%
    mutate(date_released = lubridate::mdy(date_released), date_added = lubridate::mdy(date_added))
apr_vacc <- readr::read_csv("data_raw/state_vacc_3-25.csv") %>% filter(!is.na(age)) %>%
    mutate(date_released = lubridate::mdy(date_released), date_added = lubridate::mdy(date_added))
may_vacc <- readr::read_csv("data_raw/state_vacc_5-04.csv") %>% filter(!is.na(age)) %>%
    mutate(date_released = lubridate::ymd(date_released), date_added = lubridate::ymd(date_added))
jun_vacc <- readr::read_csv("data_raw/state_vacc_6-03.csv") %>% filter(!is.na(age)) %>%
    mutate(date_released = lubridate::mdy(date_released), date_added = lubridate::mdy(date_added))

idd_vacc_data <- dplyr::bind_rows(feb_vacc %>% dplyr::mutate(month="feb"),
                                  mar_vacc %>% dplyr::mutate(month="mar"),
                                  apr_vacc %>% dplyr::mutate(month="apr"),
                                  may_vacc %>% dplyr::mutate(month="may")) %>%
    filter(state !="North Dakota") %>%
    dplyr::bind_rows(jun_vacc %>% dplyr::mutate(month="jun")) %>%
    dplyr::rename(link = `link (if different)`) %>%
    dplyr::select(-c(X12, X13, X9))



# Clean the data and apply to reported doses administered

data("state_pop_age5yr")
data("state_pop")

jan_vacc_clean <- clean_vacc_age_idd(vacc_data = jan_vacc,
                                     daily_state_vacc=daily_state_vacc,
                                     state_pop_age5yr = state_pop_age5yr,
                                     state_pop = state_pop)

feb_vacc_clean <- clean_vacc_age_idd(vacc_data = feb_vacc,
                                     daily_state_vacc,
                                     state_pop_age5yr = state_pop_age5yr,
                                     state_pop)

mar_vacc_clean <- clean_vacc_age_idd(vacc_data = mar_vacc,
                                     daily_state_vacc,
                                     state_pop_age5yr = state_pop_age5yr,
                                     state_pop)

apr_vacc_clean <- clean_vacc_age_idd(vacc_data = apr_vacc,
                                     daily_state_vacc,
                                     state_pop_age5yr = state_pop_age5yr,
                                     state_pop)

may_vacc_clean <- clean_vacc_age_idd(vacc_data = may_vacc,
                                     daily_state_vacc,
                                     state_pop_age5yr = state_pop_age5yr,
                                     state_pop)

jun_vacc_clean <- clean_vacc_age_idd(vacc_data = jun_vacc,
                                     daily_state_vacc,
                                     state_pop_age5yr = state_pop_age5yr,
                                     state_pop)

idd_vacc_clean <- dplyr::bind_rows(
    jan_vacc_clean %>% dplyr::mutate(month="jan"),
    feb_vacc_clean %>% dplyr::mutate(month="feb"),
    mar_vacc_clean %>% dplyr::mutate(month="mar"),
    apr_vacc_clean %>% dplyr::mutate(month="apr"),
    may_vacc_clean %>% dplyr::mutate(month="may")) %>%
    filter(state !="North Dakota") %>%
    dplyr::bind_rows(jun_vacc_clean %>% dplyr::mutate(month="jun"))

idd_vacc_clean <- idd_vacc_clean %>%
    left_join(state_pop %>% select(USPS, geoid))

usethis::use_data(idd_vacc_clean, overwrite = TRUE)
usethis::use_data(idd_vacc_data, overwrite = TRUE)



# Next transform to 5 year age groups for easier application to outcome probabilities

idd_vacc_5yr <- get_vacc_age_Xyr(vacc_clean = idd_vacc_clean,
                                 state_pop_ageXyr = state_pop_age5yr,
                                 daily_state_vacc = daily_state_vacc)

usethis::use_data(idd_vacc_5yr, overwrite = TRUE)



# 10 year vaccination age groups

idd_vacc_10yr <- get_vacc_age_Xyr(vacc_clean = idd_vacc_clean,
                                  state_pop_ageXyr = state_pop_age10yr,
                                  daily_state_vacc = daily_state_vacc)

usethis::use_data(idd_vacc_10yr, overwrite = TRUE)






# # Raw Vaccination Data ----------------------------------------------------
# git_token <- "AB63TGCIYANL5EQ647HRXH3AYAWZ6"
# age_vacc_data_raw <- readr::read_csv(paste0("https://raw.githubusercontent.com/govex/Covid19-demographics/main/demographics_by_state.csv?token=",
#                                             git_token))
# usethis::use_data(age_vacc_data_raw, overwrite = TRUE)
# 




