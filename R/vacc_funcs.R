# Functions





#' Get Daily State Vaccination
#'
#' @return The output from this function is cleaned, up-to-date cumulative and daily COVID-19 vaccination numbers, by state, with dose 1 and 2 for Pfizer and Moderna vaccines (combined).
#' @export
#'
#' @examples
#' get_state_vacc()
#'
get_state_vacc <- function(){

    # Data from Youyang Gu. for first couple months of vaccination
    # -- we will use data from YYG up to 3/1/2021, then use JHU data (for now)

    data("vacc_us_states_yyg")

    vacc_dat_yyg <- vacc_us_states_yyg %>%
        dplyr::select(USPS = Location,
               date = Date,
               dose1 = administered_dose1_adj,
               dose2 = administered_dose2_adj,
               dose1_daily = administered_dose1_daily_7day_avg_adj,
               dose2_daily = administered_dose2_daily_7day_avg_adj) %>%
        dplyr::arrange(USPS, date)

    # Use data from JHU Covid resource center
    vacc_us_states <- readr::read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv")
    vacc_us_states <- vacc_us_states %>%
        dplyr::filter(Vaccine_Type=="All") %>%
        dplyr::mutate(USPS = tidycensus::fips_codes$state[match(FIPS, as.integer(tidycensus::fips_codes$state_code))]) %>%
        dplyr::filter(!is.na(USPS)) %>%
        dplyr::select(USPS,
               date = Date,
               dose1 = Stage_One_Doses,
               dose2 = Stage_Two_Doses)

    # Deal with Negatives
    vacc_us_states_d1 <- vacc_us_states %>%
        dplyr::filter(!is.na(dose1)) %>%
        dplyr::arrange(USPS, date) %>%
        dplyr::group_by(USPS) %>%
        dplyr::mutate(dose1_daily = diff(c(0,dose1))) %>%
        dplyr::ungroup()

    while(sum(vacc_us_states_d1$dose1_daily<0, na.rm = TRUE)>0){
        vacc_us_states_d1 <- vacc_us_states_d1 %>%
            dplyr::filter(dose1_daily>=0) %>%
            dplyr::group_by(USPS) %>%
            dplyr::mutate(dose1_daily = diff(c(0,dose1))) %>%
            dplyr::ungroup()
    }


    # Dose 2
    vacc_us_states_d2 <- vacc_us_states %>%
        dplyr::filter(!is.na(dose2)) %>%
        dplyr::arrange(USPS, date) %>%
        dplyr::group_by(USPS) %>%
        dplyr::mutate(dose2_daily = diff(c(0,dose2))) %>%
        dplyr::ungroup()

    inds_remove <- which(vacc_us_states_d2$dose2_daily<0)
    inds_remove <- sort(unique(c(inds_remove, inds_remove-1, inds_remove+1)))

    vacc_us_states_d2 <- vacc_us_states_d2[-inds_remove, ]
    vacc_us_states_d2 <- vacc_us_states_d2 %>%
        dplyr::group_by(USPS) %>%
        dplyr::mutate(dose2_daily = diff(c(0,dose2))) %>%
        dplyr::ungroup()

    # manually fix DC
    vacc_us_states_d2 <- vacc_us_states_d2 %>%
        dplyr::filter(!(USPS=="DC" & date>=lubridate::as_date("2021-02-24") & date<=lubridate::as_date("2021-03-07")))

    vacc_us_states_d2 <- vacc_us_states_d2 %>%
        dplyr::group_by(USPS) %>%
        dplyr::mutate(dose2_daily = diff(c(0,dose2))) %>%
        dplyr::ungroup()

    while(sum(vacc_us_states_d2$dose2_daily<0, na.rm = TRUE)>0){
        vacc_us_states_d2 <- vacc_us_states_d2 %>%
            dplyr::filter(dose2_daily>=0) %>%
            dplyr::group_by(USPS) %>%
            dplyr::mutate(dose2_daily = diff(c(0,dose2))) %>%
            dplyr::ungroup()
    }

    vacc_us_states_orig <- vacc_us_states

    # Put it together
    vacc_us_states <- vacc_us_states_d1 %>%
        dplyr::select(-dose2) %>%
        dplyr::full_join(vacc_us_states_d2 %>% dplyr::select(-dose1)) %>%
        dplyr::full_join(expand_grid(USPS = unique(vacc_us_states$USPS), date = seq(min(vacc_us_states$date), max(vacc_us_states$date), by="days"))) %>%
        dplyr::arrange(USPS, date)

    # Combine YYG and JHU

    vacc_us_states <- vacc_us_states %>%
        dplyr::filter(date>=lubridate::as_date("2021-03-01")) %>%
        dplyr::bind_rows(vacc_dat_yyg %>% filter(date<=lubridate::as_date("2021-02-28"))) %>%
        dplyr::mutate(day_index = as.integer(date - lubridate::as_date("2021-03-01") + 1)) %>%
        dplyr::arrange(USPS, date)

    vacc_us_states <- vacc_us_states %>%
        dplyr::full_join(tidyr::expand_grid(USPS = unique(vacc_us_states$USPS), date = seq(min(vacc_us_states$date), max(vacc_us_states$date), by="days"))) %>%
        dplyr::arrange(USPS, date)
    vacc_us_states <- vacc_us_states %>%
        dplyr::mutate(dose1_daily = ifelse(dose1_daily<0, NA, dose1_daily),
               dose2_daily = ifelse(dose2_daily<0, NA, dose2_daily))
    vacc_us_states <- vacc_us_states %>%
        dplyr::mutate(day_index = as.integer(date - min(vacc_us_states$date) + 1))

    vacc_us_states$dose1_daily[is.na(vacc_us_states$dose1_daily)] <- 0
    vacc_us_states$dose2_daily[is.na(vacc_us_states$dose2_daily)] <- 0


    # get 7-day moving average
    vacc_us_7d <- tibble::tibble()
    dates_ <- sort(unique(vacc_us_states$date))
    days <- sort(unique(vacc_us_states$day_index))
    for (d in 1:max(vacc_us_states$day_index)){
        day_min <- max(1,d-3)
        day_max <- min(d+3, max(days))

        vacc_us_ <- vacc_us_states %>%
            dplyr::filter(day_index<=day_max & day_index>=day_min) %>%
            dplyr::group_by(USPS) %>%
            dplyr::summarise(dose1_7d_daily = mean(dose1_daily, na.rm=TRUE),
                      dose2_7d_daily = mean(dose2_daily, na.rm=TRUE)) %>%
            dplyr::mutate(date = dates_[d==days])

        vacc_us_7d <- dplyr::bind_rows(vacc_us_7d, vacc_us_)
    }

    vacc_us_states <- vacc_us_states %>%
        dplyr::full_join(vacc_us_7d) %>%
        dplyr::select(-day_index)

    vacc_us_states <- vacc_us_states %>%
        dplyr::arrange(USPS, date) %>%
        dplyr::group_by(USPS) %>%
        dplyr::mutate(dose1_new = cumsum(dose1_7d_daily),
               dose2_new = cumsum(dose2_7d_daily)) %>%
        dplyr::ungroup()

    vacc_us_states <- vacc_us_states %>%
        dplyr::select(USPS, date,
               dose1=dose1_new, dose2=dose2_new,
               dose1_daily=dose1_7d_daily, dose2_daily=dose2_7d_daily)


    return(vacc_us_states)
}




#' Monotonic smoothing function
#'
#' @param x
#' @param y
#' @param age_groups_r
#'
#' @return
#' @export
#'
#' @examples
smooth_fun <- function(x, y, age_groups_r) {
    x <- c(x, max(x)+5)
    y <- c(y, max(y))
    splinefit_ <- stats::splinefun(x, y, method="hyman")
    diff(c(0, (splinefit_(age_groups_r))))
}





##' Function to transform population data from a worldpop age groups to user-defined age groups
##'
##' @param age_pop_data data pulled from worldpop using `load_worldpop_age`
##' @param max_age max age to include (default is 100)
##' @param age_group_data
##'
##' @return long age population data by admin level 2, in user-defined age groups
##'
##' @import dplyr
##' @importFrom tibble as_tibble
##'
##' @references WorldPop (https://www.worldpop.org/geodata)
##'
##' @export
##'
convert_agegroups <- function(age_pop_data, age_group_data, max_age=100){

    age_pop_data <- age_pop_data %>%
        dplyr::mutate(age_l = as.numeric(age_l),
                      age_r = as.numeric(age_r) + 1, # add 1 for the spline fitting
                      pop = as.numeric(pop)) %>%
        dplyr::arrange(location, age_l) %>%
        group_by(location) %>%
        dplyr::mutate(pop_cum = cumsum(pop)) %>%
        dplyr::ungroup()

    if(!("group" %in% colnames(age_group_data))){
        age_group_data$group <- NA
    }
    age_group_data <- age_group_data %>%
        arrange(location, group, age_l) %>%
        mutate(loc_grp = paste0(location,"_",group))

    # # Adjust max age (change max age of data to match input. so we dont overestimate total population)
    # max_age_data <- max(age_pop_data$age_r)
    # age_pop_data <- age_pop_data %>%
    #     dplyr::mutate(age_r = ifelse(age_r==max_age_data, max_age, age_r))

    age_pop_data <- age_pop_data %>%
        filter(location %in% age_group_data$location)

    age_pop_fit_ <- list()
    loc_grps <- unique(age_group_data$loc_grp)
    for (i in 1:length(loc_grps)){

        age_group_data_tmp <- age_group_data %>% filter(loc_grp==loc_grps[i])
        age_groups_ <- age_group_data_tmp$age
        age_l_ <- as.integer(age_group_data_tmp$age_l)
        age_r_ <- as.integer(age_group_data_tmp$age_r)
        if (min(age_l_)>0){
            age_r_ <- c(min(age_l_)-1, age_r_)
            age_l_ <- c(0,age_l_)
        }
        age_groups_ <- paste0(age_l_,"_",age_r_)

        age_pop_fit_[[i]] <- age_pop_data %>% filter(location==age_group_data_tmp$location[1]) %>%
            tidyr::nest(data = c(age, age_l, age_r, pop, pop_cum)) %>%
            dplyr::mutate(age_groups = list(age_groups_),
                          age_l = list(age_l_),
                          age_r = list(age_r_),
                          pop = purrr::map(data, ~smooth_fun(x=.$age_r, y=.$pop_cum, age_r_+1))) %>%
            tidyr::unnest(c(age_groups, age_l, age_r, pop)) %>%
            tibble::as_tibble() %>%
            dplyr::select(-data) %>%
            dplyr::mutate(group = age_group_data_tmp$group[1])
    }
    age_pop_fit <- data.table::rbindlist(age_pop_fit_) %>% as_tibble()

    return(age_pop_fit)
}






transform_pop_agegroups <- function(age_l_ = c(0, 12, 16, seq(25,85, by=10)),
                                    max_age = 100){

    # load data from package
    data("state_pop_age5yr")

    age_r_ <- c(age_l_[-1]-1, max_age)
    ages_ <- c(paste0("Age ",age_l_, " to ", age_r_, " years"))
    age_groups <- paste0(age_l_,"_", age_r_)

    age_group_dat <- tibble(
        age = age_groups,
        age_l = age_l_,
        age_r = age_r_,
        AGEGROUP = ages_
    )

    state_pop_ageXyr <- get_pop_Xyr_spline(age_data = state_pop_age5yr %>% select(location = USPS, age, age_l, age_r, pop=pop2019),
                                            age_group_data=age_group_dat)
    state_pop_ageXyr <- state_pop_ageXyr %>%
        rename(USPS=location, pop2019=pop) %>%
        mutate(age_mid = (age_l+age_r)/2 + .5) %>%
        left_join(state_pop_age5yr %>% select(USPS, NAME, geoid, GEOID) %>% distinct()) %>%
        left_join(age_group_dat) %>%
        group_by(USPS, NAME, geoid) %>%
        mutate(prop = pop2019 / sum(pop2019, na.rm = TRUE)) %>%
        ungroup() %>%
        distinct() %>%
        as_tibble()

    return(state_pop_ageXyr)

}







# Get Population size for each age group reported

#' Title
#'
#' @param vacc_age_data
#' @param state_pop_ageXyr
#'
#' @return
#' @export
#'
#' @examples
get_pop_vacc_groups <- function(vacc_age_data,
                                state_pop_ageXyr){

    # limit to just the data we want on population and age groups
    if(!("date" %in% colnames(vacc_age_data))){
        vacc_age_data$date <- NA
    }

    # This is the population data we want
    age_pop_data_ <- state_pop_ageXyr %>%
        select(location=USPS, age, age_l, age_r, pop = pop2019) %>%
        filter(!is.na(age)) %>%
        arrange(location, age_l)
    # These are the age groups we want
    age_group_data_ <- vacc_age_data %>%
        select(location=USPS, group=date, age, age_l, age_r) %>%
        distinct() %>%
        filter(!is.na(age_l)) %>%
        arrange(location, group, age_l)

    # Convert age groups to match
    # - this uses a smoothing function to put population into new groups
    new_age_pop <- convert_agegroups(age_pop_data = age_pop_data_, age_group_data=age_group_data_)

    vacc_age_data <- vacc_age_data %>%
        mutate(age_l = as.integer(age_l), age_r=as.integer(age_r)) %>%
        full_join(new_age_pop, by=c("USPS"="location", "date"="group", "age"="age_groups", "age_l"="age_l", "age_r"="age_r"))

    return(vacc_age_data)
}







# Get Population size for each age group reported

#' Title
#'
#' @param vacc_age_data
#' @param state_pop_ageXyr
#' @param rep_state_vacc
#'
#' @return
#' @export
#'
#' @examples
get_vacc_Xyr_spline <- function(vacc_age_data=vacc_clean,
                                state_pop_ageXyr=state_pop_age5yr,
                                daily_state_vacc){

    #daily vacc - for use with getting props
    daily_vacc_ <- daily_state_vacc %>% dplyr::select(USPS, date, dose1)

    state_pop_ageXyr <- state_pop_ageXyr %>% filter(!is.na(age_r))

    age_pop_data_ <- vacc_age_data %>%
        select(location=USPS, date, age, age_l, age_r, prop=prop_vacc_age, dose1, dose1_age) %>%
        filter(!is.na(age_l)) %>%
        mutate(prop = ifelse(is.na(prop), 0, prop))

    vacc_age_data_ <- vacc_age_data %>%
        mutate(prop_of_vacc = prop_of_vacc/(age_r-age_l+1)) %>%
        select(USPS, date, age_l, age_r, prop_of_vacc, prop_vacc_age, dose1, dose1_age)

    tmp_ <- list()
    for (i in 1:nrow(vacc_age_data)){
        if(is.na(vacc_age_data_$age_l[i])){
            next
        }
        tmp_[[i]] <- expand_grid(USPS = vacc_age_data_$USPS[i],
                                 date = vacc_age_data_$date[i],
                                 age = seq(vacc_age_data_$age_l[i], vacc_age_data_$age_r[i], by=1),
                                 prop_vacc_age = vacc_age_data_$prop_vacc_age[i],
                                 dose1 = vacc_age_data_$dose1[i],
                                 dose1_age = vacc_age_data_$dose1_age[i])#,
        #prop_of_vacc = vacc_age_data_$prop_of_vacc[i])
    }
    vacc_1yr <- data.table::rbindlist(tmp_) %>% as_tibble() %>%
        mutate(prop_vacc_age = ifelse(is.na(prop_vacc_age), 0, prop_vacc_age)) %>%
        left_join(vacc_age_data %>% select(USPS, date, state) %>% distinct()) %>%
        mutate(prop_vacc_age = ifelse(age<16,0, prop_vacc_age))

    age_groups <- unique(state_pop_ageXyr$age)
    age_l <- unique(state_pop_ageXyr$age_l)
    age_r <- unique(state_pop_ageXyr$age_r)

    # Get means for locations without data
    vacc_1yr_mean <- vacc_1yr %>%
        group_by(age, date) %>%
        summarise(prop_vacc_age = mean(prop_vacc_age))
    states_missing <- state_pop_ageXyr %>%
        filter(!(USPS %in% unique(vacc_1yr$USPS))) %>%
        select(USPS, state=NAME) %>% distinct() %>%
        mutate(comb=TRUE)
    vacc_1yr_fill <- vacc_1yr_mean %>%
        mutate(comb=TRUE) %>%
        full_join(states_missing) %>%
        select(-comb) %>% as_tibble() %>%
        left_join(daily_vacc_ %>% select(USPS, date, dose1) %>% distinct() %>% as_tibble())

    vacc_Xyr <- vacc_1yr %>%
        bind_rows(vacc_1yr_fill) %>%
        mutate(age_group = cut(age, breaks = c(age_l,101), include.lowest=TRUE, right = F, labels = F)) %>%
        mutate(age_group = age_groups[age_group]) %>%
        group_by(USPS, state, date, age_group, dose1) %>%
        summarise(prop_vacc_age = mean(prop_vacc_age)) %>%
        left_join(state_pop_ageXyr %>% select(age_group=age, USPS, geoid, pop=pop2019, pop_prop=prop)) %>%
        mutate(dose1_age = prop_vacc_age * pop) %>%
        group_by(USPS, date) %>%
        mutate(prop_of_vacc = dose1_age / sum(dose1_age)) %>%
        ungroup() %>%
        separate(age_group, into=c("age_l", "age_r"), sep="_", remove = FALSE) %>%
        mutate(age_l = as.integer(age_l),
               age_r = as.integer(age_r))

    return(vacc_Xyr)
}







# convert to Xyr groups
#' Title
#'
#' @param vacc_clean
#' @param state_pop_ageXyr
#' @param daily_state_vacc
#' @param date_vacc_cum
#'
#' @return
#' @export
#'
#' @examples
get_vacc_age_Xyr <- function(vacc_clean = vacc_clean,
                             state_pop_ageXyr = state_pop_ageXyr,
                             daily_state_vacc = daily_state_vacc){

    #daily vacc - for use with getting props
    daily_vacc_ <- daily_state_vacc %>% select(USPS, date, dose1)

    vacc_clean_Xyr <- get_vacc_Xyr_spline(vacc_age_data=vacc_clean,
                                          state_pop_ageXyr,
                                          daily_state_vacc=daily_vacc_) %>%
        filter(!is.na(age_l))


    return(vacc_clean_Xyr)
}






#' Transform and distribute population to new age groups
#'
#' @param age_data    # Data of population data of interest in some age groups. This should have column names: "location", "age", "age_l", "age_r", "pop"
#' @param age_group_data # Data on new age groups. This should have column names: "age", "age_l", "age_r"
#'
#' @return
#' @export
#'
#' @examples
#'
get_pop_Xyr_spline <- function(age_data, age_group_data){

    age_group_data <- age_group_data %>% filter(!is.na(age_r))

    age_pop_data_ <- age_data %>%
        select(location, age, age_l, age_r, pop) %>%
        filter(!is.na(age_l)) %>%
        mutate(pop = ifelse(is.na(pop), 0, pop)) %>%
        mutate(pop_yr = pop / (age_r - age_l + 1))

    tmp_ <- list()
    for (i in 1:nrow(age_pop_data_)){
        if(is.na(age_pop_data_$age_l[i])){
            next
        }
        tmp_[[i]] <- expand_grid(location = age_pop_data_$location[i],
                                 age = seq(age_pop_data_$age_l[i], age_pop_data_$age_r[i], by=1),
                                 pop = age_pop_data_$pop_yr[i])
    }
    dat_1yr <- data.table::rbindlist(tmp_) %>% as_tibble() %>%
        mutate(pop = ifelse(is.na(pop), 0, pop))

    age_groups <- unique(age_group_data$age)
    age_l <- unique(age_group_data$age_l)
    age_r <- unique(age_group_data$age_r)


    dat_Xyr <- dat_1yr %>%
        mutate(age_group = cut(age, breaks = c(age_l,max(age_r)+1), include.lowest=TRUE, right = F, labels = F)) %>%
        mutate(age_group = age_groups[age_group]) %>%
        group_by(location, age_group) %>%
        summarise(pop = sum(pop)) %>%
        separate(age_group, into=c("age_l", "age_r"), sep="_", remove = FALSE) %>%
        mutate(age_l = as.integer(age_l),
               age_r = as.integer(age_r))

    return(dat_Xyr)
}







# CCI FUNCTIONS -----------------------------------------------------------



#' Title
#'
#' @param git_token Token required to access data
#'
#' @return
#' @export
#'
#' @examples
pull_cci_vacc <- function(git_token = "AB63TGCIYANL5EQ647HRXH3AYAWZ6"){

    age_vacc_data_raw <- readr::read_csv(paste0("https://raw.githubusercontent.com/govex/Covid19-demographics/main/demographics_by_state.csv?token=",
                                                git_token)) %>%
        filter(Category=="Vaccines" & grepl("age", Demo_cat_0)) %>%
        filter(Demo_cat_1!="unknown")

    return(age_vacc_data_raw)
}





#' Clean CCI data, before standardizing
#'
#' @param age_vacc_data_raw Raw data pulled from CCI github repo.
#'
#' @return
#' @export
#'
#' @examples
#'
clean_cci_vacc <- function(age_vacc_data_raw=cci_vacc_data){

    # DATA EXCLUSIONS
    age_vacc_data_raw <- age_vacc_data_raw %>%
        # filter(State!="IL") %>% # Doing a manual fix of the data below
        # filter(!(State=="NJ" & Estimate_type=="total_cumulative")) %>%
        # filter(!(State=="DE" & Estimate_type=="total_cumulative")) %>%
        filter(!(State=="NJ" & Estimate_type=="rate_percent")) %>%  # Drop these because they also have numbers vaccinated
        filter(!(State=="DE" & !(Estimate_type=="total_cumulative" & Metric=="doses_admin"))) %>%
        mutate(Metric = ifelse(State=="DE", "people_initiated", Metric)) %>%
        mutate(Metric = ifelse(State=="LA" & Metric=="people_initiated", "people_partial", Metric))

    # Fix IL
    age_vacc_tmp <- age_vacc_data_raw %>%
        filter(State %in% c("IL")) %>%
        pivot_longer(cols=contains("/"), names_to = "date") %>%
        filter(!is.na(value)) %>%
        pivot_wider(names_from = Metric, values_from = value) %>%
        mutate(doses_2dose = people_fully*2,
               doses_1dose = doses_admin - doses_2dose,
               people_initiated = doses_1dose + people_fully) %>%
        select(-c(doses_admin, people_fully, doses_2dose, doses_1dose)) %>%
        mutate(Metric = "people_initiated") %>%
        #mutate(people_initiated = ifelse(is.na(people_initiated), 0, people_initiated)) %>%
        pivot_wider(names_from = date, values_from = people_initiated)
    age_vacc_data_raw <- age_vacc_data_raw %>%
        filter(!(State %in% c("IL"))) %>%
        bind_rows(age_vacc_tmp)

    return(age_vacc_data_raw)
}






#' Process CCI age vaccination data
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
process_cci_age_vacc <- function(data=cci_vacc_data,
                                 daily_state_vacc,
                                 state_pop_ageXyr){

    cols_ <- c("State", "Category", "Metric", "Demo_cat_0", "Demo_cat_1", "Estimate_type")

    data <- data %>% filter(Demo_cat_0 %in% c("age", "age_gender_sex"), Category=="Vaccines") %>%
        mutate(Demo_cat_1 = str_replace(Demo_cat_1, "_older", "_100")) %>%
        pivot_longer(cols=-cols_, names_to = "date", values_to = "estimate") %>%
        rename(USPS=State) %>%
        mutate(estimate = ifelse(grepl("percent", Estimate_type), estimate/100, estimate),
               date = lubridate::as_date(lubridate::mdy(date))) %>%
        filter(!is.na(date), !is.na(estimate))

    # remove extra categories
    data <- data %>% filter(!(USPS=="UT" & Estimate_type!="total_cumulative" & Demo_cat_0!="age"))
    data <- data %>% filter(!(USPS=="DC" & Metric!="people_vaccinated"))
    data <- data %>% filter(!(USPS=="ND" & date<=lubridate::as_date("2021-06-04")))
    # filter(!(USPS=="IA" & date==lubridate::as_date("2021-04-02"))) %>%
    # filter(!(USPS=="HI" & date==lubridate::as_date("2021-04-02")))

    # combine "age_gender_sex"
    data_agegender <- data %>% filter(Demo_cat_0=="age_gender_sex") %>%
        separate(Demo_cat_1, c("age_l","age_r","gender"), sep="_", remove = FALSE) %>%
        filter(age_l!="unknown")
    # total_cum
    data_agegender_totcum <- data_agegender %>% filter(Estimate_type=="total_cumulative") %>%
        group_by(USPS, Metric, Estimate_type, date, age_l, age_r) %>%
        summarise(estimate = sum(estimate, na.rm=TRUE)) %>%
        mutate(Demo_cat_1=paste0(age_l, "_", age_r))

    # # total_cum
    # data_agegender_rateprctdemo <- data_agegender %>% filter(Estimate_type=="rate_percent_demo") %>%
    #     filter(gender!="unknown") %>%
    #     group_by(USPS, Metric, Estimate_type, date, age_l, age_r) %>%
    #     summarise(estimate = mean(estimate, na.rm=TRUE))

    data <- suppressWarnings(data %>% filter(Demo_cat_0!="age_gender_sex") %>%
        bind_rows(data_agegender_totcum) %>%
        separate(Demo_cat_1, into=c("age_l","age_r"), sep = "_", remove=FALSE) %>%
        group_by(USPS, Category, date, Estimate_type) %>%
        mutate(metric_sum = sum(c("people_partial",  "people_fully") %in% Metric)) %>%
        ungroup() %>%
        as_tibble())

    # Combine those that are broken out into partial and full
    data_partfull <- suppressMessages(data %>%
        filter(metric_sum == 2) %>%
        as_tibble() %>%
        group_by(USPS, Category, Estimate_type, date, Demo_cat_0, Demo_cat_1, age_l, age_r) %>%
        summarise(estimate = sum(estimate, na.rm=TRUE)) %>%
        mutate(Metric = "people_vaccinated"))

    data <- data %>%
        filter(!(USPS %in% unique(data_partfull$USPS))) %>%
        full_join(data_partfull) %>%
        select(-metric_sum) %>%
        filter(Metric %in% c("first_stage_doses", "people_initiated", "people_partial", "doses_admin", "people_vaccinated")) %>% # First dose
        pivot_wider(names_from = Estimate_type, values_from = estimate) %>%
        filter(age_l!="unknown" & age_r!="unknown") %>%
        group_by(USPS, Metric, Demo_cat_0, date) %>%
        mutate(prop_of_vacc = total_cumulative / sum(total_cumulative, na.rm=TRUE)) %>%
        ungroup() %>%
        mutate(prop_of_vacc = ifelse(is.na(prop_of_vacc) & !is.na(rate_percent), rate_percent, prop_of_vacc),
               prop_vacc_age = ifelse(!is.na(rate_percent_demo), rate_percent_demo,
                                      ifelse(!is.na(rate_per1kpop_demo), rate_per1kpop_demo / 1000, NA))) %>%
        rename(age = Demo_cat_1) %>% select(-Demo_cat_0, -Category)

    #fix weird age issue
    data <- data %>%
        mutate(age_l = as.integer(age_l), age_r=as.integer(age_r)) %>%
        mutate(age_l_new = ifelse(age_l>age_r, age_r, age_l),
               age_r = ifelse(age_l>age_r, age_l, age_r),
               age_l = age_l_new,
               age = paste0(age_l,"_", age_r)) %>%
        select(-age_l_new)

    # Fix weird HI overlap
    data_hi_ <- data %>% filter(USPS=="HI", date==as_date("2021-04-02"))
    data_hi_ <- data_hi_ %>%
        filter(age=="75_100") %>%
        bind_rows(data_hi_ %>% filter(age=="60_100") %>%
                      mutate(age="60_74", age_l=60, age_r=74,
                             total_cumulative = abs(diff(data_hi_$total_cumulative)))) %>%
        mutate(prop_of_vacc = round(total_cumulative / sum(total_cumulative),4))

    data <- data %>% filter(!(USPS=="HI" & date==as_date("2021-04-02"))) %>%
        bind_rows(data_hi_) %>%
        arrange(USPS, date, age_l)

    # Get rid of extra groups in WV
    data <- data %>%
        filter(!(USPS=="WV" & (age %in% c("18_100","18_100","16_100","65_100"))))  %>%
        distinct()

    data <- data %>%
        group_by(USPS, date) %>%
        mutate(remove = (!(Metric %in% c("first_stage_doses", "people_initiated")) & sum(Metric %in% c("first_stage_doses", "people_initiated"))>0)) %>%
        ungroup() %>%
        filter(remove!=TRUE)


    # Clean CCI data
    dates_ <- sort(unique(data$date))
    cci_vacc_clean <- tibble()
    for (i in 1:length(dates_)){
        cci_vacc_clean_ <- clean_vacc_age_cci(vacc_data = data %>% filter(date==dates_[i]),
                                              daily_state_vacc = daily_state_vacc,
                                              state_pop_ageXyr = state_pop_ageXyr)
        cci_vacc_clean <- bind_rows(cci_vacc_clean, cci_vacc_clean_)
    }
    cci_vacc_clean <- cci_vacc_clean %>%
        mutate(prop_vacc_age = ifelse(prop_vacc_age>0.99, .99, prop_vacc_age),
               dose1_age = round(prop_vacc_age*pop)) %>%
        group_by(USPS, date) %>%
        mutate(prop_of_vacc = round(dose1_age / sum(dose1_age, na.rm=TRUE),3))

    return(cci_vacc_clean)
}
















#' Clean Vacc data
#'
#' @param vacc_data
#' @param daily_state_vacc
#' @param state_pop_ageXyr
#' @param state_pop
#'
#' @return
#' @export
#'
#' @examples
clean_vacc_age_cci <- function(vacc_data,
                               daily_state_vacc,
                               state_pop_ageXyr){

    #daily vacc
    daily_vacc_ <- daily_state_vacc %>%
        select(USPS, date, dose1)

    # Clean vaccination data (add USPS, add state population, fix dates)
    vacc_clean <- vacc_data %>%
        filter(!is.na(age), age!="unknown") %>%
        mutate(comb=TRUE) %>% # Use this in case we are missing USPS
        left_join(tibble(state=c("US", state.name), USPS=c("US", state.abb), comb=TRUE)) %>%
        mutate(age_l = as.integer(age_l), age_r=as.integer(age_r)) %>%
        mutate(date = lubridate::as_date(date)) %>%
        filter(!is.na(date)) %>%
        dplyr::select(state, USPS, date, age, age_l, age_r, dose1_cum=total_cumulative, prop_of_vacc, prop_vacc_age_orig=prop_vacc_age)


    # Add missing age groups (some states do not report all age groups)
    # - any missing ages below those reported are put into a single 0-X age group
    vacc_clean <- vacc_clean %>%
        group_by(USPS, date) %>%
        mutate(lowest_group = (age_l==min(age_l))) %>%
        ungroup() %>% as_tibble()
    # take the lowest age group from the state, and only keep those where age_l>0;
    # - this means if the lowest age group is 15-29, we will keep it. but if it's 0-15, we will not as it's not missing.
    missing_low_groups <- vacc_clean %>%
        filter(lowest_group, age_l>0) %>%
        mutate(age_r = age_l-1,
               age_l = 0,
               prop_of_vacc = NA,
               prop_vacc_age_orig = NA,
               dose1_cum = NA,
               age = paste(age_l, age_r, sep="_"))
    if(nrow(missing_low_groups)){
        vacc_clean <- vacc_clean %>%
            full_join(missing_low_groups) %>%
            arrange(USPS, date, age_l)
    }

    state_pop <- state_pop_ageXyr %>%
        select(USPS, pop2019est) %>%
        distinct()

    # add state pop data
    vacc_clean <- vacc_clean %>%
        left_join(state_pop %>% rename(state_pop = pop2019est)) %>%
        filter(!is.na(state_pop))


    # Get total proportion of vaccines represented
    vacc_clean <- vacc_clean %>%
        group_by(USPS, date) %>%
        mutate(missing1_tmp = sum(is.na(prop_of_vacc))==1,
               prop_of_vacc_tot = sum(prop_of_vacc, na.rm=TRUE)) %>%
        ungroup() %>%
        dplyr::select(-lowest_group)

    # fill in missing proportion of vaccination
    vacc_clean_1NA <- vacc_clean %>% filter(missing1_tmp)
    vacc_clean_1NA$prop_of_vacc[is.na(vacc_clean_1NA$prop_of_vacc)] <- 1 - vacc_clean_1NA$prop_of_vacc_tot[is.na(vacc_clean_1NA$prop_of_vacc)]
    # Combine back with full data
    vacc_clean <- vacc_clean %>% filter(!missing1_tmp)
    vacc_clean <- bind_rows(vacc_clean, vacc_clean_1NA) %>%
        dplyr::select(-missing1_tmp)

    vacc_clean <- vacc_clean %>%
        as_tibble() %>%
        mutate(prop_of_vacc = ifelse(prop_of_vacc<0, 0, prop_of_vacc),
               prop_of_vacc = round(prop_of_vacc, 4)) %>%
        dplyr::select(-prop_of_vacc_tot)

    vacc_clean <- vacc_clean %>%
        arrange(USPS, date, age_l)


    # consolidate any vacc in people older than 100
    vacc_clean <- vacc_clean %>%
        group_by(USPS, date) %>%
        mutate(dose_100 = sum(dose1_cum[age_l>=100], na.rm=TRUE)) %>%
        filter(age_l<100) %>%
        mutate(dose_100 = ifelse(age_l==max(age_l), dose_100, 0)) %>%
        mutate(dose1_cum = dose1_cum + dose_100,
               age_r = ifelse(age_l==max(age_l), 100, age_r)) %>%
        ungroup() %>%
        select(-dose_100) %>%
        mutate(age = paste0(age_l, "_", age_r))

    # Get Population size for each age group reported
    vacc_clean <- get_pop_vacc_groups(vacc_age_data=vacc_clean, state_pop_ageXyr=state_pop_ageXyr)


    # --> may need to add more date grouping in future (seems to work ok now, but if differnt age groups by date occur could mess up)

    # Fill in prop_of_vacc & prop_vacc_age when missing and
    vacc_clean <- vacc_clean %>%
        group_by(USPS, date) %>%
        mutate(dose1_cum = ifelse(sum(!is.na(dose1_cum))>0 & is.na(dose1_cum), 0, dose1_cum),
               prop_of_vacc = ifelse(sum(!is.na(prop_of_vacc))>0 & is.na(prop_of_vacc), 0, prop_of_vacc),
               prop_vacc_age_orig = ifelse(sum(!is.na(prop_vacc_age_orig))>0 & is.na(prop_vacc_age_orig), 0, prop_vacc_age_orig)) %>%
        ungroup()


    # Get number and proportion vaccinated by age group

    # IF doses are available by age but proportion vaccinated or by age group are not
    # - we will use this to get the prop_vacc_age. There may be some time differences so will not use actual doses
    vacc_clean <- vacc_clean %>%
        rename(dose1_age = dose1_cum) %>%
        left_join(daily_vacc_ %>% mutate(dose1=round(dose1))) %>%
        group_by(USPS, date) %>%
        mutate(dose1_age = ifelse(is.na(dose1_age) & sum(dose1_age, na.rm=TRUE)>0, dose1 - sum(dose1_age, na.rm=TRUE), dose1_age),
               dose1_age = ifelse(dose1_age<0, 0, dose1_age)) %>%
        mutate(prop_vacc_age = round(dose1_age / pop, 4)) %>%
        ungroup()

    vacc_clean <- vacc_clean %>%
        mutate(prop_vacc_age = ifelse(is.na(prop_vacc_age) & !is.na(prop_vacc_age_orig), prop_vacc_age_orig, prop_vacc_age))

    # states with reported doses and proportion vaccinated - re-est population
    vacc_clean <- vacc_clean %>%
        mutate(pop_rev = ifelse(!is.na(prop_vacc_age_orig) & !is.na(dose1_age), round(dose1_age/prop_vacc_age_orig), pop),
               pop = ifelse(!is.na(pop_rev), pop_rev, pop)) %>%
        select(-pop_rev)

    vacc_clean <- vacc_clean %>%
        rowwise() %>%
        mutate(dose1_age = ifelse(!is.na(prop_of_vacc), round(dose1 * prop_of_vacc),
                                  ifelse(!is.na(prop_vacc_age), round(pop*prop_vacc_age), dose1_age))) %>% # number vaccinate by age group - dose 1
        ungroup() %>%
        mutate(prop_vacc_age = ifelse(is.na(prop_vacc_age), round(dose1_age / pop, 4), prop_vacc_age)) %>%
        mutate(prop_vacc_age = ifelse(is.na(prop_vacc_age) & !is.na(prop_vacc_age), prop_vacc_age, prop_vacc_age))

    # Fill in proportion of Vacc and doses if still missing
    vacc_clean <- vacc_clean %>%
        select(state, USPS, state_pop, pop, date, age, age_l, age_r, dose1, dose1_age, prop_of_vacc, prop_vacc_age) %>%
        group_by(USPS, date) %>%
        mutate(dose1_age = ifelse(is.na(dose1_age), round(prop_vacc_age*pop), dose1_age),
               prop_of_vacc = ifelse(is.na(prop_of_vacc), round(dose1_age/sum(dose1_age),4), prop_of_vacc)) %>%
        ungroup()

    return(vacc_clean)
}






#' Title
#'
#' @param cci_vacc_clean
#' @param state_pop_ageXyr
#' @param daily_state_vacc
#'
#' @return
#' @export
#'
#' @examples
#'
get_cci_vacc_Xyr <- function(cci_vacc_clean,
                             state_pop_ageXyr,
                             daily_state_vacc){
    dates_ <- sort(unique(cci_vacc_clean$date))
    cci_vacc_Xyr <- tibble()
    for (i in 1:length(dates_)){
        cci_vacc_Xyr_ <- suppressMessages(get_vacc_age_Xyr(vacc_clean = cci_vacc_clean %>% filter(date==dates_[i]),
                                          state_pop_ageXyr = state_pop_ageXyr,
                                          daily_state_vacc = daily_state_vacc) %>%
            mutate(date = dates_[i]))
        cci_vacc_Xyr <- bind_rows(cci_vacc_Xyr, cci_vacc_Xyr_)
    }
    return(cci_vacc_Xyr)
}





#' Title
#'
#' @param git_token
#'
#' @return
#' @export
#'
#' @examples
get_clean_us_agevacc <- function(git_token = "AB63TGCIYANL5EQ647HRXH3AYAWZ6"){

    # Load state populations & IDD data
    data("state_pop_age5yr")
    data("idd_vacc_clean")

    # Load latest vaccination by state
    daily_state_vacc <- suppressMessages(get_state_vacc() %>% filter(!is.na(dose1)))


    # pull and process CCI data

    # Pull and do a quick clean on cci Data
    cci_vacc_data <- pull_cci_vacc(git_token = git_token)
    cci_vacc_data <- clean_cci_vacc(cci_vacc_data)

    # Process the data
    cci_vacc_data <- process_cci_age_vacc(data=cci_vacc_data,
                                          daily_state_vacc = daily_state_vacc,
                                          state_pop_ageXyr = state_pop_age5yr) %>%
        left_join(state_pop_age5yr %>% select(USPS, geoid) %>% distinct())

    # combine
    vacc_comb <- idd_vacc_clean %>% mutate(source = "IDD") %>%
        bind_rows(cci_vacc_data %>% mutate(source = "CCI")) %>%
        select(USPS, state, geoid, date, source, pop,
               age, age_l, age_r,
               dose1, dose1_age, prop_vacc_age, prop_of_vacc)%>%
        mutate(age_mid = (age_r + age_l)/2 + .5)%>% as_tibble() %>%
        arrange(USPS, date, age_l)

    return(vacc_comb)
}




#' Title
#'
#' @param vacc_data
#' @param age_groups
#' @param git_token
#'
#' @return
#' @export
#'
#' @examples
get_standardized_us_agevacc <- function(vacc_data = NULL,
                                        age_groups = "5yr",
                                        git_token = "AB63TGCIYANL5EQ647HRXH3AYAWZ6"){

    # Load latest vaccination by state
    daily_state_vacc <- suppressMessages(get_state_vacc() %>% filter(!is.na(dose1)))

    # Load Xyr state populations
    if (age_groups=="5yr"){
        tmp <- data("state_pop_age5yr")
        assign("state_pop_Xyr", get(tmp))
        rm(tmp)
    } else if (age_groups=="10yr"){
        tmp <- data("state_pop_age10yr")
        assign("state_pop_Xyr", get(tmp))
        rm(tmp)
    }

    # Pull and clean the data
    if (is.null(vacc_data)){
        vacc_data <- get_clean_us_agevacc(git_token = git_token)
    }

    # Convert to 5yr age groups
    vacc_Xyr <- get_vacc_age_Xyr(vacc_clean=vacc_data,
                                 state_pop_ageXyr = state_pop_Xyr,
                                 daily_state_vacc = daily_state_vacc) %>%
        filter(!is.nan(prop_of_vacc)) %>%
        mutate(age_mid = (age_r + age_l)/2 + .5)%>% as_tibble() %>%
        arrange(USPS, date, age_l)

    # add back Source variable
    vacc_Xyr <- vacc_Xyr %>%
        left_join(vacc_data %>% select(USPS, date, source) %>% distinct())

    return(vacc_Xyr)
}


