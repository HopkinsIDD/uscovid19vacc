




# Clean Vacc data

clean_vacc_age_idd <- function(vacc_data,
                         daily_state_vacc,
                         state_pop_age5yr,
                         state_pop){

    # # Subset vaccination rates to date desired
    # rep_state_vacc <- daily_state_vacc %>%
    #     filter(date == date_vacc_cum) %>%
    #     select(USPS, dose1)

    #daily vacc - for use with getting props
    daily_vacc_ <- daily_state_vacc %>%
        #filter(date == date_vacc_cum) %>%
        select(USPS, date, dose1)


    # Clean vaccination data (add USPS, add state population, fix dates)
    vacc_clean <- vacc_data %>%
        filter(!is.na(age)) %>%
        mutate(comb=TRUE) %>% # Use this in case we are missing USPS
        left_join(tibble(state=c("US", state.name), USPS=c("US", state.abb), comb=TRUE)) %>%
        separate(age, into=c("age_l", "age_r"), sep="_", remove=FALSE) %>%
        mutate(age_l = as.integer(age_l), age_r=as.integer(age_r)) %>%
        mutate(prop_of_vacc = as.numeric(gsub("%", "", percent_of_vac))/100,
               prop_vacc_age_orig = as.numeric(gsub("%", "", percent_coverage))/100,
               date_released = lubridate::as_date(date_released),
               date_added = lubridate::as_date(date_added)) %>%
        mutate(date = lubridate::as_date(ifelse(!is.na(date_released), date_released, date_added))) %>%
        left_join(state_pop) %>%
        filter(!is.na(pop2019est)) %>%
        filter(!is.na(date)) %>%
        dplyr::select(state, USPS, date, age, age_l, age_r, pop2019est, population, dose, prop_of_vacc, prop_vacc_age_orig)


    # Add missing age groups (some states do not report all age groups)
    # - any missing ages below those reported are put into a single 0-X age group
    vacc_clean <- vacc_clean %>%
        group_by(USPS) %>%
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
               dose = NA,
               population = NA,
               age = paste(age_l, age_r, sep="_"))
    if(nrow(missing_low_groups)){
        vacc_clean <- vacc_clean %>%
            full_join(missing_low_groups)
    }

    # Get total proportion of vaccines represented
    vacc_clean <- vacc_clean %>%
        group_by(USPS) %>%
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
        dplyr::select(-prop_of_vacc_tot) %>%
        arrange(USPS, age_l)


    # consolidate any vacc in people older than 100
    vacc_clean <- vacc_clean %>%
        group_by(USPS) %>%
        mutate(dose_100 = sum(dose[age_l>=100], na.rm=TRUE)) %>%
        filter(age_l<100) %>%
        mutate(dose_100 = ifelse(age_l==max(age_l), dose_100, 0)) %>%
        mutate(dose = dose + dose_100,
               age_r = ifelse(age_l==max(age_l), 100, age_r)) %>%
        ungroup() %>%
        select(-dose_100) %>%
        mutate(age = paste0(age_l, "_", age_r))

    # Get Population size for each age group reported
    vacc_clean <- get_pop_vacc_groups(vacc_age_data=vacc_clean, state_pop_ageXyr=state_pop_age5yr)
    # --> may need to add date in future to grouping here


    # Fill in prop_of_vacc & prop_vacc_age when missing and
    vacc_clean <- vacc_clean %>%
        group_by(USPS) %>%
        mutate(prop_of_vacc = ifelse(sum(!is.na(prop_of_vacc))>0 & is.na(prop_of_vacc), 0, prop_of_vacc),
               prop_vacc_age_orig = ifelse(sum(!is.na(prop_vacc_age_orig))>0 & is.na(prop_vacc_age_orig), 0, prop_vacc_age_orig)) %>%
        ungroup()

    # Get number and proportion vaccinated by age group

    # IF doses are available by age but proportion vaccinated or by age group are not
    # - we will use this to get the prop_vacc_age. There may be some time differences so will not use actual doses
    vacc_clean <- vacc_clean %>%
        rename(dose1_age = dose) %>%
        left_join(daily_vacc_) %>%
        group_by(USPS) %>%
        mutate(dose1_age = ifelse(is.na(dose1_age) & sum(dose1_age, na.rm=TRUE)>0, dose1 - sum(dose1_age, na.rm=TRUE), dose1_age),
               dose1_age = ifelse(dose1_age<0, 0, dose1_age)) %>%
        mutate(prop_vacc_age = round(dose1_age / pop, 4)) %>%
        ungroup()

    vacc_clean <- vacc_clean %>%
        mutate(prop_vacc_age = ifelse(is.na(prop_vacc_age) & !is.na(prop_vacc_age_orig), prop_vacc_age_orig, prop_vacc_age))

    vacc_clean <- vacc_clean %>%
        # rename(dose1_date_of_rep = dose1) %>%
        # left_join(rep_state_vacc) %>%
        rowwise() %>%
        mutate(dose1_age = ifelse(!is.na(prop_of_vacc), round(dose1 * prop_of_vacc),
                                  ifelse(!is.na(prop_vacc_age), round(pop*prop_vacc_age), dose1_age))) %>% # number vaccinate by age group - dose 1
        ungroup() %>%
        mutate(prop_vacc_age = ifelse(is.na(prop_vacc_age), round(dose1_age / pop, 4), prop_vacc_age)) %>%
        mutate(prop_vacc_age = ifelse(is.na(prop_vacc_age) & !is.na(prop_vacc_age), prop_vacc_age, prop_vacc_age))

    # Fill in proportion of Vacc and doses if still missing
    vacc_clean <- vacc_clean %>%
        select(state, USPS, pop2019est, pop, date, age, age_l, age_r, dose1, dose1_age, prop_of_vacc, prop_vacc_age) %>%
        group_by(USPS) %>%
        mutate(dose1_age = ifelse(is.na(dose1_age), round(prop_vacc_age*pop), dose1_age),
               prop_of_vacc = ifelse(is.na(prop_of_vacc), round(dose1_age/sum(dose1_age),4), prop_of_vacc))

    return(vacc_clean)
}

