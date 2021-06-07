## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(message = FALSE)

## ---- message = FALSE---------------------------------------------------------
library(tidyverse)
library(uscovid19vacc)
#source("R/vacc_funcs.R")

## ---- message = FALSE---------------------------------------------------------
git_token <- "AB63TGGKM6VBDNGJHXRRP7LAYPDIM"

## -----------------------------------------------------------------------------

daily_state_vacc <- get_state_vacc() %>%
    filter(!is.na(dose1))


## -----------------------------------------------------------------------------

# Get clean, but not age group standardized data
vacc_data <- get_clean_us_agevacc(git_token = git_token)

# Make it standardized to either 5yr or 10 year age groups
vacc_data_5yr <- get_standardized_us_agevacc(vacc_data = vacc_data,
                                        age_groups = "5yr", # "10yr"
                                        git_token = git_token)

# Make it standardized to either 5yr or 10 year age groups
vacc_data_10yr <- get_standardized_us_agevacc(vacc_data = vacc_data,
                                        age_groups = "10yr", 
                                        git_token = git_token) 



## ---- fig.width=8, fig.height=8-----------------------------------------------
usps_samp <- sample(unique(vacc_data_5yr$USPS), 12, replace = FALSE)
vacc_data_5yr %>% filter(USPS %in% usps_samp) %>%
              arrange(prop_vacc_age) %>%
              mutate(USPS = fct_reorder(USPS, (prop_vacc_age))) %>%
              ggplot(aes(x=date, y=prop_vacc_age, color=age_group, group=age_group)) +
              geom_line() +
              geom_point(aes(shape=source, fill=source)) +
              scale_shape_manual("Source", values = c(21, 23)) +
              scale_fill_manual("Source", values=c("orange","lightblue4")) +
              scale_color_discrete("Age group, yrs") +
              xlab("State") + ylab("proportion vaccinated") +
              ggtitle("Proportion Vaccinated, By State and Age") +
              theme_bw() +
              theme(axis.text.x = element_text(angle=90)) +
              facet_wrap(~USPS)

## ----fig.width=8, fig.height=8------------------------------------------------
vacc_data_10yr %>% filter(USPS %in% usps_samp) %>%
              arrange(prop_vacc_age) %>%
              mutate(USPS = fct_reorder(USPS, (prop_vacc_age))) %>%
              ggplot(aes(x=date, y=prop_vacc_age, color=age_group, group=age_group)) +
              geom_line() +
              geom_point(aes(shape=source, fill=source)) +
              scale_shape_manual("Source", values = c(21, 23)) +
              scale_fill_manual("Source", values=c("orange","lightblue4")) +
              scale_color_discrete("Age group, yrs") +
              xlab("State") + ylab("proportion vaccinated") +
              ggtitle("Proportion Vaccinated, By State and Age") +
              theme_bw() +
              theme(axis.text.x = element_text(angle=90)) +
              facet_wrap(~USPS)

## -----------------------------------------------------------------------------
dir.create("figures", recursive = TRUE, showWarnings = FALSE)
states_ <- sort(unique(vacc_data_5yr$USPS))
pages_ <- ceiling(length(states_)/9)
state_groups <- sort(rep(1:pages_, 9))[1:length(states_)]

pdf(paste0("figures/prop_vacc_by_age_", Sys.Date(), ".pdf"), height=11, width = 8.5)
for (i in 1:pages_){
    state_tmp <- states_[state_groups==i]
    print(vacc_data_5yr %>% filter(USPS %in% state_tmp) %>%
              arrange(prop_vacc_age) %>%
              mutate(USPS = fct_reorder(USPS, (prop_vacc_age))) %>%
              ggplot(aes(x=date, y=prop_vacc_age, color=age_group, group=age_group)) +
              geom_line() +
              geom_point(aes(shape=source, fill=source)) +
              scale_shape_manual("Source", values = c(21, 23)) +
              scale_fill_manual("Source", values=c("orange","lightblue4")) +
              scale_color_discrete("Age group, yrs") +
              xlab("State") + ylab("proportion vaccinated") +
              ggtitle("Proportion Vaccinated, By State and Age") +
              theme_bw() +
              theme(axis.text.x = element_text(angle=90)) +
              facet_wrap(~USPS))
}
dev.off()


## -----------------------------------------------------------------------------
states_ <- sort(unique(vacc_data_10yr$USPS))
pages_ <- ceiling(length(states_)/9)
state_groups <- sort(rep(1:pages_, 9))[1:length(states_)]

pdf(paste0("figures/prop_vacc_by_age10yr_", Sys.Date(), ".pdf"), height=11, width = 8.5)
for (i in 1:pages_){
    state_tmp <- states_[state_groups==i]
    print(vacc_data_10yr %>% filter(USPS %in% state_tmp) %>%
              arrange(prop_vacc_age) %>%
              mutate(USPS = fct_reorder(USPS, (prop_vacc_age))) %>%
              ggplot(aes(x=date, y=prop_vacc_age, color=age_group, group=age_group)) +
              geom_line() +
              geom_point(aes(shape=source, fill=source)) +
              scale_shape_manual("Source", values = c(21, 23)) +
              scale_fill_manual("Source", values=c("orange","lightblue4")) +
              scale_color_discrete("Age group, yrs") +
              xlab("State") + ylab("proportion vaccinated") +
              ggtitle("Proportion Vaccinated, By State and Age") +
              theme_bw() +
              theme(axis.text.x = element_text(angle=90)) +
              facet_wrap(~USPS))
}
dev.off()


## -----------------------------------------------------------------------------

vacc_data_5yr_cl <- drop_dates_vaccdat(vacc_data_Xyr = vacc_data_5yr)
vacc_data_10yr_cl <- drop_dates_vaccdat(vacc_data_Xyr = vacc_data_10yr)


## ----fig.width=8, fig.height=8------------------------------------------------
vacc_data_10yr_cl %>% filter(USPS %in% usps_samp) %>%
              arrange(prop_vacc_age) %>%
              mutate(USPS = fct_reorder(USPS, (prop_vacc_age))) %>%
              ggplot(aes(x=date, y=prop_vacc_age, color=age_group, group=age_group)) +
              geom_line() +
              geom_point(aes(shape=source, fill=source)) +
              scale_shape_manual("Source", values = c(21, 23)) +
              scale_fill_manual("Source", values=c("orange","lightblue4")) +
              scale_color_discrete("Age group, yrs") +
              xlab("State") + ylab("proportion vaccinated") +
              ggtitle("Proportion Vaccinated, By State and Age") +
              theme_bw() +
              theme(axis.text.x = element_text(angle=90)) +
              facet_wrap(~USPS)

## -----------------------------------------------------------------------------
states_ <- sort(unique(vacc_data_5yr_cl$USPS))
pages_ <- ceiling(length(states_)/9)
state_groups <- sort(rep(1:pages_, 9))[1:length(states_)]

pdf(paste0("figures/prop_vacc_by_age5yr_", Sys.Date(), "_clean.pdf"), height=11, width = 8.5)
for (i in 1:pages_){
    state_tmp <- states_[state_groups==i]
    print(vacc_data_5yr_cl %>% filter(USPS %in% state_tmp) %>%
              arrange(prop_vacc_age) %>%
              mutate(USPS = fct_reorder(USPS, (prop_vacc_age))) %>%
              ggplot(aes(x=date, y=prop_vacc_age, color=age_group, group=age_group)) +
              geom_line() +
              geom_point(aes(shape=source, fill=source)) +
              scale_shape_manual("Source", values = c(21, 23)) +
              scale_fill_manual("Source", values=c("orange","lightblue4")) +
              scale_color_discrete("Age group, yrs") +
              xlab("State") + ylab("proportion vaccinated") +
              ggtitle("Proportion Vaccinated, By State and Age") +
              theme_bw() +
              theme(axis.text.x = element_text(angle=90)) +
              facet_wrap(~USPS))
}
dev.off()


## -----------------------------------------------------------------------------
dir.create("figures", recursive = TRUE, showWarnings = FALSE)
states_ <- sort(unique(vacc_data_10yr_cl$USPS))
pages_ <- ceiling(length(states_)/9)
state_groups <- sort(rep(1:pages_, 9))[1:length(states_)]

pdf(paste0("figures/prop_vacc_by_age10yr_", Sys.Date(), "_clean.pdf"), height=11, width = 8.5)
for (i in 1:pages_){
    state_tmp <- states_[state_groups==i]
    print(vacc_data_10yr_cl %>% filter(USPS %in% state_tmp) %>%
              arrange(prop_vacc_age) %>%
              mutate(USPS = fct_reorder(USPS, (prop_vacc_age))) %>%
              ggplot(aes(x=date, y=prop_vacc_age, color=age_group, group=age_group)) +
              geom_line() +
              geom_point(aes(shape=source, fill=source)) +
              scale_shape_manual("Source", values = c(21, 23)) +
              scale_fill_manual("Source", values=c("orange","lightblue4")) +
              scale_color_discrete("Age group, yrs") +
              xlab("State") + ylab("proportion vaccinated") +
              ggtitle("Proportion Vaccinated, By State and Age") +
              theme_bw() +
              theme(axis.text.x = element_text(angle=90)) +
              facet_wrap(~USPS))
}
dev.off()


## -----------------------------------------------------------------------------
dir.create("output")
readr::write_csv(vacc_data_5yr_cl, "output/vacc_data_5yr_cl.csv")
readr::write_csv(vacc_data_10yr_cl, "output/vacc_data_10yr_cl.csv")


