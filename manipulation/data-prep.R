# This script needs to run before publishing App.  Opens the local versions
# of needed data and places them into a folder in the app
# This file preps Data for Shiny App.
# final file should be loaded into app

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio
# ---- load-packages -----------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
# ---- load-sources ------------------------------------------------------------

# updates local data prior to creating app_data
# set working dir to Covid19-country-response for COVID data
setwd("../covid19-country-response/")
source("./manipulation/ellis-covid-jh.R")
source("./manipulation/scribe-john-hopkins.R")
# clean environment from sourcing files
rm(list=ls(all=TRUE))


# ---- declare-globals ---------------------------------------------------------
# return working direcotry to project root
setwd("~/GitHub/covid-explorer")

# get config file
config <- config::get()


# ---- load-data ---------------------------------------------------------------

ds_jh_state <- readr::read_rds(config$path_ds_jh_daily)

ds_vote <- readr::read_rds(config$path_ds_votes)

# Note: political leadership reflects the state of 2020

ds_vote_pres_2020 <-  read_rds(config$path_ds_vote_pres_2020)

# ---- merge-data --------------------------------------------------------------

ds_covid_vote <- ds_jh_state %>%
  left_join(
    ds_vote %>% select(-c("state_po","state_fips"))
    ,by = c("state"= "province_state")
  ) %>%
  left_join(ds_vote_pres_2020, by = "state") %>%
  mutate(state = factor(state))


# ---- save-data ---------------------------------------------------------------
# saves the combine data set into the app directory /data

ds_covid_vote %>% write_rds(
  "./data-unshared/derived/app-data.rds"
  ,compress = "gz"
  )
