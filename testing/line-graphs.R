# This script needs to run before publishing App.  Opens the local versions
# of needed data and places them into a folder in the app

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio
# ---- load-packages -----------------------------------------------------------

library(tidyverse)
library(plotly)


# ---- declare-globals ---------------------------------------------------------

# get config file
config <- config::get()

compute_epi <- function(
  d
  ,grouping_vars
  ,var_cases = "n_cases"
  ,var_deaths = "n_deaths"
  ,var_tests = "n_tested"
  ,long =FALSE){
  # d <- ds_jh_state %>% filter(province_state == "Florida")
  # grouping_vars <- c("date","province_state")
  # grouping_vars <- c("date")
  # var_cases = "n_cases"
  # var_deaths = "n_deaths"
  # var_tests = "people_tested"
  # long = F
  # browser()
  grouping_vars_enquo <- rlang::syms(grouping_vars)
  grouping_vars_no_date <- rlang::syms(setdiff(grouping_vars,"date"))
  var_cases_enquo  <- rlang::sym(var_cases)
  var_deaths_enquo <- rlang::sym(var_deaths)
  var_tests_enquo  <- rlang::sym(var_tests)


  d_out <- d %>%
    dplyr::arrange(!!!grouping_vars_enquo) %>%
    dplyr::group_by(!!!grouping_vars_enquo) %>%
    dplyr::summarize(
      n_cases_cum     = sum(!!var_cases_enquo, na.rm = T)
      ,n_deaths_cum   = sum(!!var_deaths_enquo, na.rm = T)
      ,n_tests_cum    = sum(!!var_tests_enquo, na.rm = T)
      ,population     = sum(population, na.rm = T)
      ,incident_rate  = n_cases_cum/population*100000
      ,mortality_rate = n_deaths_cum/population*100000
      ,testing_rate   = n_tests_cum/population*100000
      ,.groups = "keep"
    ) %>%
    group_by(!!!grouping_vars_no_date) %>%
    arrange(date) %>%
    dplyr::mutate(
      n_cases   = n_cases_cum - lag(n_cases_cum,1)
      ,n_deaths  = n_deaths_cum - lag(n_deaths_cum,1)
      ,n_tests    = n_tests_cum - lag(n_tests_cum,1)
      ,n_cases_roll_7 = zoo::rollapply(n_cases, 7, mean, align = 'right', fill = NA)
      ,n_deaths_roll_7 = zoo::rollapply(n_deaths, 7, mean, align = 'right', fill = NA)
      ,n_tests_roll_7 = zoo::rollapply(n_tests, 7, mean, align = 'right', fill = NA)
      ,n_cases_roll_7_rate = n_cases_roll_7/population*100000
      ,n_deaths_roll_7_rate = n_deaths_roll_7/population*100000
      ,n_tests_roll_7_rate = n_tests_roll_7/population*100000
    ) %>%
    ungroup() %>%
    select(all_of(c(grouping_vars,names(metric_order))))
  # d_out %>% glimpse()
  if(long){
    var_pivot_longer <- setdiff(names(d_out), grouping_vars)
    d_out <- d_out %>%
      tidyr::pivot_longer(cols = var_pivot_longer, names_to = "metric", values_to = "value") %>%
      mutate(
        metric = factor(metric, levels = names(metric_order), labels = metric_order)
      )
  }
  d_out <- d_out %>% dplyr::na_if(0L)
  return(d_out)
}



metric_order <- c(
  "n_cases_roll_7"        = "Cases (7-day average)"
  ,"n_cases_roll_7_rate"  = "Cases (7DA/100K)"
  ,"n_cases_cum"          = "Cases (cumulative)"
  ,"incident_rate"        = "Cases (cum/100K)"

  ,"n_deaths_roll_7"      = "Deaths (7-day average)"
  ,"n_deaths_roll_7_rate" = "Deaths (7DA/100K)"
  ,"n_deaths_cum"         = "Deaths (cumulative)"
  ,"mortality_rate"       = "Deaths (cum/100K)"

  ,"n_tests_roll_7"       = "Tests (7-day average)"
  ,"n_tests_roll_7_rate"  = "Tests (7DA/100K)"
  ,"n_tests_cum"          = "Tests (cumulative)"
  ,"testing_rate"         = "Tests (cum/100K)"

)


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

# ---- graphing ----------------------------------------------------------------
focus_metrics <-  "Cases (7DA/100K)"




d <- ds_covid_vote %>%
  compute_epi(c("date","state","region", "country", "winner_2016"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  mutate(
    metric = fct_relevel(metric, focus_metrics) %>% fct_drop()
  )




g <- d %>%
  highlight_key(~state) %>%
  {ggplot(.,aes(x = date, y = value, group = state, color = winner_2016)) +
  geom_line() +
  facet_wrap(.~winner_2016) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b", limits = c(as.Date("2020-03-01"), max(d$date, na.rm = TRUE))) +
  labs(
    x  = NULL
    ,y = focus_metrics
  )}
g

gg <- g %>% ggplotly(tooltip = "state") %>%
  highlight(on = "plotly_click", off = "plotly_doubleclick", selected = attrs_selected(showlegend = FALSE))
gg


test_graph <- function(d,color_in){

  g <- d %>%
    highlight_key(~state) %>%
    ggplot(aes(x = date, y = value, group = state, color = .data[[color_in]])) +
    geom_line() +
    facet_wrap(.~.data[[color_in]])
}


test_graph(d, "winner_2016") %>% print()
test_graph()

test_
