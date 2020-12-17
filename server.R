rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# must run data-prep.R before uploading to shinyapps.io
# ---- load-packages -----------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
#---- load-sources ------------------------------------------------------------


# ---- declare-globals --------------------

ggplot2::theme_set(
    ggplot2::theme_bw(
    )+
        theme(
            strip.background = element_rect(fill="grey90", color = NA)
        )
)

division_levels <- c(
    "Pacific"
    ,"Mountain"
    ,"New England"
    ,"West North Central"
    ,"East North Central"
    ,"Middle Atlantic"
    ,"West South Central"
    ,"South Atlantic"
    ,"East South Central"
)

region_colors <- c(
    "West" = "#1B9E77"
    ,"South" = "#E7298A"
    ,"NorthEast" = "#7570B3"
    ,"MidWest" = "#D95F02"
)

party_colors <- c(
    "Democrat"    = "#0015BC"
    ,"Divided"    = "#800080"
    ,"Republican" = "#E9141D"

)

division_colors <- c(
    "Pacific"             = "#1B9E77"
    ,"Mountain"           = "#D95F02"
    ,"New England"        = "#7570B3"
    ,"West North Central" = "#E7298A"
    ,"East North Central" = "#66A61E"
    ,"Middle Atlantic"    = "#E6AB02"
    ,"West South Central" = "#A6761D"
    ,"South Atlantic"     = "#666666"
    ,"East South Central" = "#1F78B4"
)





# ---- declare-functions ---------------------------
metric_order <- c(
    "n_cases_roll_7"         = "Cases (7-day average)"
    ,"n_cases_roll_7_rate"   = "Cases (7DA/100K)"
    ,"n_cases_cum"           = "Cases (cumulative)"
    ,"incident_rate"         = "Cases (cum/100K)"
    ,"n_deaths_roll_7"       = "Deaths (7-day average)"
    ,"n_deaths_roll_7_rate"  = "Deaths (7DA/100K)"
    ,"n_deaths_cum"          = "Deaths (cumulative)"
    ,"mortality_rate"        = "Deaths (cum/100K)"
    ,"n_tests_roll_7"        = "Tests (7-day average)"
    ,"n_tests_roll_7_rate"   = "Tests (7DA/100K)"
    ,"n_tests_cum"           = "Tests (cumulative)"
    ,"testing_rate"          = "Tests (cum/100K)"
)

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

# ---- load-data ---------------------------------------------------------------

app_data <- read_rds("./data-public/derived/app-data.rds")

# ---- shiny-server ------------------------------------------------------------
shinyServer(function(input, output, session) {
# browser()


    create_graph <- function(){
        political_grouping <- input$grouping
        focus_metric <- c(input$xaxis, input$yaxis)
        lock_scales <- input$freescalesradio
        date_seq <- input$dateinput1

        if(input$grouping == "region"){
            color_fill <- region_colors
        } else {
            color_fill <- party_colors
        }

        d <- app_data %>%
            compute_epi(
                c(
                    "date"
                    ,"state"
                    ,"state_abb"
                    ,"division"
                    ,"country"
                    ,political_grouping
                    )
                ,long = T
            ) %>% filter(
                date %in% as_date(format(seq(input$date1[1],input$date1[2], date_seq)))
                ) %>%
            filter(metric %in% focus_metric) %>%
            mutate(
                metric  = janitor::make_clean_names(as.character(metric))
                ,metric = str_remove_all(metric, "_\\d+$")
            ) %>%
            tidyr::pivot_wider(names_from = "metric", values_from = "value")

           g4 <- d %>%
               ggplot(
                   aes_string(
                       x      = janitor::make_clean_names(focus_metric[1])
                       ,y     = janitor::make_clean_names(focus_metric[2])
                       ,label = "state_abb"
                       ,fill  =  political_grouping
                       ,color = political_grouping
                   ))+
               scale_fill_manual(values = color_fill) +
               scale_color_manual(values = color_fill) +
               geom_point(shape = 21, color = "grey30", alpha = .2, size = 7)+
               geom_text(alpha = .9, size = 3)+
               facet_wrap(.~date, scales = lock_scales, ncol = 6)+
               labs(x = focus_metric[1], y = focus_metric[2])
           return(g4)

    }


    output$plot1 <- renderPlot(
        height = function() input$height,
        {

        create_graph()
    })


    output$downloadplot <- downloadHandler(
        filename = function() {
            paste0("plot-", Sys.Date(), ".png")
        }
        ,content = function(file) {
            ggsave(file, plot = create_graph() , device = "png"
                   ,width = 15, height = 10, units = 'in', dpi = 300)
        }
    )


    # ---- line graph ----------------------------------------------------------

    line_graph_data <- reactive({
        if(input$facet_graph_choice == input$line_color){
            facet_group <- NULL
        } else if(input$facet_graph) {
            facet_group <- input$facet_graph_choice
        } else {
            facet_group <- NULL
        }

       d <-  app_data %>%
            compute_epi(
                c("date","state","country", input$line_color, facet_group)
                ,long = T
                ) %>%
           filter(metric %in% input$metric)
       return(d)

        # NOT USED CURRENTLY, just using one meteric
            # mutate(
            #     metric = fct_relevel(metric, focus_metrics) %>% fct_drop()
            # )
    })

    line_graph <- reactive({
        if(input$line_color == "region"){
            color_fill <- region_colors
        } else if(input$line_color == "division"){
            color_fill <- division_colors
        } else {
            color_fill <- party_colors
        }

        g <- line_graph_data() %>%
            highlight_key(~state) %>%
            {ggplot(.,aes(x = date, y = value, group = state, color = .data[[input$line_color]])) +
            geom_line() +
            scale_x_date(
                date_breaks = "2 month"
                ,date_labels = "%b"
                ,limits = c(as.Date("2020-03-01"), max(line_graph_data()$date, na.rm = TRUE))) +
            scale_color_manual(values = color_fill) +
            labs(
                x  = ""
                ,y = input$metric
            )}

        if(input$line_smoother){
            g <- g +
                geom_smooth(
                    aes(group = .data[[input$line_color]])
                    ,method = "loess"
                    ,se = FALSE
                    )
        }


        return(g)
    })


    output$linegraph <- renderPlotly({
        if(input$facet_graph && input$free_y_line){
            facet_scale_line <- "fixed"
        } else {
            facet_scale_line <- "free_y"
        }

        if(input$facet_graph){
             g <- line_graph() +
                facet_wrap(
                    .~.data[[input$facet_graph_choice]]
                    ,scales = facet_scale_line
                )
        } else {
             g <- line_graph()
        }

        gplotly <- g %>%
            ggplotly(tooltip = "state", height = 700) %>%
            highlight(
                on = "plotly_click"
                ,off = "plotly_doubleclick"
                ,selected = attrs_selected(showlegend = FALSE)
            )

        return(gplotly)


    })



})
