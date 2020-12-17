# ---- load-libraries ----------------------------------------------------------
library(shiny)
library(plotly)

# ---- globals -----------------------------------------------------------------

axis_choices <- c(
  "Cases (7-day average)"
  ,"Cases (7DA/100K)"
  ,"Cases (cumulative)"
  ,"Cases (cum/100K)"
  ,"Deaths (7-day average)"
  ,"Deaths (7DA/100K)"
  ,"Deaths (cumulative)"
  ,"Deaths (cum/100K)"
  ,"Tests (7-day average)"
  ,"Tests (7DA/100K)"
  ,"Tests (cumulative)"
  ,"Tests (cum/100K)"
)


# ---- shiny-UI ----------------------------------------------------------------
shinyUI(
  navbarPage(
    "COVID-19 Explorer"
    # ---- bubble-graph --------------------------------------------------------
    ,tabPanel(
      "Bubbles" # NEED BETTER TITLE
      ,fluidRow(
        column(
          4
          ,dateRangeInput(
            inputId = "date1"
            ,label  = "Chose Date Range"
            ,start  = "2020-03-15"
            ,end    = Sys.Date()
            ,min    = "2020-03-15"
          )
          ,numericInput(
            inputId = "dateinput1"
            ,label  = "Days Between Facets"
            ,value  = 7
            ,min    = 1
          )
          ,radioButtons(
            inputId   = "freescalesradio"
            ,label    = "Free Axis Scale"
            ,choices  = c(
              "X"        = "free_x"
              ,"Y"       = "free_y"
              ,"Both"    = "free"
              ,"Neither" = "fixed"
            )
            ,inline   = TRUE
            ,selected = "free_y"
          )
        )
        ,column(
          4
          ,selectInput(
            inputId  = "grouping"
            ,label   = "Grouping"
            ,choices = c(
              "region"
              ,"winner_2016"
              ,"winner_2020_pres"
              ,"governor_political_affiliation"
              ,"state_senate_majority_political_affiliation"
              ,"state_house_majority_political_affiliation"
              ,"state_attorney_general_political_affiliation"
              ,"state_leadership"
            )
          )
          ,numericInput(
            "height"
            ,"Plot Height (pixels)"
            ,min   = 500
            ,max   = 3000
            ,value = 1000
          )
          ,downloadButton(
            "downloadplot"
            ,label = "Download Plot"
          )
        )
        ,column(
          4
          ,selectInput(
            inputId  = "xaxis"
            ,label   = "Choose X-Axis"
            ,choices = axis_choices
            ,selected = "Cases (7DA/100K)"
          )
          ,selectInput(
            inputId   = "yaxis"
            ,label    = "Choose Y-Axis"
            ,choices  = axis_choices
            ,selected = "Cases (cum/100K)"
          )
        )
      )
      ,plotOutput("plot1", height = "1000px" )
    )
    # ---- line-graph ----------------------------------------------------------
    ,tabPanel(
      "Line"
      ,fluidRow(
        column(
          4
          ,selectInput(
            inputId   = "metric"
            ,label    = "Choose Metric"
            ,choices  = axis_choices
            ,selected = "Cases (7DA/100K)"
          )
          ,checkboxInput(
            inputId = "line_smoother"
            ,label  = "Add Smoother?"
          )
        )
        ,column(
          4
          ,checkboxInput(
            inputId = "facet_graph"
            ,label  = "Facet Graph"
          )
          ,conditionalPanel(
            condition = "input.facet_graph == true"
            ,checkboxInput(
              inputId  = "free_y_line"
              ,label   = "Lock Y axis"
              ,value   = TRUE
            )
            ,selectInput(
              inputId  = "facet_graph_choice"
              ,label   = "Choose Facet"
              ,choices = c(
                "Region"                        = "region"
                ,"Division"                     = "division"
                ,"Governor Affiliation"         = "governor_political_affiliation"
                ,"State Leadership Affiliation" = "state_leadership"
                ,"Winner 2016"                  = "winner_2016"
                ,"Winner 2020"                  = "winner_2020_pres"
              )
            )
          )
        )
        ,column(
          4
          ,selectInput(
            inputId = "line_color"
            ,label = "Chose Line Color"
            ,choices = c(
              "Region"                        = "region"
              ,"Division"                     = "division"
              ,"Governor Affiliation"         = "governor_political_affiliation"
              ,"State Leadership Affiliation" = "state_leadership"
              ,"Winner 2016"                  = "winner_2016"
              ,"Winner 2020"                  = "winner_2020_pres"
            )
          )
        )
      )
      ,plotlyOutput("linegraph")
    )
    # ---- about ---------------------------------------------------------------
    ,tabPanel(
      "About"
      ,htmlTemplate("www/html/about.html")
    )
  )
)
