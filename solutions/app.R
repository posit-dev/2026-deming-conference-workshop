library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(scales)

source("helpers.R")
candidates <- read.csv("candidates.csv")

treatment_choices <- c(
  "Any / no restriction" = "any",
  "GnRH antagonist" = "tx_gnrh_antagonist",
  "GnRH agonist" = "tx_gnrh_agonist",
  "Combined hormonal" = "tx_combined_hormonal",
  "Progestin" = "tx_progestin",
  "Aromatase inhibitor" = "tx_aromatase_inhibitor",
  "NSAID" = "tx_nsaid",
  "Opioid" = "tx_opioid"
)

# Candidate grouping variables: categorical columns only
group_vars <- c(
  "age_band",
  "sex",
  "region",
  "urbanicity",
  "plan_type",
  "index_year",
  "baseline_pelvic_pain",
  "baseline_infertility",
  "baseline_depression_anxiety"
)
group_choices <- setNames(
  group_vars,
  tools::toTitleCase(gsub("_", " ", group_vars))
)

ui <- page_sidebar(
  title = "Feasibility of an endometriosis NSAID study",
  fillable = TRUE,
  sidebar = sidebar(
    title = "Candidate criteria",
    radioButtons(
      "n_claims",
      "Number of index claims",
      choices = c(
        "1 or more" = 1,
        "2 or more" = 2,
        "3 or more" = 3,
        "4 or more" = 4,
        "5 or more" = 5
      ),
      selected = 2
    ),
    input_switch("rx_coverage", "Has pharmacy coverage", value = TRUE),
    selectInput(
      "treatment",
      "Treatment received",
      choices = treatment_choices,
      selected = "tx_nsaid"
    )
  ),
  # Row proportions match the report: 20% / 80%
  layout_columns(
    col_widths = 12,
    row_heights = c(1, 4),
    layout_columns(
      col_widths = c(4, 4, 4),
      uiOutput("vb_cohort"),
      uiOutput("vb_subgroups"),
      uiOutput("vb_smallest")
    ),
    card(
      full_screen = TRUE,
      card_body(plotOutput("heatmap")),
      card_footer(
        layout_columns(
          col_widths = c(6, 6),
          selectInput(
            "x_var",
            "X variable",
            group_choices,
            selected = "age_band"
          ),
          selectInput("y_var", "Y variable", group_choices, selected = "region")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  cohort <- reactive({
    d <- candidates |>
      filter(n_index_claims >= as.numeric(input$n_claims))
    if (input$rx_coverage) {
      d <- filter(d, rx_coverage == "Y")
    }
    if (input$treatment != "any") {
      d <- filter(d, .data[[input$treatment]])
    }
    d
  })

  # Keep x and y distinct
  observeEvent(input$x_var, {
    if (input$y_var == input$x_var) {
      updateSelectInput(
        session,
        "y_var",
        selected = setdiff(group_vars, input$x_var)[1]
      )
    }
  })
  observeEvent(input$y_var, {
    if (input$x_var == input$y_var) {
      updateSelectInput(
        session,
        "x_var",
        selected = setdiff(group_vars, input$y_var)[1]
      )
    }
  })

  ready <- reactive({
    req(input$x_var, input$y_var, input$x_var != input$y_var)
    validate(need(nrow(cohort()) > 0, "No patients meet these criteria."))
  })

  output$vb_cohort <- renderUI({
    cohort_value_box(cohort(), title = "Candidates meeting criteria")
  })
  output$vb_subgroups <- renderUI({
    ready()
    subgroups_value_box(cohort(), input$x_var, input$y_var)
  })
  output$vb_smallest <- renderUI({
    ready()
    smallest_subgroup_value_box(cohort(), input$x_var, input$y_var)
  })
  output$heatmap <- renderPlot({
    ready()
    subgroup_heatmap(cohort(), input$x_var, input$y_var, text = 25)
  })
}

shinyApp(ui, server)
