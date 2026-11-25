library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(scales)
library(querychat)

source("helpers.R")
candidates <- read.csv("candidates.csv")

qc <- querychat(
  candidates,
  table_name = "candidates",
  tools = "update",
  greeting = paste(
    "Describe the patients you want in your cohort, e.g.",
    "*women with 2+ index claims, pharmacy coverage, and an NSAID prescription*.",
    "I'll filter the candidates for you."
  ),
  data_description = paste(
    "Each row is a candidate patient for an endometriosis NSAID feasibility study.",
    "n_index_claims is the number of index endometriosis claims.",
    "rx_coverage is 'Y' or 'N' for pharmacy coverage.",
    "tx_* columns are logical flags for treatment received (GnRH antagonist/agonist,",
    "combined hormonal, progestin, aromatase inhibitor, NSAID, opioid).",
    "baseline_* columns are 'Yes'/'No' flags."
  )
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
  sidebar = qc$sidebar(width = 400),
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
  qc_vals <- qc$server()
  cohort <- reactive(qc_vals$df())

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
