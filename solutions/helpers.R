# Internal helpers --------------------------------------------------------------

#' Count patients in every subgroup
#'
#' Validates the inputs and counts patients in each combination of the values
#' of `x_var` and `y_var`, including combinations with no patients. Factor
#' variables keep their level order. Character variables are sorted
#' alphabetically, except `age_band`, which uses its natural order. Rows with
#' missing values in either variable are dropped.
#'
#' @inheritParams subgroup_heatmap
#'
#' @return A tibble with one column per variable, `n` (patients) and
#'   `meets_min` (`TRUE` when `n >= min_n`). It has no rows if `data` has no
#'   patients with non-missing values.
#'
#' @noRd
count_subgroup_cells <- function(data, x_var, y_var, min_n = 30) {
  stopifnot(
    is.data.frame(data),
    is.character(x_var),
    length(x_var) == 1,
    is.character(y_var),
    length(y_var) == 1
  )
  missing_vars <- setdiff(c(x_var, y_var), names(data))
  if (length(missing_vars) > 0) {
    stop(
      "Column(s) not found in `data`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  if (x_var == y_var) {
    stop("`x_var` and `y_var` must be different columns.", call. = FALSE)
  }
  min_n <- max(1, min_n %||% 1)

  age_levels <- c("<18", "18-24", "25-34", "35-44", "45-50", ">50")
  as_ordered_factor <- function(v) {
    if (is.factor(v)) {
      return(droplevels(v))
    }
    vals <- unique(stats::na.omit(v))
    if (all(vals %in% age_levels)) {
      factor(v, levels = age_levels[age_levels %in% vals])
    } else {
      factor(v, levels = sort(vals))
    }
  }

  # Count every combination, keeping empty ones
  counts <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(x_var, y_var)),
        as_ordered_factor
      )
    ) |>
    dplyr::count(
      dplyr::across(dplyr::all_of(c(x_var, y_var))),
      .drop = FALSE
    ) |>
    dplyr::filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]])) |>
    dplyr::mutate(meets_min = n >= min_n)

  counts
}


# Value boxes -------------------------------------------------------------------

#' Value box: patients in the cohort
#'
#' Builds a value box showing the number of patients (rows) in `data`. The value
#' is computed when the function is called, so in a Shiny app call it inside
#' [shiny::renderUI()] to update it when the data change.
#'
#' Requires the bslib and bsicons packages.
#'
#' @param data A data frame of patients, e.g. a filtered subset of
#'   `candidates.csv`. One row per patient.
#' @param title Text shown above the value.
#' @param theme Colour theme passed to [bslib::value_box()].
#' @param icon Name of a Bootstrap icon, passed to [bsicons::bs_icon()].
#'
#' @return A [bslib::value_box()] UI element.
#'
#' @examples
#' library(bslib)
#' source("subgroup-heatmap.R")
#'
#' candidates <- read.csv("candidates.csv")
#'
#' cohort_value_box(candidates)
#' cohort_value_box(candidates, title = "Endometriosis patients", theme = "success")
#'
#' \dontrun{
#' # In a Shiny app, call it inside renderUI() so it updates with the data
#' library(shiny)
#' ui <- page_fluid(uiOutput("n_patients"))
#' server <- function(input, output) {
#'   output$n_patients <- renderUI(cohort_value_box(candidates))
#' }
#' shinyApp(ui, server)
#' }
cohort_value_box <- function(
  data,
  title = "Patients in cohort",
  theme = "primary",
  icon = "people"
) {
  stopifnot(is.data.frame(data))
  bslib::value_box(
    title,
    format(nrow(data), big.mark = ","),
    showcase = bsicons::bs_icon(icon),
    showcase_layout = "top right",
    theme = theme
  )
}

#' Value box: subgroups meeting the minimum size
#'
#' Builds a value box showing how many subgroups (combinations of the values of
#' `x_var` and `y_var`) have at least `min_n` patients, out of all subgroups,
#' e.g. "18 of 24". Empty subgroups count towards the total.
#'
#' Requires the bslib, bsicons and dplyr packages.
#'
#' @inheritParams subgroup_heatmap
#' @param title Text shown above the value.
#' @param theme Colour theme passed to [bslib::value_box()].
#' @param icon Name of a Bootstrap icon, passed to [bsicons::bs_icon()].
#'
#' @return A [bslib::value_box()] UI element.
#'
#' @examples
#' library(bslib)
#' library(dplyr)
#' source("subgroup-heatmap.R")
#'
#' candidates <- read.csv("candidates.csv")
#'
#' subgroups_value_box(candidates, "age_band", "region")
#'
#' # With a larger cutoff, fewer subgroups qualify
#' subgroups_value_box(candidates, "age_band", "region", min_n = 500)
subgroups_value_box <- function(
  data,
  x_var,
  y_var,
  min_n = 30,
  title = "Subgroups meeting minimum",
  theme = "secondary",
  icon = "check2-square"
) {
  counts <- count_subgroup_cells(data, x_var, y_var, min_n)
  bslib::value_box(
    title,
    sprintf("%d of %d", sum(counts$meets_min), nrow(counts)),
    showcase = bsicons::bs_icon(icon),
    showcase_layout = "top right",
    theme = theme
  )
}

#' Value box: patients in the smallest subgroup
#'
#' Builds a value box showing the number of patients in the smallest subgroup
#' (combination of the values of `x_var` and `y_var`). Empty subgroups count, so
#' the value is 0 when any combination has no patients. Shows "-" if there are
#' no subgroups.
#'
#' Requires the bslib, bsicons and dplyr packages.
#'
#' @inheritParams subgroup_heatmap
#' @param title Text shown above the value.
#' @param theme Colour theme passed to [bslib::value_box()].
#' @param icon Name of a Bootstrap icon, passed to [bsicons::bs_icon()].
#'
#' @return A [bslib::value_box()] UI element.
#'
#' @examples
#' library(bslib)
#' library(dplyr)
#' source("subgroup-heatmap.R")
#'
#' candidates <- read.csv("candidates.csv")
#'
#' smallest_subgroup_value_box(candidates, "age_band", "region")
#'
#' # An empty subgroup makes the smallest subgroup 0
#' no_west_teens <- subset(candidates, !(age_band == "<18" & region == "West"))
#' smallest_subgroup_value_box(no_west_teens, "age_band", "region")
#'
#' # All three boxes side by side
#' layout_column_wrap(
#'   width = "220px",
#'   cohort_value_box(candidates),
#'   subgroups_value_box(candidates, "plan_type", "urbanicity"),
#'   smallest_subgroup_value_box(candidates, "plan_type", "urbanicity")
#' )
smallest_subgroup_value_box <- function(
  data,
  x_var,
  y_var,
  title = "Smallest subgroup",
  theme = "light",
  icon = "arrows-angle-contract"
) {
  counts <- count_subgroup_cells(data, x_var, y_var)
  bslib::value_box(
    title,
    if (nrow(counts) > 0) format(min(counts$n), big.mark = ",") else "-",
    showcase = bsicons::bs_icon(icon),
    showcase_layout = "top right",
    theme = theme
  )
}

# Heat map ----------------------------------------------------------------------

#' Heatmap of patients per subgroup
#'
#' Draws a heatmap with one tile for every combination of the values of two
#' variables, including combinations with no patients. Each tile shows its
#' patient count. Subgroups with at least `min_n` patients are shaded by size
#' (darker = more patients); smaller subgroups stay light gray, so it is easy
#' to see which subgroups are large enough for statistical inference. The
#' heatmap does not have a legend. A caption below the plot states that grey
#' cells fall below the `min_n` cutoff.
#'
#' Factor variables keep their level order. Character variables are sorted
#' alphabetically, except `age_band`, which uses its natural order
#' (`<18`, `18-24`, ..., `>50`). Missing values are dropped before counting.
#'
#' Requires the dplyr, ggplot2 and scales packages.
#'
#' @param data A data frame of patients, e.g. a filtered subset of
#'   `candidates.csv`. One row per patient.
#' @param x_var Name of the column to place on the x axis, as a string.
#' @param y_var Name of the column to place on the y axis, as a string.
#' @param min_n Minimum number of patients a subgroup needs to be considered
#'   large enough. Defaults to 30.
#' @param low,high Fill colours for the smallest and largest subgroups that
#'   meet the minimum. Default to low = "#DCEBF7", high = "#08519C".
#' @param text_size Text size of the counts and axis labels, in points. Defaults to 10
#'
#' @return A ggplot object.
#'
#' @examples
#' # Run from the folder that contains subgroup-heatmap.R and candidates.csv
#' library(dplyr)
#' library(ggplot2)
#' library(scales)
#' source("subgroup-heatmap.R")
#'
#' candidates <- read.csv("candidates.csv")
#'
#' subgroup_heatmap(candidates, "age_band", "region")
#'
#' # A larger cutoff turns more subgroups gray
#' subgroup_heatmap(candidates, "plan_type", "urbanicity", min_n = 500)
#'
#' # Works on any filtered subset; subgroups with no patients show a 0
#' subset_data <- subset(candidates, !(age_band == "<18" & region == "West"))
#' subgroup_heatmap(subset_data, "age_band", "region")
subgroup_heatmap <- function(
  data,
  x_var,
  y_var,
  min_n = 30,
  low = "#DCEBF7",
  high = "#08519C",
  text_size = 10
) {
  # Turn "age_band" into "Age band" for axis titles
  make_label <- function(name) {
    name <- gsub("_", " ", name)
    paste0(toupper(substring(name, 1, 1)), substring(name, 2))
  }

  counts <- count_subgroup_cells(data, x_var, y_var, min_n)

  if (nrow(counts) == 0) {
    stop("No patients in `data` to plot.", call. = FALSE)
  }

  counts$x <- counts[[x_var]]
  counts$y <- factor(counts[[y_var]], levels = rev(levels(counts[[y_var]])))
  counts$fill_n <- ifelse(counts$meets_min, counts$n, NA)
  fill_range <- suppressWarnings(range(counts$fill_n, na.rm = TRUE))
  # Use white text on the darker cells
  dark_cell <- counts$meets_min &
    (counts$n - fill_range[1]) >= 0.4 * diff(fill_range)

  ggplot2::ggplot(counts, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = fill_n),
      colour = "white",
      linewidth = 1.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = format(n, big.mark = ",", trim = TRUE)),
      size = text_size,
      size.unit = "pt",
      colour = ifelse(
        dark_cell,
        "white",
        ifelse(counts$meets_min, "black", "grey30")
      ),
      fontface = ifelse(counts$meets_min, "bold", "plain")
    ) +
    ggplot2::scale_fill_gradient(
      low = low,
      high = high,
      na.value = "grey96",
      guide = "none"
    ) +
    ggplot2::scale_x_discrete(
      position = "top",
      labels = scales::label_wrap(14)
    ) +
    ggplot2::labs(
      x = make_label(x_var),
      y = make_label(y_var),
      caption = sprintf(
        "Blue cells have at least %s patients (darker = more). Light gray cells fall below that minimum.",
        format(min_n, big.mark = ",", trim = TRUE)
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = text_size),
      axis.title = ggplot2::element_text(size = text_size, face = "bold"),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 6)),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        colour = "grey30",
        margin = ggplot2::margin(t = 10)
      ),
      plot.caption.position = "plot"
    )
}
