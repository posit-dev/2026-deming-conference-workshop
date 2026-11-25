# Load packages of functions to use
library(tidyverse)
library(reactable)
library(scales)
library(bslib)

# Load custom helper functions
source("helpers.R")

# Read in data
candidates <- read_csv("candidates.csv")

# Build interactive table
reactable(candidates, filterable = TRUE, compact = TRUE, wrap = FALSE)

# Build value boxes
cohort_value_box(candidates)
subgroups_value_box(candidates, "age_band", "region")
smallest_subgroup_value_box(candidates, "age_band", "region")

# Create heatmap
subgroup_heatmap(candidates, "age_band", "region")
