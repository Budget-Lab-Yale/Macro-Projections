#--------------------------------
# demographic_data.R
#
# Functions for assembling demographic datasets
#--------------------------------

library(dplyr)
library(openxlsx)
library(purrr)
library(tidyr)

read_first_cbo_demographic_year <- function(path) {
  read.xlsx(
    file.path(path, 'Demographic-Projections.xlsx'),
    sheet = '2. Pop by age, sex, marital',
    startRow = 7,
    skipEmptyRows = FALSE,
    skipEmptyCols = TRUE,
    colNames = FALSE
  )[1, 2] %>%
    as.numeric()
}

read_ssa_demographics <- function(path, years, first_cbo_year) {
  read.csv(file.path(path, 'SSPopJan.csv')) %>%
    as_tibble() %>%
    rename(
      year = Year,
      age = Age,
      total = Total,
      total_male = M.Tot,
      single_male = M.Sin,
      married_male = M.Mar,
      widowed_male = M.Wid,
      divorced_male = M.Div,
      total_female = F.Tot,
      single_female = F.Sin,
      married_female = F.Mar,
      widowed_female = F.Wid,
      divorced_female = F.Div
    ) %>%
    mutate(
      age = if_else(age == '100+', '100', as.character(age)),
      age = as.integer(age),
      married = married_male + married_female,
      unmarried = total - married
    ) %>%
    select(year, age, unmarried, married) %>%
    filter(year >= years$first_hist, year < first_cbo_year) %>%
    pivot_wider(
      names_from = age,
      values_from = c(unmarried, married),
      names_glue = '{.value}_{age}',
      values_fill = 0
    ) %>%
    arrange(year)
}

read_cbo_demographics_year <- function(path, year, first_year) {
  start_row <- 11 + 107 * (year - first_year)
  read.xlsx(
    file.path(path, 'Demographic-Projections.xlsx'),
    sheet = '2. Pop by age, sex, marital',
    startRow = start_row,
    skipEmptyRows = FALSE,
    skipEmptyCols = TRUE,
    colNames = FALSE
  ) %>%
    as_tibble() %>%
    rename(
      age = X1,
      total = X2,
      total_male = X3,
      single_male = X4,
      married_male = X5,
      widowed_male = X6,
      divorced_male = X7,
      total_female = X8,
      single_female = X9,
      married_female = X10,
      widowed_female = X11,
      divorced_female = X12
    ) %>%
    mutate(
      age = if_else(age == '100+', '100', as.character(age)),
      age = as.integer(age)
    ) %>%
    filter(!is.na(age)) %>%
    mutate(
      year = year,
      married = married_male + married_female,
      unmarried = total - married
    ) %>%
    select(year, age, unmarried, married) %>%
    pivot_wider(
      names_from = age,
      values_from = c(unmarried, married),
      names_glue = '{.value}_{age}',
      values_fill = 0
    )
}

read_cbo_demographics <- function(path, years, first_cbo_year) {
  tibble(year = seq(first_cbo_year, years$last_proj)) %>%
    mutate(data = map(year, ~ read_cbo_demographics_year(path, .x, first_cbo_year))) %>%
    unnest(data)
}

assemble_demographic_data <- function(paths, years) {
  first_cbo_year <- read_first_cbo_demographic_year(paths$CBO_Demographic)

  historical <- read_ssa_demographics(paths$SSA_Demographic, years, first_cbo_year)
  projected <- read_cbo_demographics(paths$CBO_Demographic, years, first_cbo_year)

  list(
    historical = historical %>% filter(year < years$first_proj),
    projections = projected %>% filter(year >= years$first_proj)
  )
}
