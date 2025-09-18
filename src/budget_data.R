#--------------------------------
# budget_data.R
#
# Functions for assembling budget datasets
#--------------------------------

library(dplyr)
library(openxlsx)
library(purrr)
library(tidyr)
library(tibble)

source('src/utils.R')

read_budget_history <- function(paths, years) {
  revenues <- read.xlsx(
    file.path(paths$CBO_Budget_Hist, 'Historical-Budget-Data.xlsx'),
    sheet = '2. Revenues',
    startRow = 7,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    colNames = TRUE
  ) %>%
    as_tibble() %>%
    rename(
      year = X1,
      rev_iit = Individual.income.taxes,
      rev_payroll = Payroll.taxes,
      rev_corp = Corporate.income.taxes,
      rev_excise = Excise.taxes,
      rev_estate = Estate.and.gift.taxes,
      rev_customs = Customs.duties,
      rev_misc = Miscellaneous.receipts,
      rev = Total
    ) %>%
    mutate(across(-year, as.numeric)) %>%
    filter(!is.na(year))

  outlays <- read.xlsx(
    file.path(paths$CBO_Budget_Hist, 'Historical-Budget-Data.xlsx'),
    sheet = '3. Outlays',
    startRow = 9,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    colNames = TRUE
  ) %>%
    as_tibble() %>%
    select(X1, Discretionary, Net.interest, Total) %>%
    rename(
      year = X1,
      outlays_disc = Discretionary,
      outlays_ni = Net.interest,
      outlays = Total
    ) %>%
    mutate(across(-year, as.numeric)) %>%
    filter(!is.na(year))

  mandatory <- read.xlsx(
    file.path(paths$CBO_Budget_Hist, 'Historical-Budget-Data.xlsx'),
    sheet = '5. Mandatory Outlays',
    startRow = 8,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    colNames = TRUE
  ) %>%
    as_tibble() %>%
    select(X1, Total, Social.Security, matches('health.care')) %>%
    rename(
      year = X1,
      outlays_mand = Total,
      outlays_mand_oasdi = Social.Security
    ) %>%
    mutate(across(-year, as.numeric)) %>%
    mutate(
      outlays_mand_health = rowSums(select(., matches('health.care')), na.rm = TRUE),
      outlays_mand_other = outlays_mand - outlays_mand_oasdi - outlays_mand_health
    ) %>%
    select(year, outlays_mand, outlays_mand_oasdi, outlays_mand_health, outlays_mand_other) %>%
    mutate(across(-year, as.numeric)) %>%
    filter(!is.na(year))

  revenues %>%
    left_join(outlays, by = 'year') %>%
    left_join(mandatory, by = 'year') %>%
    filter(year >= years$first_hist, year <= years$first_proj - 1) %>%
    select(
      year,
      rev,
      rev_iit,
      rev_payroll,
      rev_corp,
      rev_excise,
      rev_estate,
      rev_customs,
      rev_misc,
      outlays,
      outlays_disc,
      outlays_mand,
      outlays_mand_oasdi,
      outlays_mand_health,
      outlays_mand_other,
      outlays_ni
    )
}

read_revenue_projections <- function(paths, years, econ_proj) {
  rev_10yr <- read.xlsx(
    file.path(paths$CBO_Budget_Proj, 'Revenue-Projections.xlsx'),
    sheet = '1. Revenue Projections',
    startRow = 7,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    colNames = FALSE
  ) %>%
    slice(2:19) %>%
    mutate(
      X1 = case_when(
        X1 == 'Fiscal year' ~ 'year',
        X1 == 'Individual income taxes' ~ 'rev_iit',
        X1 == 'Payroll taxes' ~ 'rev_payroll',
        X1 == 'Corporate income taxes' ~ 'rev_corp',
        X1 == 'Excise taxes' ~ 'rev_excise',
        X1 == 'Federal Reserve remittances' ~ 'rev_fed_remit',
        X1 == 'Customs duties' ~ 'rev_customs',
        X1 == 'Estate and gift taxes' ~ 'rev_estate',
        X1 == 'Miscellaneous fees and fines' ~ 'rev_misc_fees',
        X1 == 'Total' ~ 'rev',
        TRUE ~ X1
      )
    ) %>%
    t() %>%
    as.data.frame() %>%
    setNames(tolower(as.character(.[1, ]))) %>%
    slice(-1) %>%
    mutate(across(everything(), as.numeric)) %>%
    as_tibble() %>%
    filter(year >= years$first_proj)

  revenue_vars <- c('rev', 'rev_iit', 'rev_payroll', 'rev_corp', 'rev_excise', 'rev_estate', 'rev_customs')

  rev_10yr <- rev_10yr %>%
    mutate(rev_misc = rev_fed_remit + rev_misc_fees) %>%
    select(year, all_of(c(revenue_vars, 'rev_misc')))

  rev_proj <- econ_proj %>%
    select(year, gdp_fy) %>%
    left_join(rev_10yr, by = 'year') %>%
    arrange(year) %>%
    mutate(gdp_fy_growth = gdp_fy / lag(gdp_fy)) %>%
    mutate(across(all_of(c(revenue_vars, 'rev_misc')), ~ propagate_growth(.x, gdp_fy_growth))) %>%
    select(year, all_of(c(revenue_vars, 'rev_misc')))

  rev_proj
}

read_outlay_projections <- function(paths, years, econ_proj) {
  outlays_10yr <- read.xlsx(
    file.path(paths$CBO_Budget_Proj, 'Budget-Projections.xlsx'),
    sheet = 'Table B-2',
    startRow = 5,
    skipEmptyRows = FALSE,
    skipEmptyCols = TRUE,
    colNames = TRUE
  ) %>%
    slice(1:9) %>%
    rename(X1 = starts_with('Table')) %>%
    mutate(
      X1 = case_when(
        X1 == 'Mandatory' ~ 'outlays_mand',
        X1 == 'Discretionary' ~ 'outlays_disc',
        X1 == 'Net interest' ~ 'outlays_ni',
        X1 == 'Total' ~ 'outlays',
        str_detect(X2, 'Actual') ~ 'year',
        TRUE ~ X1
      )
    ) %>%
    t() %>%
    as.data.frame() %>%
    setNames(tolower(as.character(.[1, ]))) %>%
    slice(-1) %>%
    mutate(across(everything(), as.numeric)) %>%
    as_tibble() %>%
    select(year, outlays, starts_with('outlays_')) %>%
    filter(!is.na(year))

  outlays_10yr_mand <- read.xlsx(
    file.path(paths$CBO_Budget_Proj, 'Budget-Projections.xlsx'),
    sheet = 'Table B-4',
    startRow = 7,
    skipEmptyRows = FALSE,
    skipEmptyCols = TRUE,
    colNames = TRUE
  ) %>%
    slice(c(1:5, 64:n())) %>%
    mutate(
      X1 = case_when(
        X1 == 'Subtotal' ~ 'outlays_mand_oasdi',
        X1 == 'Major health care programs' ~ 'outlays_mand_health',
        str_detect(X2, 'Actual') ~ 'year',
        TRUE ~ X1
      )
    ) %>%
    t() %>%
    as.data.frame() %>%
    setNames(tolower(as.character(.[1, ]))) %>%
    slice(-1) %>%
    mutate(across(everything(), as.numeric)) %>%
    as_tibble() %>%
    select(year, outlays_mand_oasdi, outlays_mand_health) %>%
    filter(!is.na(year))

  outlays_ltbo <- read.xlsx(
    file.path(paths$CBO_LTBO, 'LTBO-budget.xlsx'),
    sheet = '1. Summary Ext Baseline',
    startRow = 10,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    colNames = TRUE
  ) %>%
    as_tibble() %>%
    select(
      Fiscal.year,
      Social.Security,
      Medicarea,
      matches('^Medicaid'),
      Discretionary,
      Net.interest,
      Other.mandatory
    ) %>%
    rename(
      year = Fiscal.year,
      outlays_mand_oasdi_gdp_ltbo = Social.Security,
      medicare = Medicarea,
      outlays_disc_gdp_ltbo = Discretionary,
      outlays_ni_gdp_ltbo = Net.interest,
      outlays_mand_other_gdp_ltbo = Other.mandatory
    ) %>%
    mutate(across(-year, as.numeric)) %>%
    mutate(
      medicaid_total = rowSums(select(., matches('^Medicaid')), na.rm = TRUE),
      outlays_mand_health_gdp_ltbo = medicare + medicaid_total,
      outlays_mand_gdp_ltbo = outlays_mand_oasdi_gdp_ltbo + outlays_mand_health_gdp_ltbo + outlays_mand_other_gdp_ltbo,
      outlays_gdp_ltbo = outlays_disc_gdp_ltbo + outlays_mand_gdp_ltbo + outlays_ni_gdp_ltbo
    ) %>%
    select(year, starts_with('outlays_')) %>%
    filter(!is.na(year), year >= years$first_ltbo - 1)

  outlays_ltbo <- extend_outlay_ltbo(outlays_ltbo, years$last_proj)

  outlay_vars <- c(
    'outlays',
    'outlays_disc',
    'outlays_mand',
    'outlays_mand_oasdi',
    'outlays_mand_health',
    'outlays_mand_other',
    'outlays_ni'
  )

  outlays <- econ_proj %>%
    select(year, gdp_fy) %>%
    left_join(outlays_10yr, by = 'year') %>%
    left_join(outlays_10yr_mand, by = 'year') %>%
    mutate(outlays_mand_other = outlays_mand - outlays_mand_oasdi - outlays_mand_health) %>%
    mutate(across(all_of(outlay_vars), ~ 100 * .x / gdp_fy, .names = '{.col}_gdp')) %>%
    left_join(outlays_ltbo, by = 'year')

  adjustments <- outlays %>%
    filter(year == years$first_ltbo - 1) %>%
    mutate(across(
      all_of(paste0(outlay_vars, '_gdp')),
      ~ .x - .data[[paste0(cur_column(), '_ltbo')]],
      .names = "{gsub('_gdp', '_adj', .col)}"
    )) %>%
    select(ends_with('_adj')) %>%
    slice(1) %>%
    as.list()

  outlays <- outlays %>%
    mutate(across(
      all_of(outlay_vars),
      ~ if_else(
        !is.na(.x),
        .x,
        gdp_fy * (.data[[paste0(cur_column(), '_gdp_ltbo')]] + adjustments[[paste0(cur_column(), '_adj')]]) / 100
      )
    )) %>%
    select(year, all_of(outlay_vars))

  outlays
}

extend_outlay_ltbo <- function(outlays_ltbo, final_year) {
  last_year <- max(outlays_ltbo$year)
  if (final_year <= last_year) {
    return(outlays_ltbo)
  }

  last_row <- outlays_ltbo %>% filter(year == last_year)
  additional <- tibble(year = seq(last_year + 1, final_year)) %>%
    mutate(across(-year, ~ last_row[[cur_column()]]))

  bind_rows(outlays_ltbo, additional)
}

assemble_budget_data <- function(paths, years, econ_proj) {
  budget_hist <- read_budget_history(paths, years)
  rev_proj <- read_revenue_projections(paths, years, econ_proj)
  outlays_proj <- read_outlay_projections(paths, years, econ_proj)

  budget_proj <- rev_proj %>%
    left_join(outlays_proj, by = 'year') %>%
    mutate(
      rev = NA_real_,
      rev_iit = NA_real_,
      rev_payroll = NA_real_
    )

  list(historical = budget_hist, projections = budget_proj)
}
