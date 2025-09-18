#--------------------------------
# economic_data.R
#
# Functions for assembling economic datasets
#--------------------------------

library(dplyr)
library(lubridate)
library(openxlsx)
library(fredr)
library(purrr)
library(tidyr)

source('src/utils.R')

fred_fetch_cpiu <- function(first_hist, first_proj) {
  fredr('CPIAUCNS') %>%
    mutate(
      date = as.Date(date),
      year = year(date),
      month = month(date),
      year_irs = if_else(month < 9, year, year + 1)
    ) %>%
    filter(year_irs >= first_hist, year_irs <= first_proj) %>%
    group_by(year_irs) %>%
    summarise(cpiu_irs_index = mean(value), .groups = 'drop') %>%
    rename(year = year_irs)
}

fred_fetch_ccpiu <- function(start_year, end_year) {
  tibble(vintage_year = start_year:end_year) %>%
    mutate(
      vintage_date = as.Date(sprintf('%d-09-30', vintage_year)),
      data = map(
        vintage_date,
        ~ fredr(
          'SUUR0000SA0',
          vintage_dates = .x
        ) %>%
          transmute(
            date = as.Date(date),
            value
          )
      )
    ) %>%
    unnest(data) %>%
    mutate(
      year = year(date),
      month = month(date),
      year_irs = if_else(month < 9, year, year + 1)
    ) %>%
    group_by(year_irs, vintage_year) %>%
    summarise(ccpiu_index = mean(value), .groups = 'drop') %>%
    filter(vintage_year >= start_year, vintage_year <= end_year, vintage_year == year_irs) %>%
    transmute(year = year_irs, ccpiu_irs_index = ccpiu_index)
}

read_ltbo_rates <- function(path) {
  read.xlsx(
    file.path(path, 'LTBO-econ.xlsx'),
    sheet = '1. Econ Vars_Annual Rates',
    startRow = 7,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    colNames = FALSE
  ) %>%
    mutate(
      X1 = case_when(
        X1 == 'Real GDP' ~ 'rgdp_index_gr',
        X1 == 'Nominal GDP' ~ 'gdp_gr',
        X1 == 'Labor force participation rated' ~ 'lfpr_ltbo',
        X1 == 'Labor force growth' ~ 'lf_gr',
        X1 == 'Unemployment ratee' ~ 'u3_ltbo',
        X1 == 'Growth of the PCE price index' ~ 'pce_deflator_index_gr',
        X1 == 'Growth of the CPI-U' ~ 'cpiu_index_gr',
        X1 == 'Growth of the GDP price index ' ~ 'gdp_deflator_index_gr',
        X1 == 'Nominal rates' ~ 'tsy_10y_ltbo',
        X1 == 'On all federal debt held by the public (fiscal year)h' ~ 'avg_rate_debt',
        TRUE ~ X1
      )
    ) %>%
    t() %>%
    as.data.frame() %>%
    setNames(c('year', as.character(.[1, -1]))) %>%
    slice(-1) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(
      year,
      rgdp_index_gr,
      gdp_gr,
      lf_gr,
      lfpr_ltbo,
      u3_ltbo,
      pce_deflator_index_gr,
      cpiu_index_gr,
      gdp_deflator_index_gr,
      tsy_10y_ltbo,
      avg_rate_debt
    ) %>%
    filter(!is.na(year))
}

extend_ltbo_rates <- function(ltbo_rates, final_year) {
  last_year <- max(ltbo_rates$year)
  if (final_year <= last_year) {
    return(ltbo_rates)
  }

  last_row <- ltbo_rates %>% filter(year == last_year)
  additional_years <- tibble(year = seq(last_year + 1, final_year)) %>%
    mutate(across(-year, ~ last_row[[cur_column()]]))

  bind_rows(ltbo_rates, additional_years)
}

prepare_economic_history <- function(paths, years) {
  econ_hist_cy <- read.csv(file.path(paths$CBO_Econ_Hist, 'Annual_CY.csv')) %>%
    as_tibble() %>%
    select(
      date,
      gdp,
      empl_payroll_nf,
      wages_and_salaries,
      cpiu,
      chained_cpiu,
      pce_price_index,
      treasury_note_rate_10yr
    ) %>%
    rename(
      year = date,
      emp_est = empl_payroll_nf,
      gdp_wages = wages_and_salaries,
      cpiu_index = cpiu,
      ccpiu_index = chained_cpiu,
      pce_deflator_index = pce_price_index,
      tsy_10y = treasury_note_rate_10yr
    )

  econ_hist_fy <- read.csv(file.path(paths$CBO_Econ_Hist, 'Annual_FY.csv')) %>%
    as_tibble() %>%
    select(date, gdp) %>%
    rename(year = date, gdp_fy = gdp)

  awi <- read.csv(file.path(paths$SSA_AWI, 'awi_historical.csv')) %>%
    as_tibble() %>%
    select(year, awi_index)

  cpiu <- fred_fetch_cpiu(years$first_hist, years$first_proj)
  ccpiu <- fred_fetch_ccpiu(2012, years$first_proj - 1)

  econ_hist_cy %>%
    left_join(econ_hist_fy, by = 'year') %>%
    left_join(awi, by = 'year') %>%
    left_join(cpiu, by = 'year') %>%
    left_join(ccpiu, by = 'year') %>%
    filter(year >= years$first_hist, year <= years$first_proj - 1)
}

prepare_economic_projections <- function(paths, years) {
  econ_10yr_fy <- read.csv(file.path(paths$CBO_Econ_Hist, 'Annual_FY.csv')) %>%
    as_tibble() %>%
    select(date, gdp) %>%
    rename(year = date, gdp_fy = gdp) %>%
    filter(!is.na(year), year >= years$first_proj)

  econ_10yr_cy <- read.csv(file.path(paths$CBO_Econ_Hist, 'Annual_CY.csv')) %>%
    as_tibble() %>%
    rename(
      year = date,
      gdp = gdp,
      rgdp_index = real_gdp,
      pce_deflator_index = pce_price_index,
      cpiu_index = cpiu,
      ccpiu_index = chained_cpiu,
      gdp_deflator_index = gdp_price_index,
      u3 = unemployment_rate,
      lfpr = lfpr_16yo,
      emp_hh = empl_civ_16yo,
      emp_est = empl_payroll_nf,
      tsy_10y = treasury_note_rate_10yr,
      tsy_3m = treasury_bill_rate_3mo,
      ffr = fed_funds_rate,
      gdp_comp = compensation,
      gdp_wages = wages_and_salaries,
      gdp_proprietors_farm = prop_inc_farm_adj,
      gdp_proprietors_nonfarm = prop_inc_nonfarm_adj,
      gdp_rent = rental_inc_adj,
      gdp_interest = interest_inc_pers,
      gdp_div = dividend_inc_pers,
      gdp_corp = corp_profits_adj,
      gdp_c = pce,
      gdp_i = gross_pri_dom_invest,
      gdp_g = government_c_gi
    ) %>%
    select(
      year,
      gdp,
      rgdp_index,
      pce_deflator_index,
      cpiu_index,
      ccpiu_index,
      gdp_deflator_index,
      u3,
      lfpr,
      emp_hh,
      emp_est,
      tsy_10y,
      tsy_3m,
      ffr,
      gdp_comp,
      gdp_wages,
      gdp_proprietors_farm,
      gdp_proprietors_nonfarm,
      gdp_rent,
      gdp_interest,
      gdp_div,
      gdp_corp,
      gdp_c,
      gdp_i,
      gdp_g
    ) %>%
    mutate(gdp_proprietors = gdp_proprietors_farm + gdp_proprietors_nonfarm) %>%
    filter(!is.na(year), year >= years$first_proj)

  econ_ltbo <- read_ltbo_rates(paths$CBO_LTBO) %>%
    extend_ltbo_rates(years$last_proj)

  list(
    econ_10yr_fy = econ_10yr_fy,
    econ_10yr_cy = econ_10yr_cy,
    econ_ltbo = econ_ltbo
  )
}

assemble_economic_data <- function(paths, years) {
  econ_hist <- prepare_economic_history(paths, years)
  econ_proj_components <- prepare_economic_projections(paths, years)

  econ_10yr <- econ_proj_components$econ_10yr_cy %>%
    full_join(econ_proj_components$econ_10yr_fy, by = 'year')

  econ_all <- bind_rows(
    econ_hist,
    full_join(
      econ_10yr,
      filter(econ_proj_components$econ_ltbo, year >= years$first_proj),
      by = 'year'
    )
  ) %>%
    arrange(year)

  econ_all <- econ_all %>%
    mutate(across(
      c('gdp_gr', 'rgdp_index_gr', 'gdp_deflator_index_gr', 'cpiu_index_gr', 'pce_deflator_index_gr', 'lf_gr'),
      ~ 1 + .x / 100
    )) %>%
    mutate(
      gdp_fy_gr = gdp_gr,
      emp_hh_gr = lf_gr,
      emp_est_gr = lf_gr,
      gdp_interest_gr = gdp_gr * (tsy_10y_ltbo / lag(tsy_10y_ltbo)),
      cpiu_irs_index_gr = cpiu_index_gr
    )

  ccpiu_spread <- econ_all %>%
    select(year, cpiu_index, ccpiu_index) %>%
    filter(year %in% c(years$first_proj, years$first_proj + 10)) %>%
    arrange(year) %>%
    mutate(across(c('cpiu_index', 'ccpiu_index'), ~ (.x / lag(.x))^(1 / 10))) %>%
    summarise(ccpiu_spread = last(ccpiu_index / cpiu_index), .groups = 'drop') %>%
    pull(ccpiu_spread) %>%
    tidyr::replace_na(1)

  econ_all <- econ_all %>%
    mutate(ccpiu_index_gr = cpiu_index_gr * ccpiu_spread,
           ccpiu_irs_index_gr = ccpiu_index_gr)

  econ_all <- econ_all %>%
    mutate(
      tsy_10y_diff = tsy_10y_ltbo - lag(tsy_10y_ltbo),
      tsy_3m_diff = tsy_10y_diff,
      ffr_diff = tsy_10y_diff
    )

  gdp_shares <- econ_all %>%
    select(
      year,
      gdp,
      gdp_c,
      gdp_i,
      gdp_g,
      gdp_comp,
      gdp_wages,
      gdp_proprietors,
      gdp_rent,
      gdp_div,
      gdp_corp
    ) %>%
    filter(year >= years$first_ltbo - 3, year <= years$first_ltbo - 1) %>%
    summarise(across(starts_with('gdp'), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(across(starts_with('gdp_'), ~ .x / gdp)) %>%
    select(-gdp) %>%
    rename_with(~ paste0(.x, '_sh'))

  econ_all <- econ_all %>% left_join(gdp_shares, by = character())

  growth_vars <- c(
    'gdp', 'gdp_fy', 'rgdp_index', 'gdp_interest',
    'gdp_deflator_index', 'pce_deflator_index', 'cpiu_index', 'ccpiu_index',
    'cpiu_irs_index', 'ccpiu_irs_index', 'emp_hh', 'emp_est'
  )

  difference_vars <- c('tsy_10y', 'tsy_3m', 'ffr')
  share_vars <- c(
    'gdp_c', 'gdp_i', 'gdp_g', 'gdp_comp', 'gdp_wages',
    'gdp_proprietors', 'gdp_rent', 'gdp_div', 'gdp_corp'
  )

  econ_all <- econ_all %>%
    mutate(across(
      all_of(growth_vars),
      ~ propagate_growth(.x, .data[[paste0(cur_column(), '_gr')]])
    )) %>%
    mutate(across(
      all_of(difference_vars),
      ~ propagate_difference(.x, .data[[paste0(cur_column(), '_diff')]])
    )) %>%
    mutate(across(
      all_of(share_vars),
      ~ if_else(is.na(.x), .data$gdp * .data[[paste0(cur_column(), '_sh')]], .x)
    ))

  econ_all <- econ_all %>%
    mutate(
      awi_growth = (gdp_wages / lag(gdp_wages)) / (emp_est / lag(emp_est)),
      awi_index = propagate_growth(awi_index, awi_growth)
    )

  econ_all <- econ_all %>%
    mutate(
      across(
        c(
          'rgdp_index', 'cpiu_index', 'ccpiu_index', 'cpiu_irs_index',
          'ccpiu_irs_index', 'gdp_deflator_index', 'pce_deflator_index', 'awi_index'
        ),
        ~ {
          baseline <- econ_all %>% filter(year == years$first_proj) %>% pull(cur_column())
          .x / baseline
        },
        .names = "{gsub('_index', '', .col)}"
      )
    )

  econ_all <- econ_all %>%
    mutate(gdp_nx = gdp - gdp_c - gdp_i - gdp_g)

  econ_hist <- econ_all %>%
    select(year, gdp, gdp_fy, gdp_wages, cpiu, cpiu_irs, ccpiu_irs, awi, tsy_10y) %>%
    filter(year >= years$first_hist, year <= years$first_proj - 1)

  econ_proj <- econ_all %>%
    select(
      year,
      gdp,
      gdp_c,
      gdp_i,
      gdp_g,
      gdp_nx,
      gdp_comp,
      gdp_wages,
      gdp_proprietors,
      gdp_rent,
      gdp_interest,
      gdp_div,
      gdp_corp,
      gdp_fy,
      rgdp,
      pce_deflator,
      cpiu,
      ccpiu,
      cpiu_irs,
      ccpiu_irs,
      gdp_deflator,
      awi,
      u3,
      lfpr,
      emp_hh,
      emp_est,
      tsy_10y,
      tsy_3m,
      ffr,
      avg_rate_debt
    ) %>%
    filter(year >= years$first_proj)

  list(historical = econ_hist, projections = econ_proj)
}
