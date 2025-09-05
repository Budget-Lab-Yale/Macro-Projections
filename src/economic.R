#------------------------------
# economic.R
#------------------------------

#' Compile historical and projected economic variables
#'
#' Combines CBO and SSA datasets, augments with FRED series, and projects
#' forward using LTBO growth assumptions.
#'
#' @return list with elements `hist` and `proj`
build_economic_data <- function() {
  # A. HISTORICAL ECON
  econ_hist_cy <- read.csv(file.path(CBO_Econ_Hist_path, 'Annual_CY.csv')) %>%
                    select(date, gdp, empl_payroll_nf, wages_and_salaries, cpiu,
                           chained_cpiu, pce_price_index, treasury_note_rate_10yr) %>%
                      rename(year = date, emp_est = empl_payroll_nf, gdp_wages = wages_and_salaries,
                             cpiu_index = cpiu, ccpiu_index = chained_cpiu,
                             pce_deflator_index = pce_price_index, tsy_10y = treasury_note_rate_10yr)

  econ_hist_fy <- read.csv(file.path(CBO_Econ_Hist_path, 'Annual_FY.csv')) %>%
                    select(date, gdp) %>%
                      rename(year = date, gdp_fy = gdp)

  awi <- read.csv(file.path(SSA_AWI_path, 'awi_historical.csv')) %>%
          select(year, awi_index)

  cpiu <- fredr('CPIAUCNS') %>%
            mutate(
              date = as.Date(date, format = '%Y-%m-%d'),
              year = year(date),
              month = month(date),
              year_irs = ifelse(month < 9, year, year + 1)
            ) %>%
              filter(year_irs %in% seq(firstyr_hist, firstyr_proj)) %>%
                group_by(year_irs) %>%
                  summarize(cpiu_irs_index = mean(value)) %>%
                    rename(year = year_irs)

  for (y in 2012:(firstyr_proj-1)) {
   fred_raw <- fredr('SUUR0000SA0', vintage_dates = as.Date(paste0(y,'-09-30'))) %>%
                select(date, value) %>%
                  rename(!!paste0('ccpiu_',y,'0930') := value)
   if (y == 2012) ccpiu <- fred_raw
   else ccpiu <- ccpiu %>% full_join(fred_raw, by = 'date')
  }
  ccpiu <- ccpiu %>%
            mutate(
              date = as.Date(date, format = '%Y-%m-%d'),
              year = year(date),
              month = month(date),
              year_irs = ifelse(month < 9, year, year + 1)
            ) %>%
              select(year_irs, starts_with('ccpiu_'))
  ccpiu <- aggregate(. ~ year_irs, data = ccpiu, mean, na.action=NULL)
  for (y in 2012:(firstyr_proj-1)) {
    ccpiu$ccpiu_irs_index[ccpiu$year_irs == y] <- (ccpiu[ccpiu$year_irs == y, paste0('ccpiu_',y,'0930')])
  }
  ccpiu <- ccpiu %>%  filter(year_irs %in% seq(firstyr_hist, firstyr_proj)) %>%
                        select(year_irs, ccpiu_irs_index) %>%
                          rename(year = year_irs)

  econ_hist <- econ_hist_cy %>%
                left_join(econ_hist_fy, by = 'year') %>%
                  left_join(awi, by = 'year') %>%
                    left_join(cpiu, by = 'year') %>%
                      left_join(ccpiu, by = 'year') %>%
                        filter(year >= firstyr_hist, year <= firstyr_proj - 1)

  # B. PROJECTED ECON
  econ_10yr_fy <- read.csv(file.path(CBO_Econ_Hist_path, 'Annual_FY.csv')) %>%
                    select(date, gdp) %>%
                       rename(year = date, gdp_fy = gdp) %>%
                          filter(!is.na(year) & year>=firstyr_proj)

  econ_10yr_cy <- read.csv(file.path(CBO_Econ_Hist_path, 'Annual_CY.csv')) %>%
    rename(year = date,
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
           gdp_g = government_c_gi) %>%
        select(year,
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
               gdp_g) %>%
       mutate(gdp_proprietors = gdp_proprietors_farm + gdp_proprietors_nonfarm) %>%
        filter(!is.na(year) & year>=firstyr_proj)

  econ_ltbo <- read.xlsx(file.path(CBO_LTBO_path, 'LTBO-econ.xlsx'),
                         sheet = '1. Econ Vars_Annual Rates',
                         startRow = 7,
                         skipEmptyRows=TRUE,
                         skipEmptyCols = TRUE,
                         colNames=FALSE) %>%
                    mutate(X1 = case_when(
                      X1 == 'Real GDP'                                              ~ 'rgdp_index_gr',
                      X1 == 'Nominal GDP'                                           ~ 'gdp_gr',
                      X1 == 'Labor force participation rated'                       ~ 'lfpr_ltbo',
                      X1 == 'Labor force growth'                                    ~ 'lf_gr',
                      X1 == 'Unemployment ratee'                                    ~ 'u3_ltbo',
                      X1 == 'Growth of the PCE price index'                         ~ 'pce_deflator_index_gr',
                      X1 == 'Growth of the CPI-U'                                   ~ 'cpiu_index_gr',
                      X1 == 'Growth of the GDP price index '                        ~ 'gdp_deflator_index_gr',
                      X1 == 'Nominal rates'                                         ~ 'tsy_10y_ltbo',
                      X1 == 'On all federal debt held by the public (fiscal year)h' ~ 'avg_rate_debt',
                      TRUE ~ X1
                      )
                    ) %>%
                      t %>%
                        as.data.frame
  econ_ltbo <- econ_ltbo %>% setNames(c('year', as.character(econ_ltbo[1,-1]))) %>%
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
                          filter(!is.na(year)) %>%
                            mutate(across(where(is.character), as.numeric))

  lastyr_ltbo_rate <- econ_ltbo %>% filter(year==lastyr_ltbo)
  for (y in (lastyr_ltbo+1):lastyr_proj) {
    lastyr_ltbo_rate$year <- y
    econ_ltbo <- bind_rows(econ_ltbo,lastyr_ltbo_rate)
  }

  econ_10yr <- econ_10yr_cy %>%
                  full_join(econ_10yr_fy, by='year')
  econ_all <- bind_rows(econ_hist,
                        full_join(econ_10yr, filter(econ_ltbo, year>=firstyr_proj), by='year'))

  econ_all <- econ_all %>%
    mutate(across(c('gdp_gr', 'rgdp_index_gr', 'gdp_deflator_index_gr', 'cpiu_index_gr', 'pce_deflator_index_gr', 'lf_gr'),
                  ~ 1 + .x/100)) %>%
      mutate(gdp_fy_gr = gdp_gr,
             emp_hh_gr = lf_gr,
             emp_est_gr = lf_gr,
             gdp_interest_gr = gdp_gr * (tsy_10y_ltbo/lag(tsy_10y_ltbo)),
             cpiu_irs_index_gr = cpiu_index_gr
      )
  ccpiu_spread <- econ_all %>%
                select(year, cpiu_index, ccpiu_index) %>%
                  filter(year == firstyr_proj | year == firstyr_proj+10) %>%
                    mutate(across(c('cpiu_index','ccpiu_index'), ~ (.x/lag(.x))^(1/10))) %>%
                       mutate(ccpiu_spread = ccpiu_index / cpiu_index) %>%
                         filter(!is.na(ccpiu_spread)) %>%
                            select(ccpiu_spread)
  econ_all <- econ_all %>%
                left_join(ccpiu_spread, by=character()) %>%
                  mutate(ccpiu_index_gr = cpiu_index_gr * ccpiu_spread,
                         ccpiu_irs_index_gr = ccpiu_index_gr)

  econ_all <- econ_all %>%
                mutate(tsy_10y_diff = tsy_10y_ltbo - lag(tsy_10y_ltbo),
                       tsy_3m_diff = tsy_10y_diff,
                       ffr_diff = tsy_10y_diff)

  gdp_shares <- econ_all %>%
                  select(year,
                         gdp,
                         gdp_c,
                         gdp_i,
                         gdp_g,
                         gdp_comp,
                         gdp_wages,
                         gdp_proprietors,
                         gdp_rent,
                         gdp_div,
                         gdp_corp) %>%
                    filter(year>=(firstyr_ltbo-3) & year<=(firstyr_ltbo-1)) %>%
                      summarise(across(starts_with('gdp'), mean, na.rm=TRUE)) %>%
                        mutate(across(starts_with('gdp_'), ~ .x / gdp)) %>%
                          select(-gdp) %>%
                            rename_with(~paste0(.x,'_sh'))
  econ_all <- econ_all %>% left_join(gdp_shares, by=character())

  gr_vars <- c('gdp','gdp_fy','rgdp_index','gdp_interest',
               'gdp_deflator_index', 'pce_deflator_index', 'cpiu_index', 'ccpiu_index','cpiu_irs_index','ccpiu_irs_index',
               'emp_hh','emp_est')
  sh_vars <- c('gdp_c','gdp_i','gdp_g','gdp_comp','gdp_wages','gdp_proprietors','gdp_rent','gdp_div','gdp_corp')
  diff_vars <- c('tsy_10y','tsy_3m','ffr')
  for (y in (firstyr_proj-1):lastyr_proj) {
    for (var in c(gr_vars, sh_vars, diff_vars, 'awi_index')) {

      if (var %in% gr_vars)   project <- expr(lag(!!sym(var)) * !!sym(paste0(var,'_gr')))
      if (var %in% diff_vars) project <- expr(lag(!!sym(var)) + !!sym(paste0(var,'_diff')))
      if (var %in% sh_vars)   project <- expr(gdp             * !!sym(paste0(var,'_sh')))
      if (var == 'awi_index') project <- expr(lag(!!sym(var)) * (gdp_wages/lag(gdp_wages))/
                                                                (emp_est/lag(emp_est)))

      econ_all <- econ_all %>%
        mutate(!!sym(var) := case_when(
          year == y & is.na(!!sym(var)) ~ !!project,
          TRUE ~ !!sym(var)
        ))
    }
  }
  econ_all <- econ_all %>%
    mutate(across(c('u3','lfpr'), ~ ifelse(!is.na(.x),.x, get(paste0(cur_column(),'_ltbo')))))

  econ_all <- econ_all %>% mutate(
      across(c('rgdp_index', 'cpiu_index', 'ccpiu_index', 'cpiu_irs_index', 'ccpiu_irs_index',
               'gdp_deflator_index', 'pce_deflator_index', 'awi_index'),
             ~ {baseline_value <- econ_all %>%  filter(year == firstyr_proj) %>% pull(cur_column())
               get(cur_column()) / baseline_value},
             .names = "{gsub('_index', '', .col)}"
          )
        )

  econ_all <- econ_all %>% mutate(gdp_nx = gdp - gdp_c - gdp_i - gdp_g)

  econ_hist_order <- c('year', 'gdp',     'gdp_fy', 'gdp_wages', 'cpiu', 'cpiu_irs',      'ccpiu_irs','awi', 'tsy_10y')
  econ_hist <- econ_all[, econ_hist_order] %>% filter(year>=firstyr_hist, year<=(firstyr_proj-1))

  econ_proj_order <- c('year', 'gdp', 'gdp_c', 'gdp_i', 'gdp_g','gdp_nx',
            'gdp_comp', 'gdp_wages', 'gdp_proprietors',   'gdp_rent', 'gdp_interest', 'gdp_div', 'gdp_corp', 'gdp_fy', 'rgdp',
            'pce_deflator', 'cpiu', 'ccpiu', 'cpiu_irs', 'ccpiu_irs', 'gdp_deflator', 'awi',
            'u3', 'lfpr', 'emp_hh', 'emp_est', 'tsy_10y', 'tsy_3m', 'ffr', 'avg_rate_debt')
  econ_proj <- econ_all[, econ_proj_order] %>% filter(year>=firstyr_proj)

  list(hist = econ_hist, proj = econ_proj)
}
