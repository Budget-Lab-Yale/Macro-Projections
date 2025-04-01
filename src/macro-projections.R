#------------------------------
# macro-projections.R
#------------------------------

#------------------------------

# 0. PARAMETERS

#------------------------------
# A. USER INPUTS
#------------------------------
# Define first/last years of historical/projected data
firstyr_hist <- 1970
firstyr_proj <- 2025
firstyr_ltbo <- firstyr_proj + 11
lastyr_ltbo <- 2055
lastyr_proj <- 2098

# Define input/output version and vintages
data_version <- 'v3'
# a. CBO products
for (x in c('CBO_Budget_Hist', 'CBO_Budget_Proj', 'CBO_Econ_Hist', 'CBO_Econ_Proj')) {
  if (x == 'CBO_Budget_Hist') assign(paste0(x,'_vintage'), '20250130')
  else assign(paste0(x,'_vintage'), '20250117')
}
CBO_Demographic_vintage <- '20250113'
CBO_LTBO_vintage        <- '20250327'
#b. SSA products
SSA_Demographic_vintage <- '20240506'
SSA_AWI_vintage <- '20241010'
#c. Output
out_vintage <- '2025040115'


#---------------------------------------
# B. SET PATHS AND SOURCE FUNCTIONS/KEYS
#---------------------------------------

# Set working directory
setwd('~/project/repositories/Macro-Projections')

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Source utility functions
source('src/utils.R')

# Set FRED key
parse_api_keys('~/project/api_keys.csv', keep=c('fred'))
fredr_set_key(fred_key)

# Lists of data sources and vintages
sources_hist <- c('SSA_AWI', 'SSA_Demographic', 'CBO_Budget_Hist','CBO_Econ_Hist')
sources_proj <- c('CBO_Budget_Proj', 'CBO_Demographic', 'CBO_Econ_Proj', 'CBO_LTBO')
for (x in c('hist', 'proj')) {
  assign(paste0('sources_',x,'_vintages'), c())
  for (data_source in get(paste0('sources_',x))) {
    assign(paste0('sources_',x,'_vintages'), 
           c(get(paste0('sources_',x,'_vintages')), 
             c(get(paste0(data_source,'_vintage')))))
  }
}

# Define data paths
for (data_source in sources_hist) {
  assign(paste0(data_source, '_path'),
         paste0('/gpfs/gibbs/project/sarin/shared/raw_data/',
                gsub('_','-',data_source),'/',data_version,'/', 
                get(paste0(data_source,'_vintage')), '/historical/'))
}
for (data_source in sources_proj) {
  assign(paste0(data_source, '_path'),
         paste0('/gpfs/gibbs/project/sarin/shared/raw_data/',
                gsub('_','-',data_source),'/',data_version,'/', 
                get(paste0(data_source,'_vintage')), '/baseline/'))
}
out_path <- paste0('/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/', 
                   data_version, '/', out_vintage, '/baseline/')

# Create output directories if they don't already exist
dir.create(gsub('/baseline/','',out_path))
dir.create(out_path)

#------------------------------

# 1. ECONOMIC VARIABLES

#------------------------------
# A. HISTORICAL ECON
#------------------------------

# i. CY GDP/EMP
econ_hist_cy <- read.csv(file.path(CBO_Econ_Hist_path, 'Annual_CY.csv')) %>%
                  select(date, gdp, empl_payroll_nf, wages_and_salaries, cpiu, 
                         chained_cpiu, pce_price_index, treasury_note_rate_10yr) %>%
                    rename(year = date, emp_est = empl_payroll_nf, gdp_wages = wages_and_salaries, 
                           cpiu_index = cpiu, ccpiu_index = chained_cpiu, 
                           pce_deflator_index = pce_price_index, tsy_10y = treasury_note_rate_10yr)

# ii. FY GDP
econ_hist_fy <- read.csv(file.path(CBO_Econ_Hist_path, 'Annual_FY.csv')) %>%
                  select(date, gdp) %>%
                    rename(year = date, gdp_fy = gdp)

# iii. AWI
awi <- read.csv(file.path(SSA_AWI_path, 'awi_historical.csv')) %>% 
        select(year, awi_index)

# iv. CPI/CCPI
# a. CPI-U (NSA)
# NOTE: CPI-U indexes are unrevised. As such, need only do one pull from FRED.
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

# b. C-CPI-U (NSA)
# NOTE: Under TCJA, the IRS adjusts tax parameters from tax year (y-1) to tax 
#    year (y) by multiplying the (y-1) parameters by the ratio of:
#    -- C-CPI-U (NSA) average, September (y-2) to August (y-1); to
#    -- C-CPI-U (NSA) average, September (y-3) to August (y-2).
#    This is done using data current as of September (y-1) and September (y-2),
#    respectively.
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

# Merge all historical econ data
econ_hist <- econ_hist_cy %>%
              left_join(econ_hist_fy, by = 'year') %>%
                left_join(awi, by = 'year') %>%
                  left_join(cpiu, by = 'year') %>%
                    left_join(ccpiu, by = 'year') %>%
                      filter(year >= firstyr_hist, year <= firstyr_proj - 1)

#------------------------------
# B. PROJECTED ECON
#------------------------------
#a. Compile data

# Get 10-year projections from machine-readable "Historical Econ" files
# Ten-year projections, FY
econ_10yr_fy <- read.csv(file.path(CBO_Econ_Hist_path, 'Annual_FY.csv')) %>%
                  select(date, gdp) %>%
                     rename(year = date, gdp_fy = gdp) %>% 
                        filter(!is.na(year) & year>=firstyr_proj)

# Ten-year projections, CY
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
                  
  
#LTBO, Econ
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
                            mutate_if(is.character,as.numeric)

# For years after lastyr_ltbo, extend by assuming that all variables are in long-run
# steady-state as of lastyr_ltbo
lastyr_ltbo_rate <- econ_ltbo %>% filter(year==lastyr_ltbo)
for (y in (lastyr_ltbo+1):lastyr_proj) {
  lastyr_ltbo_rate$year <- y
  econ_ltbo <- bind_rows(econ_ltbo,lastyr_ltbo_rate)
}

#Combine historical and projected econ (average rate on debt only found in LTBO file):
econ_10yr <- econ_10yr_cy %>% 
                full_join(econ_10yr_fy, by='year')
econ_all <- bind_rows(econ_hist,
                      full_join(econ_10yr, filter(econ_ltbo, year>=firstyr_proj), by='year'))

#b. Get growth factors for each variable
# i. Projected growth rates from LTBO
econ_all <- econ_all %>%
  mutate(across(c('gdp_gr', 'rgdp_index_gr', 'gdp_deflator_index_gr', 'cpiu_index_gr', 'pce_deflator_index_gr', 'lf_gr'), 
                ~ 1 + .x/100)) %>%
    mutate(gdp_fy_gr = gdp_gr,
           emp_hh_gr = lf_gr,
           emp_est_gr = lf_gr,
           gdp_interest_gr = gdp_gr * (tsy_10y/lag(tsy_10y)),
           cpiu_irs_index_gr = cpiu_index_gr
    )
#CCPIU_INDEX: in long run, grows by same rate as cpi_u minus average spread in growth rates over 10-year window
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

# ii. Level differences from LTBO
econ_all <- econ_all %>% 
              mutate(tsy_10y_diff = tsy_10y_ltbo - lag(tsy_10y_ltbo),
                     tsy_3m_diff = tsy_10y_diff,
                     ffr_diff = tsy_10y_diff
                )

# iii. Fixed shares of nominal GDP (using final three years of ten-year projections)
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
  

#c. Project all variables forward (starting a few years before firstyr_proj in case
#   anything is missing there)
#   For AWI, growing by ratio of wage growth to employment growth (doing this last so
#   those variables are filled in for all years)
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
#Fill in some missing variables from LTBO
econ_all <- econ_all %>%
  mutate(across(c('u3','lfpr'), ~ ifelse(!is.na(.x),.x, get(paste0(cur_column(),'_ltbo')))))

#For some variables, rescale as cumulative growth from first year of CBO forecast
econ_all <- econ_all %>% mutate(
    across(c('rgdp_index', 'cpiu_index', 'ccpiu_index', 'cpiu_irs_index', 'ccpiu_irs_index', 
             'gdp_deflator_index', 'pce_deflator_index', 'awi_index'),
           ~ {baseline_value <- econ_all %>%  filter(year == firstyr_proj) %>% pull(cur_column())
             get(cur_column()) / baseline_value},
           .names = "{gsub('_index', '', .col)}"
        )
      )

#Compute net exports (residual):
econ_all <- econ_all %>% mutate(gdp_nx = gdp - gdp_c - gdp_i - gdp_g)

#Split back out into historical/projected data frames
econ_hist_order <- c('year', 'gdp',	'gdp_fy', 'gdp_wages', 'cpiu', 'cpiu_irs',	'ccpiu_irs','awi', 'tsy_10y')
econ_hist <- econ_all[, econ_hist_order] %>% filter(year>=firstyr_hist, year<=(firstyr_proj-1))

econ_proj_order <- c('year', 'gdp', 'gdp_c', 'gdp_i', 'gdp_g','gdp_nx', 
          'gdp_comp', 'gdp_wages', 'gdp_proprietors',	'gdp_rent', 'gdp_interest', 'gdp_div', 'gdp_corp', 'gdp_fy', 'rgdp',
          'pce_deflator', 'cpiu', 'ccpiu', 'cpiu_irs', 'ccpiu_irs', 'gdp_deflator', 'awi',	
          'u3', 'lfpr',	'emp_hh', 'emp_est', 'tsy_10y', 'tsy_3m', 'ffr', 'avg_rate_debt')
econ_proj <- econ_all[, econ_proj_order] %>% filter(year>=firstyr_proj)


#------------------------------

# 2. BUDGET VARIABLES

#------------------------------
# A. HISTORICAL BUDGET
#------------------------------

#Table 2 (Revenues)
budget_hist_tab2 <- read.xlsx(file.path(CBO_Budget_Hist_path, 'Historical-Budget-Data.xlsx'), 
                              sheet = '2. Revenues', 
                              startRow = 7, 
                              skipEmptyRows=TRUE, 
                              skipEmptyCols = TRUE, 
                              colNames=TRUE) %>%
                        mutate_if(is.character,as.numeric) %>%
                          rename(year = X1,
                                 rev_iit = Individual.income.taxes,
                                 rev_payroll = Payroll.taxes,
                                 rev_corp = Corporate.income.taxes,
                                 rev_excise = Excise.taxes,
                                 rev_estate = Estate.and.gift.taxes,
                                 rev_customs = Customs.duties,
                                 rev_misc = Miscellaneous.receipts,
                                 rev = Total) %>%
                            filter(!is.na(year))

#Table 3 (Outlays)
budget_hist_tab3 <- read.xlsx(file.path(CBO_Budget_Hist_path, 'Historical-Budget-Data.xlsx'), 
                              sheet = '3. Outlays', 
                              startRow = 9, 
                              skipEmptyRows=TRUE, 
                              skipEmptyCols = TRUE, 
                              colNames=TRUE) %>%
                        select(X1, Discretionary,Net.interest,Total) %>% 
                          mutate_if(is.character,as.numeric) %>%
                            rename(year = X1,
                                   outlays_disc = Discretionary,
                                   outlays_ni = Net.interest,
                                   outlays = Total) %>%
                              filter(!is.na(year))

#Table 5 (Outlays)
budget_hist_tab5 <- read.xlsx(file.path(CBO_Budget_Hist_path, 'Historical-Budget-Data.xlsx'), 
                              sheet = '5. Mandatory Outlays', 
                              startRow = 8, 
                              skipEmptyRows=TRUE, 
                              skipEmptyCols = TRUE, 
                              colNames=TRUE) %>%
                        select(X1, Total, Social.Security, contains('health.care')) %>%
                          mutate_if(is.character,as.numeric) %>%
                            rename(year = X1,
                                   outlays_mand = Total,
                                   outlays_mand_oasdi = Social.Security,
                                   outlays_mand_health = contains('health.care')) %>%
                              mutate(outlays_mand_other = outlays_mand - outlays_mand_oasdi - outlays_mand_health) %>%
                                filter(!is.na(year))

#Combine all historical budget data
budget_hist <- budget_hist_tab2 %>%
                left_join(budget_hist_tab3, by = 'year') %>%
                  left_join(budget_hist_tab5, by = 'year') %>%
                    filter(year >= firstyr_hist, year <= firstyr_proj - 1)
budget_hist <- budget_hist[,  c('year', 
                                'rev', 'rev_iit', 'rev_payroll', 'rev_corp', 
                                  'rev_excise', 'rev_estate', 'rev_customs', 'rev_misc',
                                'outlays',	'outlays_disc',	'outlays_mand', 'outlays_mand_oasdi', 
                                    'outlays_mand_health', 'outlays_mand_other', 'outlays_ni')]


#------------------------------
# B. PROJECTED BUDGET
#------------------------------
#Revenues (Table 1)
rev_10yr_proj <- read.xlsx(file.path(CBO_Budget_Proj_path, 'Revenue-Projections.xlsx'), 
                           sheet = '1. Revenue Projections', 
                           startRow = 7, 
                           skipEmptyRows=TRUE, 
                           skipEmptyCols = TRUE, 
                           colNames=FALSE) %>%
                    slice(2:19) %>%
                      mutate(X1 = case_when(
                        X1 == 'Fiscal year'                  ~ 'year',
                        X1 == 'Individual income taxes'      ~ 'rev_iit',
                        X1 == 'Payroll taxes'                ~ 'rev_payroll',
                        X1 == 'Corporate income taxes'       ~ 'rev_corp',
                        X1 == 'Excise taxes'                 ~ 'rev_excise',
                        X1 == 'Federal Reserve remittances'  ~ 'rev_fed_remit',
                        X1 == 'Customs duties'               ~ 'rev_customs',
                        X1 == 'Estate and gift taxes'        ~ 'rev_estate',
                        X1 == 'Miscellaneous fees and fines' ~ 'rev_misc_fees',
                        X1 == 'Total' ~ 'rev',
                        TRUE ~ X1
                      )
                      ) %>%
                        t %>%
                        as.data.frame
rev_10yr_proj <- rev_10yr_proj %>% setNames(tolower(as.character(rev_10yr_proj[1,]))) %>%
                                    select(year, starts_with('rev')) %>% 
                                        mutate_if(is.character,as.numeric) %>%
                                          filter(!is.na(year) & year>=firstyr_proj) %>%
                                            mutate(rev_misc = rev_fed_remit + rev_misc_fees) 

#Project forward using growth in GDP_FY
rev_proj <- econ_proj %>% select(year, gdp_fy) %>% 
              left_join(rev_10yr_proj)
rev_vars <- c('rev', 'rev_iit', 'rev_payroll', 'rev_corp', 'rev_excise', 'rev_estate', 'rev_customs', 'rev_misc')
for (y in firstyr_proj:lastyr_proj) {
  for (var in rev_vars) {
    rev_proj <- rev_proj %>% 
      mutate(!!sym(var) := case_when(
        year == y & is.na(!!sym(var)) ~ lag(!!sym(var)) * (gdp_fy/lag(gdp_fy)),
        TRUE ~ !!sym(var)
      ))
  }
}
rev_proj <- rev_proj[, c('year', rev_vars)]


#Outlays:
#Ten-year outlays, adjusted for timing shifts (BEO Table B-2):
outlays_proj_10yr <- read.xlsx(file.path(CBO_Budget_Proj_path, 'Budget-Projections.xlsx'), 
                               sheet = 'Table B-2', 
                               startRow = 5, 
                               skipEmptyRows=FALSE, 
                               skipEmptyCols = TRUE, 
                               colNames=TRUE)  %>%
                        slice(1:9) %>% 
                          rename(X1 = starts_with('Table')) %>%
                            mutate(X1 = case_when(
                              X1 == 'Mandatory'      ~ 'outlays_mand',
                              X1 == 'Discretionary'  ~ 'outlays_disc',
                              X1 == 'Net interest'   ~ 'outlays_ni',
                              X1 == 'Total'          ~ 'outlays',
                              grepl('Actual', X2)    ~ 'year',
                              TRUE ~ X1
                            )
                            ) %>%
                             t %>%
                              as.data.frame
outlays_proj_10yr <- outlays_proj_10yr %>% 
                      setNames(tolower(as.character(outlays_proj_10yr[1,]))) %>%
                        select(year, outlays, starts_with('outlays_')) %>% 
                          mutate_if(is.character,as.numeric) %>%
                          filter(!is.na(year))

#Ten-year mandatory outlays, detail, adjusted for timing shifts (BEO Table B-4):
outlays_proj_10yr_mand <- read.xlsx(file.path(CBO_Budget_Proj_path, 'Budget-Projections.xlsx'), 
                                    sheet = 'Table B-4', 
                                    startRow = 7, 
                                    skipEmptyRows=FALSE, 
                                    skipEmptyCols = TRUE, 
                                    colNames=TRUE) %>%
                            slice(1:5,64:n()) %>%
                              mutate(X1 = case_when(
                                X1 == 'Subtotal'                    ~ 'outlays_mand_oasdi',
                                X1 == 'Major health care programs'  ~ 'outlays_mand_health',
                                grepl('Actual', X2)                 ~ 'year',
                                TRUE ~ X1
                              )
                              ) %>%
                                t %>%
                                  as.data.frame
outlays_proj_10yr_mand <- outlays_proj_10yr_mand %>% 
                            setNames(tolower(as.character(outlays_proj_10yr_mand[1,]))) %>%
                              select(year,outlays_mand_oasdi,outlays_mand_health) %>%
                                mutate_if(is.character,as.numeric) %>%
                                filter(!is.na(year))
  
#Long-run outlay projections (LTBO Table 1)
outlays_proj_ltbo <- read.xlsx(file.path(CBO_LTBO_path, 'LTBO-budget.xlsx'), 
                               sheet = '1. Summary Ext Baseline',
                               startRow = 10, 
                               skipEmptyRows=TRUE, 
                               skipEmptyCols = TRUE, colNames=TRUE) %>%
                      select(Fiscal.year,Social.Security,Medicarea,starts_with('Medicaid'), 
                             Discretionary, Net.interest, Other.mandatory) %>%
                        mutate_if(is.character,as.numeric) %>%
                          rename(year = Fiscal.year,
                                 outlays_mand_oasdi_gdp_ltbo = Social.Security,
                                 Medicare = Medicarea,
                                 Medicaid = starts_with('Medicaid'),
                                 outlays_disc_gdp_ltbo = Discretionary,
                                 outlays_ni_gdp_ltbo = Net.interest,
                                 outlays_mand_other_gdp_ltbo = Other.mandatory) %>%
                            mutate(
                              outlays_mand_health_gdp_ltbo = Medicare + Medicaid,
                              outlays_mand_gdp_ltbo = outlays_mand_oasdi_gdp_ltbo + 
                                                      outlays_mand_health_gdp_ltbo + 
                                                      outlays_mand_other_gdp_ltbo,
                              outlays_gdp_ltbo = outlays_disc_gdp_ltbo + 
                                                 outlays_mand_gdp_ltbo +
                                                 outlays_ni_gdp_ltbo
                            ) %>%
                              filter(!is.na(year) & year>=(firstyr_ltbo-1)) %>% 
                                select(year, starts_with('outlays_'))
#For years after lastyr_ltbo, extend by assuming that all variables are in long-run
#steady-state as of lastyr_ltbo:
lastyr_ltbo_share <- outlays_proj_ltbo %>% filter(year==lastyr_ltbo)
for (y in (lastyr_ltbo+1):lastyr_proj) {
  lastyr_ltbo_share$year <- y
  outlays_proj_ltbo <- bind_rows(outlays_proj_ltbo,lastyr_ltbo_share)
}

#Join projections, and express all outlay variables as percent of FY GDP
outlays_proj <- econ_proj %>% 
                  select(year, gdp_fy) %>% 
                    left_join(outlays_proj_10yr , by='year') %>%
                      left_join(outlays_proj_10yr_mand, by='year') %>%
                        mutate(outlays_mand_other = outlays_mand - outlays_mand_oasdi - outlays_mand_health) %>%
                          mutate(across(
                            c('outlays', 'outlays_disc','outlays_mand', 'outlays_mand_oasdi', 
                              'outlays_mand_health', 'outlays_mand_other', 'outlays_ni'),
                            ~ {100 * get(cur_column()) / gdp_fy},
                            .names = "{paste0(.col,'_gdp')}"
                          )) %>%
                            left_join(outlays_proj_ltbo, by='year')

#For years 11+, use the LTBO share of GDP adjusted by the difference in BEO vs. LTBO shares
#in year 10 (in cases where BEO has been released but LTBO has't yet). 
#After, multiply by FY GDP to get long-run levels:
outlays_vars <- c('outlays', 'outlays_disc','outlays_mand', 
                  'outlays_mand_oasdi', 'outlays_mand_health', 
                  'outlays_mand_other', 'outlays_ni')
outlays_adjust <- outlays_proj %>%
  select(year, 
         contains('gdp')) %>%
          filter(year==firstyr_ltbo-1) %>%
            mutate(across(all_of(paste0(outlays_vars,'_gdp')),
                  ~ .x - get(paste0(cur_column(),'_ltbo')),
                  .names = "{gsub('_gdp', '_adj', .col)}")) %>%
              select(contains('adj'))
outlays_proj <- outlays_proj %>% 
                  left_join(outlays_adjust, by=character()) %>%
                    mutate(across(outlays_vars,
                                  ~ifelse(!is.na(.x),
                                          .x,
                                          gdp_fy * (get(paste0(cur_column(),'_gdp_ltbo')) + get(paste0(cur_column(),'_adj'))) / 100)
                                  ))
outlays_proj <- outlays_proj[, c('year', 'outlays', 'outlays_disc', 'outlays_mand', 
                                 'outlays_mand_oasdi', 'outlays_mand_health', 'outlays_mand_other', 
                                 'outlays_ni')]

# Combine budget projections and set some revenue variables to missing for 
# projected (these will be filled in by Tax-Simulator)
budget_proj <- rev_proj %>% 
                left_join(outlays_proj, by='year')  %>%
                  mutate(rev = NA,
                         rev_iit = NA,
                         rev_payroll = NA)
 
#------------------------------

# 3. DEMOGRAPHIC VARIABLES

#------------------------------
#---------------------------------------
# A. HISTORICAL AND PROJECTED POPULATION
#---------------------------------------
#First, test to see what the first year of CBO projections is
demo_test <- read.xlsx(file.path(CBO_Demographic_path, 'Demographic-Projections.xlsx'), 
                       sheet = '2. Pop by age, sex, marital', 
                       startRow = 7, 
                       skipEmptyRows=FALSE, 
                       skipEmptyCols = TRUE, 
                       colNames=FALSE)
firstyr_cbo_demo <- as.numeric(demo_test[1,2])

#SSA Trustees' Report counts for historical data
demo <- read.csv(file.path(SSA_Demographic_path,'SSPopJan.csv')) %>%
          rename(year = Year,
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
            mutate(married = married_male + married_female,
                   unmarried = total - married) %>%
              select(year, age, married, unmarried) %>% 
                reshape(timevar = 'age', idvar = 'year', 
                        v.names = c('unmarried', 'married'), sep = '_', direction = 'wide' ) %>%
                  select(year, starts_with('unmarried'),starts_with('married')) %>%
                    filter(year >= firstyr_hist, year < firstyr_cbo_demo)

#For projections, loop over years, pulling population counts from CBO
for (y in firstyr_cbo_demo:lastyr_proj) {
 start_row <- 11 + 107 * (y - firstyr_cbo_demo)
 demo_raw <- read.xlsx(file.path(CBO_Demographic_path, 'Demographic-Projections.xlsx'), 
                       sheet = '2. Pop by age, sex, marital', 
                       startRow = start_row, 
                       skipEmptyRows=FALSE, 
                       skipEmptyCols = TRUE, 
                       colNames=FALSE) %>%
                rename(age = X1,
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
                       divorced_female = X12) %>% 
                    mutate(age = ifelse(age == '100+', '100', age)) %>%
                        mutate_if(is.character,as.numeric) %>%
                          filter(!is.na(age)) %>%
                            mutate(year = y,
                                   married = married_male + married_female,
                                   unmarried = total - married) %>%
                              select(year, age, unmarried, married) %>% 
                                reshape(timevar = 'age', idvar = 'year', 
                                        v.names = c('unmarried', 'married'), sep = '_', direction = 'wide' ) %>%
                                  select(year, starts_with('unmarried'),starts_with('married'))
  demo <- bind_rows(demo, demo_raw)
}

#Finally, split into historical/projected based on specified years
demo_hist <- demo %>% filter(year >= firstyr_hist, year < firstyr_proj)
demo_proj <- demo %>% filter(year >= firstyr_proj, year <= lastyr_proj)

#------------------------------

# 4. ASSEMBLY AND EXPORT

#------------------------------
# Historical
historical <- econ_hist %>% left_join(budget_hist, by='year') %>% 
                              left_join(demo_hist, by='year') 
write.csv(historical, file = paste0(out_path,'historical.csv'), row.names = FALSE, na='')

# Projections
projections <- econ_proj %>% left_join(budget_proj, by='year') %>% 
                              left_join(demo_proj, by='year')
write.csv(projections, file = paste0(out_path,'projections.csv'), row.names = FALSE, na='')

# Dependencies
dependencies <- data.frame(ID = c(replicate((length(sources_hist)+length(sources_proj)),'baseline')),
                           interface = c(sources_hist, sources_proj),
                           version = c(replicate((length(sources_hist)+length(sources_proj)), gsub('v','',data_version))),
                           vintage = c(sources_hist_vintages, sources_proj_vintages),
                           scenario = c(replicate(length(sources_hist),'historical'), replicate(length(sources_proj),'baseline')))
write.csv(dependencies, file = paste0(gsub('/baseline/','/',out_path),'dependencies.csv'), row.names = FALSE, na='')
