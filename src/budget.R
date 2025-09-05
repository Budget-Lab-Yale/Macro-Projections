#------------------------------
# budget.R
#------------------------------

#' Compile historical and projected federal budget variables
#'
#' @return list with elements `hist` and `proj`
build_budget_data <- function(econ_proj) {
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
                          mutate(across(where(is.character), as.numeric)) %>%
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
                            mutate(across(where(is.character), as.numeric)) %>%
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
                            mutate(across(where(is.character), as.numeric)) %>%
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
                                          mutate(across(where(is.character), as.numeric)) %>%
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
                            mutate(across(where(is.character), as.numeric)) %>%
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
                                  mutate(across(where(is.character), as.numeric)) %>%
                                  filter(!is.na(year))
    
  #Long-run outlay projections (LTBO Table 1)
  outlays_proj_ltbo <- read.xlsx(file.path(CBO_LTBO_path, 'LTBO-budget.xlsx'), 
                                 sheet = '1. Summary Ext Baseline',
                                 startRow = 10, 
                                 skipEmptyRows=TRUE, 
                                 skipEmptyCols = TRUE, colNames=TRUE) %>%
                        select(Fiscal.year,Social.Security,Medicarea,starts_with('Medicaid'), 
                               Discretionary, Net.interest, Other.mandatory) %>%
                          mutate(across(where(is.character), as.numeric)) %>%
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
    list(hist = budget_hist, proj = budget_proj)
   
  #------------------------------
  
}

