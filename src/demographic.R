#------------------------------
# demographic.R
#------------------------------

#' Compile historical and projected demographic variables
#'
#' @return list with elements `hist` and `proj`
build_demographic_data <- function() {
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
                          mutate(across(where(is.character), as.numeric)) %>%
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
  
  list(hist = demo_hist, proj = demo_proj)
}
