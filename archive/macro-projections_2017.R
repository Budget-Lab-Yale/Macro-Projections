#------------------------------
# macro-projections.R
#------------------------------

# Load required libraries
library(openxlsx)
library(dplyr)
library(lubridate)
library(fredr)
library(tidyverse)

# Set FRED key
fredr_set_key("4cfe9b7c31406f13e6f8c1a593c18c33")

# Define input/output version and vintages
data_version <- "v3"
# a. CBO products
CBO_Budget_Hist_vintage <- "20170629"
CBO_Budget_Proj_vintage <- "20170629"
CBO_Demographic_vintage <- "20170330"
CBO_Econ_Hist_vintage   <- "20170629"
CBO_Econ_Proj_vintage   <- "20170629"
CBO_LTBO_vintage        <- "20170330"
#b. SSA products
SSA_Demographic_vintage <- "20230331"
SSA_AWI_vintage <- "20231012"
#c. Output
out_vintage <- "2017123100"

# Define first/last years of historical/projected data
firstyr_hist <- 1970
firstyr_proj <- 2017
firstyr_ltbo <- firstyr_proj + 11
lastyr_ltbo <- 2047
lastyr_proj <- 2091

# Lists of data sources and vintages
sources_hist <- c("SSA_AWI", "SSA_Demographic", "CBO_Budget_Hist","CBO_Econ_Hist")
sources_proj <- c("CBO_Budget_Proj", "CBO_Demographic", "CBO_Econ_Proj", "CBO_LTBO")
for (x in c("hist", "proj")) {
  assign(paste0("sources_",x,"_vintages"), c())
  for (data_source in get(paste0("sources_",x))) {
    assign(paste0("sources_",x,"_vintages"), c(get(paste0("sources_",x,"_vintages")), c(get(paste0(data_source,"_vintage")))))
  }
}

# Define data paths
for (data_source in sources_hist) {
  assign(paste0(data_source, "_path"),paste0("/gpfs/gibbs/project/sarin/shared/raw_data/",gsub("_","-",data_source),"/",data_version,"/", get(paste0(data_source,"_vintage")), "/historical/"))
}

for (data_source in sources_proj) {
  assign(paste0(data_source, "_path"),paste0("/gpfs/gibbs/project/sarin/shared/raw_data/",gsub("_","-",data_source),"/",data_version,"/", get(paste0(data_source,"_vintage")), "/baseline/"))
}

out_path <- paste0("/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/", out_vintage, "/baseline/")

# Create output directories if they doesn't already exist
dir.create(gsub("/baseline/","",out_path))
dir.create(out_path)

#------------------------------

# 1. ECONOMIC VARIABLES

#------------------------------
# A. HISTORICAL ECON
#------------------------------

# i. CY GDP/EMP
econ_hist_cy <- read.csv(file.path(CBO_Econ_Hist_path, "Annual_CY.csv"))
econ_hist_cy <- econ_hist_cy %>%
 select(date, gdp, empl_payroll_nf, wages_and_salaries, cpiu, chained_cpiu, pce_price_index) %>%
 rename(year = date, emp_est = empl_payroll_nf, gdp_wages = wages_and_salaries, cpiu_index = cpiu, ccpiu_index = chained_cpiu, pce_deflator_index = pce_price_index)

# ii. FY GDP
econ_hist_fy <- read.csv(file.path(CBO_Econ_Hist_path, "Annual_FY.csv"))
econ_hist_fy <- econ_hist_fy %>%
 select(date, gdp) %>%
 rename(year = date, gdp_fy = gdp)

# iii. AWI
awi <- read.csv(file.path(SSA_AWI_path, "awi_historical.csv")) %>% select(year, awi_index)

# iv. CPI/CCPI
# a. CPI-U (NSA)
# NOTE: CPI-U indexes are unrevised. As such, need only do one pull.
cpiu <- fredr("CPIAUCNS")
cpiu$date <- as.Date(cpiu$date, format = "%Y-%m-%d")
cpiu$year <- year(cpiu$date)
cpiu$month <- month(cpiu$date)
cpiu$year_irs <- ifelse(cpiu$month < 9, cpiu$year, cpiu$year + 1)
cpiu <- subset(cpiu, year_irs %in% seq(firstyr_hist, firstyr_proj))
cpiu <- aggregate(value ~ year_irs, data = cpiu, mean)
names(cpiu) <- c("year", "cpiu_irs_index")

# b. C-CPI-U (NSA)
# NOTE: Under TCJA, the IRS adjusts tax parameters from tax year (y-1) to tax 
#    year (y) by multiplying the (y-1) parameters by the ratio of:
#    -- C-CPI-U (NSA) average, September (y-2) to August (y-1); to
#    -- C-CPI-U (NSA) average, September (y-3) to August (y-2).
#    This is done using data current as of September (y-1) and September (y-2),
#    respectively.
for (y in 2012:(firstyr_proj-1)) {
 fred_raw <- fredr("SUUR0000SA0", vintage_dates = as.Date(paste0(y,"-09-30"))) %>% select(date, value)
 names(fred_raw) <- c("date", paste0("ccpiu_",y,"0930"))
 
 if (y == 2012) ccpiu <- fred_raw
 else ccpiu <- merge(ccpiu, fred_raw, by = "date", all=TRUE)
}
ccpiu$year <- year(as.Date(ccpiu$date, format = "%Y-%m-%d"))
ccpiu$month <- month(as.Date(ccpiu$date, format = "%Y-%m-%d"))
ccpiu$year_irs <- ifelse(ccpiu$month < 9, ccpiu$year, ccpiu$year + 1)
ccpiu <- ccpiu %>% select(year_irs, starts_with('ccpiu_'))
ccpiu <- aggregate(. ~ year_irs, data = ccpiu, mean, na.action=NULL)
for (y in 2012:(firstyr_proj-1)) {
  ccpiu$ccpiu_irs[ccpiu$year_irs == y] <- (ccpiu[ccpiu$year_irs == y, paste0("ccpiu_",y,"0930")])
}
ccpiu <- subset(ccpiu, year_irs %in% seq(firstyr_hist, firstyr_proj)) %>% select(year_irs, ccpiu_irs)
names(ccpiu) <- c("year", "ccpiu_irs_index")

# Merge all historical econ data
econ_hist <- econ_hist_cy %>%
 left_join(econ_hist_fy, by = "year") %>%
 left_join(awi, by = "year") %>%
 left_join(cpiu, by = "year") %>%
 left_join(ccpiu, by = "year") %>%
 filter(year >= firstyr_hist, year <= firstyr_proj - 1)

#------------------------------
# B. PROJECTED ECON
#------------------------------

# Ten-year projections, FY
econ_10yr_fy <- as.data.frame(t(read.xlsx(file.path(CBO_Econ_Proj_path, "Economic-Projections.xlsx"), sheet = "3. Fiscal Year", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE))) %>% 
        select(1,3) %>% mutate_if(is.character,as.numeric) 
names(econ_10yr_fy) <- c("year", "gdp_fy")
econ_10yr_fy <- econ_10yr_fy %>% filter(!is.na(year) & year>=firstyr_proj)

# Ten-year projections, CY
econ_10yr_cy <- as.data.frame(t(read.xlsx(file.path(CBO_Econ_Proj_path, "Economic-Projections.xlsx"), sheet = "2. Calendar Year", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE)))
econ_10yr_cy <- econ_10yr_cy %>% select(1,3,9,16,20,24,
                                        34,39,40,42,47,48,49,
                                        53,55,59,61,63,65,67,69,
                                        74,76,83) %>% mutate_if(is.character,as.numeric) 
names(econ_10yr_cy) <- c("year", "gdp", "rgdp_index", "pce_deflator_index","cpiu_index","gdp_deflator_index",
              "u3","lfpr","emp_hh", "emp_est","tsy_10y", "tsy_3m", "ffr",
              "gdp_comp","gdp_wages", "gdp_proprietors_farm","gdp_proprietors_nonfarm","gdp_rent", "gdp_interest", "gdp_div", "gdp_corp",
              "gdp_c", "gdp_i","gdp_g")
econ_10yr_cy <- econ_10yr_cy %>% filter(!is.na(year) & year>=firstyr_proj)
econ_10yr_cy$gdp_proprietors = econ_10yr_cy$gdp_proprietors_farm + econ_10yr_cy$gdp_proprietors_nonfarm

#LTBO, Econ
econ_ltbo <- as.data.frame(t(read.xlsx(file.path(CBO_LTBO_path, "LTBO.xlsx"), sheet = "2. Econ and Demographic Vars", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE))) %>%
        select(1,10,11,13,12,15,26,27,32,33) %>% mutate_if(is.character,as.numeric) 
names(econ_ltbo) <- c("year", "rgdp_index_gr", "gdp_gr", "lfpr", "lf_gr", "u3",
             "cpiu_index_gr", "gdp_deflator_index_gr", "tsy_10y", "avg_rate_debt") 
econ_ltbo_debt <- econ_ltbo[, c("year","avg_rate_debt")] %>% filter(!is.na(year) & year>=firstyr_proj & year<firstyr_ltbo)
econ_ltbo$tsy_10y_diff <- c(NA, diff(econ_ltbo$tsy_10y))
econ_ltbo <-econ_ltbo %>% filter(!is.na(year) & year>=firstyr_ltbo)
#NEW 02.11.2024: For years after lastyr_ltbo, extend by assuming that all variables are in long-run
#steady-state as of lastyr_ltbo (so zeroing out interest rate growth):
lastyr_ltbo_rate <- econ_ltbo %>% filter(year==lastyr_ltbo)
lastyr_ltbo_rate$tsy_10y_diff <- 0
for (y in (lastyr_ltbo+1):lastyr_proj) {
  lastyr_ltbo_rate$year <- y
  econ_ltbo <- bind_rows(econ_ltbo,lastyr_ltbo_rate)
}

#Combine historical and projected econ:
econ_10yr <- full_join(full_join(econ_10yr_cy, econ_10yr_fy, by="year"), econ_ltbo_debt, by = "year")
econ_all <- bind_rows(econ_hist, econ_10yr ,econ_ltbo)

#a. Project forward using projected growth rates from LTBO
ltbo_rate_proj_vars <- c("gdp", "rgdp_index", "gdp_deflator_index", "cpiu_index")
for (var in ltbo_rate_proj_vars) {
 for (y in firstyr_ltbo:lastyr_proj) {
    econ_all[econ_all$year == y, var] = econ_all[econ_all$year == y-1, var] * (1 + (econ_all[econ_all$year == y,paste0(var, "_gr")]/100))
 }
}

#b. Project forward using fixed shares of nominal GDP
for (var in c("gdp_c", "gdp_i", "gdp_g", "gdp_comp", "gdp_wages", "gdp_proprietors", "gdp_rent", "gdp_div", "gdp_corp")) {
 for (y in firstyr_ltbo:lastyr_proj) {
  econ_all[econ_all$year == y, var] = econ_all[econ_all$year == y, "gdp"] * (mean(econ_all[(econ_all$year>=(firstyr_ltbo-3) & econ_all$year<=(firstyr_ltbo-1)), var]) 
                                        /mean(econ_all[(econ_all$year>=(firstyr_ltbo-3) & econ_all$year<=(firstyr_ltbo-1)), "gdp"]))
 }
}
#Compute net exports (residual):
econ_all$gdp_nx = econ_all$gdp - econ_all$gdp_c - econ_all$gdp_i - econ_all$gdp_g

#c. Project forward using growth rates for other variables
#GDP_FY: use GDP_CY
#EMP_HH, EMP_EST: use LF growth
for (var in c("gdp_fy", "emp_hh", "emp_est")) {
 if (var =="gdp_fy")	 gr_var <- "gdp_gr"
 if (var =="emp_hh" | var =="emp_est")	 gr_var <- "lf_gr" 
 for (y in firstyr_ltbo:lastyr_proj) {
  econ_all[econ_all$year == y, var] = econ_all[econ_all$year == y-1, var] * (1 + (econ_all[econ_all$year == y, gr_var]/100))
 } 
}

#d. Project forward using level differences for other variables
#TSY_10Y, TSY_3M, FFR: use differences in TSY_10Y (from LTBO)
for (var in c("tsy_10y", "tsy_3m","ffr")) {
 diff_var <- "tsy_10y_diff"
 for (y in firstyr_ltbo:lastyr_proj) {
  econ_all[econ_all$year == y, var] = econ_all[econ_all$year == y-1, var] + econ_all[econ_all$year == y, diff_var]
 } 
}

#e. Other

#GDP_INTEREST: grows by product of (gdp growth) & (10yr Treasury rate growth)
for (y in firstyr_ltbo:lastyr_proj) {
 econ_all[econ_all$year == y, "gdp_interest"] = econ_all[econ_all$year == y-1, "gdp_interest"] * ((econ_all[econ_all$year == y, "gdp"]/econ_all[econ_all$year == y-1, "gdp"])
                                                  * (econ_all[econ_all$year == y, "tsy_10y"]/econ_all[econ_all$year == y-1, "tsy_10y"]))
} 

#AWI_INDEX: grows by ratio of (wage growth)/(emp_est growth)
firstyr_awi_proj <- max(econ_all$year[!is.na(econ_all$awi)]) + 1
for (y in firstyr_awi_proj:lastyr_proj) {
 econ_all[econ_all$year == y, "awi_index"] = econ_all[econ_all$year == y-1, "awi_index"] * ((econ_all[econ_all$year == y, "gdp_wages"]/econ_all[econ_all$year == y-1, "gdp_wages"])
                                             / (econ_all[econ_all$year == y, "emp_est"]/econ_all[econ_all$year == y-1, "emp_est"]))
}

#CCPIU_INDEX: in long run, grows by same rate as cpi_u minus average spread in growth rates over 10-year window (either from short-term econ. projections if available,
#or last 10 years of actual data if not)
cpiu_gr_avg <-((econ_all[econ_all$year == (firstyr_proj+10), "cpiu_index"])/econ_all[econ_all$year == (firstyr_proj), "cpiu_index"])^(1/10)
ccpiu_gr_avg <- ((econ_all[econ_all$year == (firstyr_proj+10), "ccpiu_index"])/econ_all[econ_all$year == (firstyr_proj), "ccpiu_index"])^(1/10)
if (is.na(ccpiu_gr_avg)) {
 cpiu_gr_avg <-((econ_all[econ_all$year == (firstyr_proj-1), "cpiu_index"])/econ_all[econ_all$year == (firstyr_proj-11), "cpiu_index"])^(1/10)
 ccpiu_gr_avg <- ((econ_all[econ_all$year == (firstyr_proj-1), "ccpiu_index"])/econ_all[econ_all$year == (firstyr_proj-11), "ccpiu_index"])^(1/10)

 for (y in firstyr_proj:(firstyr_ltbo-1)) {
  econ_all[econ_all$year == y, "ccpiu_index"] = econ_all[econ_all$year == y-1, "ccpiu_index"] * ( econ_all[econ_all$year == y, "cpiu_index"] / econ_all[econ_all$year == y-1, "cpiu_index"] ) * (ccpiu_gr_avg/cpiu_gr_avg)
 } 
}
for (y in firstyr_ltbo:lastyr_proj) {
 econ_all[econ_all$year == y, "ccpiu_index"] = econ_all[econ_all$year == y-1, "ccpiu_index"] * (1 + (econ_all[econ_all$year == y,"cpiu_index_gr"]/100)) * (ccpiu_gr_avg/cpiu_gr_avg)
}


#PCE_DEFLATOR_INDEX: grows by projected rate (if available in vintage), or by same rate as cpi_u minus average spread in growth rates over last 10 years
#of actual data (if not)
if (any(names(econ_all) == "pce_deflator_index_gr")) {
 for (y in firstyr_ltbo:lastyr_proj) {
  econ_all[econ_all$year == y, "pce_deflator_index"] = econ_all[econ_all$year == y-1, "pce_deflator_index"] * (1 + (econ_all[econ_all$year == y,"pce_deflator_index_gr"]/100))
 }
} else {
 cpiu_gr_avg <-((econ_all[econ_all$year == (firstyr_proj-1), "cpiu_index"])/econ_all[econ_all$year == (firstyr_proj-11), "cpiu_index"])^(1/10)
 pce_deflator_gr_avg <- ((econ_all[econ_all$year == (firstyr_proj-1), "pce_deflator_index"])/econ_all[econ_all$year == (firstyr_proj-11), "pce_deflator_index"])^(1/10)
 for (y in firstyr_ltbo:lastyr_proj) {
  econ_all[econ_all$year == y, "pce_deflator_index"] = econ_all[econ_all$year == y-1, "pce_deflator_index"] * (1 + (econ_all[econ_all$year == y,"cpiu_index_gr"]/100)) * (pce_deflator_gr_avg/cpiu_gr_avg)
 }
}

#CPIU_IRS_INDEX: grows by same rate as CPIU_INDEX
#CCPIU_IRS_INDEX: grows by same rate as CCCPIU_INDEX
for (y in firstyr_proj:lastyr_proj) {
 econ_all[econ_all$year == y, "cpiu_irs_index"] = econ_all[econ_all$year == y-1, "cpiu_irs_index"] * (econ_all[econ_all$year == y, "cpiu_index"]/econ_all[econ_all$year == y-1, "cpiu_index"])
 econ_all[econ_all$year == y, "ccpiu_irs_index"] = econ_all[econ_all$year == y-1, "ccpiu_irs_index"] * (econ_all[econ_all$year == y, "ccpiu_index"]/econ_all[econ_all$year == y-1, "ccpiu_index"])
}

#For some variables, rescale as cumulative growth from first year of CBO forecast
for (var in c("rgdp", "cpiu", "ccpiu", "cpiu_irs", "ccpiu_irs", "gdp_deflator", "pce_deflator", "awi")) {
 econ_all[, var] = econ_all[, paste0(var,"_index")] / econ_all[econ_all$year == firstyr_proj, paste0(var,"_index")]
}

econ_hist_order <- c("year", "gdp",	"gdp_fy",	"cpiu_irs",	"ccpiu_irs",	"awi")
econ_hist <- econ_all[, econ_hist_order] %>% filter(year>=firstyr_hist, year<=(firstyr_proj-1))

econ_proj_order <- c("year", "gdp", "gdp_c", "gdp_i", "gdp_g","gdp_nx", 
          "gdp_comp", "gdp_wages", "gdp_proprietors",	"gdp_rent", "gdp_interest", "gdp_div", "gdp_corp", "gdp_fy", "rgdp",
          "pce_deflator", "cpiu", "ccpiu", "cpiu_irs", "ccpiu_irs", "gdp_deflator", "awi",	
          "u3", "lfpr",	"emp_hh", "emp_est", "tsy_10y", "tsy_3m", "ffr", "avg_rate_debt")
econ_proj <- econ_all[, econ_proj_order] %>% filter(year>=firstyr_proj)


#------------------------------

# 2. BUDGET VARIABLES

#------------------------------
# A. HISTORICAL BUDGET
#------------------------------

#Table 2 (Revenues)
budget_hist_tab2 <- read.xlsx(file.path(CBO_Budget_Hist_path, "Historical-Budget-Data.xlsx"), sheet = "2. Revenues", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
          mutate_if(is.character,as.numeric)
names(budget_hist_tab2) <- c("year","rev_iit", "rev_payroll","rev_corp","rev_excise","rev_estate","rev_customs","rev_misc", "rev")
budget_hist_tab2 <- budget_hist_tab2 %>% filter(!is.na(year))
budget_hist_tab2 <- budget_hist_tab2[c(seq(1,50)),]


#Table 3 (Outlays)
budget_hist_tab3 <- read.xlsx(file.path(CBO_Budget_Hist_path, "Historical-Budget-Data.xlsx"), sheet = "3. Outlays", startRow = 8, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
          select(X1, Discretionary,Net.Interest,Total) %>% mutate_if(is.character,as.numeric)
names(budget_hist_tab3) <- c("year","outlays_disc","outlays_ni","outlays")
budget_hist_tab3 <- budget_hist_tab3 %>% filter(!is.na(year))
budget_hist_tab3 <- budget_hist_tab3[c(seq(1,50)),]

#Table 5 (Outlays)
budget_hist_tab5 <- read.xlsx(file.path(CBO_Budget_Hist_path, "Historical-Budget-Data.xlsx"), sheet = "5. Mandatory Outlays", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
 select(X1, Total, Social.Security, contains("health.care")) %>% mutate_if(is.character,as.numeric)
names(budget_hist_tab5) <- c("year","outlays_mand","outlays_mand_oasdi","outlays_mand_health")
budget_hist_tab5$outlays_mand_other <- budget_hist_tab5$outlays_mand - budget_hist_tab5$outlays_mand_oasdi - budget_hist_tab5$outlays_mand_health
budget_hist_tab5 <- budget_hist_tab5 %>% filter(!is.na(year))
budget_hist_tab5 <- budget_hist_tab5[c(seq(1,50)),]

#Combine all historical budget data
budget_hist <- budget_hist_tab2 %>%
 left_join(budget_hist_tab3, by = "year") %>%
 left_join(budget_hist_tab5, by = "year") %>%
 filter(year >= firstyr_hist, year <= firstyr_proj - 1)
budget_hist_order <- c("year", "rev", "rev_iit", "rev_payroll", "rev_corp", "rev_excise", "rev_estate", "rev_customs", "rev_misc",
            "outlays",	"outlays_disc",	"outlays_mand", "outlays_mand_oasdi", "outlays_mand_health", "outlays_mand_other", "outlays_ni")
budget_hist <- budget_hist[, budget_hist_order]


#------------------------------
# B. PROJECTED BUDGET
#------------------------------
#Revenues (Table 1)
rev_10yr_proj <- as.data.frame(t(read.xlsx(file.path(CBO_Budget_Proj_path, "Revenue.xlsx"), sheet = "1. Revenue Projections", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE)))
rev_10yr_proj <- rev_10yr_proj %>% select(3,4,5,6,8,9,10,11,12,15) %>% mutate_if(is.character,as.numeric) 
names(rev_10yr_proj) <- c("year", "rev_iit", "rev_payroll","rev_corp", "rev_excise", "rev_fed_remit", "rev_customs", "rev_estate", "rev_misc_fees", "rev")
rev_10yr_proj <- rev_10yr_proj %>% filter(!is.na(year) & year>=firstyr_proj)
rev_10yr_proj$rev_misc = rev_10yr_proj$rev_fed_remit + rev_10yr_proj$rev_misc_fees
rev_10yr_proj <- rev_10yr_proj[c(seq(1,11)),]

#Project forward using growth in GDP_FY
rev_vars <- c("rev", "rev_iit", "rev_payroll", "rev_corp", "rev_excise", "rev_estate", "rev_customs", "rev_misc")
rev_proj <- econ_proj %>% select(year, gdp_fy)
for (var in rev_vars) {
 rev_proj[, var] = NA
 for (y in firstyr_proj:lastyr_proj) {
   if (y<firstyr_ltbo) {
    rev_proj[rev_proj$year == y, var] = rev_10yr_proj[rev_10yr_proj$year == y, var] 
   }
   else {
    rev_proj[rev_proj$year == y, var] = rev_proj[rev_proj$year == y-1, var] * (rev_proj[rev_proj$year == y, "gdp_fy"]/rev_proj[rev_proj$year == y-1, "gdp_fy"])
   }
  }
}
rev_proj_order <- c("year", rev_vars)
rev_proj <- rev_proj[, rev_proj_order]


#Outlays:
#Ten-year outlays (BEO Table 1):
outlays_proj_10yr <- as.data.frame(t(read.xlsx(file.path(CBO_Budget_Proj_path, "Budget-Projections.xlsx"), sheet = "Table 1", 
                                                    startRow = 7, skipEmptyRows=FALSE, skipEmptyCols = TRUE, colNames=TRUE))) %>%
                                                    select(1,15,16,17,19)
names(outlays_proj_10yr) <- c("year", "outlays_mand", "outlays_disc", "outlays_ni", "outlays")
outlays_proj_10yr <- outlays_proj_10yr[c(seq(6,16)),] %>% mutate_if(is.character,as.numeric)

#Ten-year mandatory outlays, detail (BEO Table 2):
outlays_proj_10yr_mand <- as.data.frame(t(read.xlsx(file.path(CBO_Budget_Proj_path, "Budget-Projections.xlsx"), sheet = "Table 2", 
                          startRow = 7, skipEmptyRows=FALSE, skipEmptyCols = TRUE, colNames=TRUE))) %>% 
                          select(3,9,79)
names(outlays_proj_10yr_mand) <- c("year", "outlays_mand_oasdi", "outlays_mand_health")
outlays_proj_10yr_mand <- outlays_proj_10yr_mand[c(seq(7,17)),] %>% mutate_if(is.character,as.numeric)


#Long-run outlay projections (LTBO Table 1)
fig9 <- read.xlsx(file.path(CBO_LTBO_path, "LTBO.xlsx"), sheet = "Figure 9", 
                  startRow = 8, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
  mutate_if(is.character,as.numeric) 
names(fig9) <- c("year","outlays_disc_gdp_ltbo", "outlays_mand_other_gdp_ltbo") 
fig9 <- fig9 %>% filter(!is.na(year))

outlays_proj_ltbo <- read.xlsx(file.path(CBO_LTBO_path, "LTBO.xlsx"), sheet = "1. Summary Extended Baseline", 
                          startRow = 10, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
  select(Fiscal.Year,Social.Security,Medicarea,starts_with("Medicaid"), Net.Interest) %>% mutate_if(is.character,as.numeric) 
names(outlays_proj_ltbo) <- c("year", "outlays_mand_oasdi_gdp_ltbo", "Medicare", "Medicaid", "outlays_ni_gdp_ltbo")
outlays_proj_ltbo <- outlays_proj_ltbo %>% left_join(fig9, by="year")

outlays_proj_ltbo$outlays_mand_health_gdp_ltbo <- outlays_proj_ltbo$Medicare + outlays_proj_ltbo$Medicaid 
outlays_proj_ltbo$outlays_mand_gdp_ltbo <- outlays_proj_ltbo$outlays_mand_oasdi_gdp_ltbo + outlays_proj_ltbo$outlays_mand_health_gdp_ltbo + outlays_proj_ltbo$outlays_mand_other_gdp_ltbo
outlays_proj_ltbo$outlays_gdp_ltbo <- outlays_proj_ltbo$outlays_disc_gdp_ltbo + outlays_proj_ltbo$outlays_mand_gdp_ltbo + outlays_proj_ltbo$outlays_ni_gdp_ltbo
outlays_proj_ltbo <- outlays_proj_ltbo %>% filter(!is.na(year) & year>=(firstyr_ltbo-1)) %>% select(year, starts_with("outlays_"))
#For years after lastyr_ltbo, extend by assuming that all variables are in long-run
#steady-state as of lastyr_ltbo:
lastyr_ltbo_share <- outlays_proj_ltbo %>% filter(year==lastyr_ltbo)
for (y in (lastyr_ltbo+1):lastyr_proj) {
  lastyr_ltbo_share$year <- y
  outlays_proj_ltbo <- bind_rows(outlays_proj_ltbo,lastyr_ltbo_share)
}

#Join 10-year projections, and express all outlay variables as percent of FY GDP
outlays_proj <- outlays_proj_10yr %>% left_join(outlays_proj_10yr_mand, by="year") 
outlays_proj$outlays_mand_other <- outlays_proj$outlays_mand - (outlays_proj$outlays_mand_oasdi + outlays_proj$outlays_mand_health)
outlays_proj <- outlays_proj %>% full_join(econ_proj[c("year","gdp_fy")], by="year")
outlays_vars <- c("outlays", "outlays_disc","outlays_mand", "outlays_mand_oasdi", "outlays_mand_health", "outlays_mand_other", "outlays_ni")
for (var in outlays_vars) {
  outlays_proj[, paste0(var,"_gdp")] <- 100 * outlays_proj[, var] / outlays_proj[, "gdp_fy"] 
}

#Join in LTBO projections, then, for years 11+, use the LTBO share of GDP adjusted by the 
#difference in BEO vs. LTBO shares in year 10. After, multiply by FY GDP to get long-run levels:
outlays_proj <- outlays_proj %>% left_join(outlays_proj_ltbo, by="year")
for (var in outlays_vars) {
  for (y in firstyr_ltbo:lastyr_proj) {
   outlays_proj[outlays_proj$year == y, paste0(var,"_gdp")] <-  outlays_proj[outlays_proj$year== y, paste0(var,"_gdp_ltbo")] +  (outlays_proj[outlays_proj$year==(firstyr_ltbo-1), paste0(var,"_gdp")] -
                                                                                                                                 outlays_proj[outlays_proj$year==(firstyr_ltbo-1), paste0(var,"_gdp_ltbo")])
   outlays_proj[outlays_proj$year == y, var] <- (outlays_proj[outlays_proj$year == y, paste0(var,"_gdp")] * outlays_proj[outlays_proj$year == y, "gdp_fy"]) / 100
  }
}
outlays_proj_order <- c("year", "outlays", "outlays_disc", "outlays_mand", "outlays_mand_oasdi", "outlays_mand_health", "outlays_mand_other", "outlays_ni")
outlays_proj <- outlays_proj[, outlays_proj_order]

# Combine budget projections
budget_proj <- rev_proj %>% left_join(outlays_proj, by="year")

#Finally, set some revenue variables to missing for projected (these will be filled in by the Tax Simulator)
for (var in c("rev","rev_iit","rev_payroll")) {
 #budget_proj[budget_proj$year>=firstyr_proj, var] = NA
}
 
#------------------------------

# 3. DEMOGRAPHIC VARIABLES

#------------------------------
#---------------------------------------
# A. HISTORICAL AND PROJECTED POPULATION
#---------------------------------------
#UPDATED 2/3/24: Now uses SSA data for historical up to first year of CBO demographic
#projections.

#First, test to see what the first year of CBO projections is
demo_test <- read.xlsx(file.path(CBO_Demographic_path, "Demographic-Projections.xlsx"), sheet = "Pop by Age, Sex, and Marital", 
                      startRow = 7, skipEmptyRows=FALSE, skipEmptyCols = TRUE, colNames=FALSE)
firstyr_cbo_demo <- as.numeric(demo_test[1,2])

#SSA Trustees' Report counts for historical data
demo_raw <- read.csv(file.path(SSA_Demographic_path,"SSPopJan.csv"))
names(demo_raw) <- c("year", "age","total", "total_male","single_male","married_male","widowed_male", "divorced_male",
                     "total_female","single_female","married_female","widowed_female", "divorced_female" )
demo_raw[,"married"] <- demo_raw[,"married_male"] + demo_raw[,"married_female"] 
demo_raw[,"unmarried"] <- demo_raw[,"total"] - demo_raw[,"married"]
demo_raw <- demo_raw[,c("year", "age", "unmarried","married")] %>% 
  reshape(timevar = "age", idvar = "year", 
          v.names = c("unmarried", "married"), sep = "_", direction = "wide" )
demo_raw <- demo_raw %>%  select(c("year", starts_with("unmarried"),starts_with("married")))
demo <- demo_raw %>% filter(year >= firstyr_hist, year < firstyr_cbo_demo)


#For projections, loop over years, pulling population counts from CBO
for (y in firstyr_cbo_demo:lastyr_proj) {
  start_row <- 11 + 110*(y - firstyr_cbo_demo)
 demo_raw <- read.xlsx(file.path(CBO_Demographic_path, "Demographic-Projections.xlsx"), sheet = "Pop by Age, Sex, and Marital", 
               startRow = start_row, skipEmptyRows=FALSE, skipEmptyCols = TRUE, colNames=FALSE)
 names(demo_raw) <- c("age","total", "total_male","single_male","married_male","widowed_male", "divorced_male",
                           "total_female","single_female","married_female","widowed_female", "divorced_female" )
 demo_raw <- demo_raw[1:101,]
 demo_raw[demo_raw$age=="100+", "age"] <- "100"
 demo_raw <- demo_raw %>% mutate_if(is.character,as.numeric)
 demo_raw[,"year"] <- y
 demo_raw[,"married"] <- demo_raw[,"married_male"] + demo_raw[,"married_female"] 
 demo_raw[,"unmarried"] <- demo_raw[,"total"] - demo_raw[,"married"]
 demo_raw <- demo_raw[,c("year", "age", "unmarried","married")] %>% 
              reshape(timevar = "age", idvar = "year", 
                      v.names = c("unmarried", "married"), sep = "_", direction = "wide" )
 demo_raw <- demo_raw %>%  select(c("year", starts_with("unmarried"),starts_with("married")))
 demo <- bind_rows(demo, demo_raw)
}

#Finally, split into historical/projected based on specified years
demo_hist <- demo %>% filter(year >= firstyr_hist, year < firstyr_proj)
demo_proj <- demo %>% filter(year >= firstyr_proj, year <= lastyr_proj)

#------------------------------

# 4. ASSEMBLY AND EXPORT

#------------------------------
# Historical
historical <- econ_hist %>% left_join(budget_hist, by="year") %>% left_join(demo_hist, by="year") 
write.csv(historical, file = paste0(out_path,"historical.csv"), row.names = FALSE, na="")

# Projections
projections <- econ_proj %>% left_join(budget_proj, by="year") %>% left_join(demo_proj, by="year")
write.csv(projections, file = paste0(out_path,"projections.csv"), row.names = FALSE, na="")

# Dependencies
dependencies <- data.frame(ID = c(replicate((length(sources_hist)+length(sources_proj)),"baseline")),
                           interface = c(sources_hist, sources_proj),
                           version = c(replicate((length(sources_hist)+length(sources_proj)), gsub("v","",data_version))),
                           vintage = c(sources_hist_vintages, sources_proj_vintages),
                           scenario = c(replicate(length(sources_hist),"historical"), replicate(length(sources_proj),"baseline")))
write.csv(dependencies, file = paste0(gsub("/baseline/","/",out_path),"dependencies.csv"), row.names = FALSE, na="")
