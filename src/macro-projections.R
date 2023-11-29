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

# Define input/output vintages
cbo_vintage <- "2023112915"
ssa_vintage <- "2023112915"
out_vintage <- "2023112915"

# Define first years of historical/projected data
firstyr_hist <- 1970
firstyr_proj <- 2023
firstyr_ltbo <- firstyr_proj + 11
lastyr_proj <- firstyr_proj + 30

# Define data paths
cbo_path <- paste0("/gpfs/gibbs/project/sarin/shared/raw_data/CBO/v2/", cbo_vintage, "/baseline/")
ssa_path <- paste0("/gpfs/gibbs/project/sarin/shared/raw_data/SSA/v2/", ssa_vintage, "/historical/")
out_path <- paste0("/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v2/", out_vintage, "/baseline/")

# Create output directories if they doesn't already exist
dir.create(gsub("/baseline/","",out_path))
dir.create(out_path)

#------------------------------

# 1. ECONOMIC VARIABLES

#------------------------------
# A. HISTORICAL ECON
#------------------------------

# i. CY GDP/EMP
econ_hist_cy <- read.csv(file.path(cbo_path, "Annual_CY.csv"))
econ_hist_cy <- econ_hist_cy %>%
 select(date, gdp, empl_payroll_nf, wages_and_salaries, cpiu, chained_cpiu, pce_price_index) %>%
 rename(year = date, emp_est = empl_payroll_nf, gdp_wages = wages_and_salaries, cpiu_index = cpiu, ccpiu_index = chained_cpiu, pce_deflator_index = pce_price_index)

# ii. FY GDP
econ_hist_fy <- read.csv(file.path(cbo_path, "Annual_FY.csv"))
econ_hist_fy <- econ_hist_fy %>%
 select(date, gdp) %>%
 rename(year = date, gdp_fy = gdp)

# iii. AWI
awi <- read.csv(file.path(ssa_path, "awi_historical.csv")) %>% select(year, awi_index)

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
names(cpiu) <- c("year", "cpiu_irs")
#Rescale so that IRS year (firstyr_proj) = 1:
cpiu[, "cpiu_irs"] = cpiu[, "cpiu_irs"] / cpiu[cpiu$year == firstyr_proj, "cpiu_irs"]


# b. C-CPI-U (NSA)
# NOTE: Under TCJA, the IRS adjusts tax parameters from tax year (y-1) to tax 
#    year (y) by multiplying the (y-1) parameters by the ratio of:
#    -- C-CPI-U (NSA) average, September (y-2) to August (y-1); to
#    -- C-CPI-U (NSA) average, September (y-3) to August (y-2).
#    This is done using data current as of September (y-1). However, the 
#    C-CPI-U index for a given month is not finalized until up to twelve 
#    months after its initial release. This means that the denominator of 
#    the ratio will always use final values, while the numerator of the 
#    ratio will always use at least some initial (i.e. to-be-revised) values. 
#    To account for this fact, we create an annual pseudo-index beginning with 
#    (September 2016 - August 2017) = 100, corresponding to the base year for
#    inflation-indexed provisions under the TCJA.
for (y in 2017:firstyr_proj) {
 fred_raw <- fredr("SUUR0000SA0", vintage_dates = as.Date(paste0(y,"-09-30"))) %>% select(date, value)
 names(fred_raw) <- c("date", paste0("ccpiu_",y,"0930"))
 
 if (y == 2017) ccpiu <- fred_raw
 else ccpiu <- merge(ccpiu, fred_raw, by = "date", all=TRUE)
}
ccpiu$year <- year(as.Date(ccpiu$date, format = "%Y-%m-%d"))
ccpiu$month <- month(as.Date(ccpiu$date, format = "%Y-%m-%d"))
ccpiu$year_irs <- ifelse(ccpiu$month < 9, ccpiu$year, ccpiu$year + 1)
ccpiu <- ccpiu %>% select(year_irs, starts_with('ccpiu_'))
ccpiu <- aggregate(. ~ year_irs, data = ccpiu, mean, na.action=NULL)
ccpiu$ccpiu_irs[ccpiu$year_irs == 2017] <- 100
for (y in 2018:firstyr_proj) {
 ccpiu$ccpiu_irs[ccpiu$year_irs == y] <- 100 * (ccpiu[ccpiu$year_irs == y, paste0("ccpiu_",y,"0930")]) / (ccpiu[ccpiu$year_irs == 2017, paste0("ccpiu_",y,"0930")])
}
ccpiu <- subset(ccpiu, year_irs %in% seq(firstyr_hist, firstyr_proj)) %>% select(year_irs, ccpiu_irs)
names(ccpiu) <- c("year", "ccpiu_irs")
#Rescale so that IRS year (firstyr_proj) = 1:
ccpiu[, "ccpiu_irs"] = ccpiu[, "ccpiu_irs"] / ccpiu[ccpiu$year == firstyr_proj, "ccpiu_irs"]


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
econ_10yr_fy <- as.data.frame(t(read.xlsx(file.path(cbo_path, "Economic-Projections.xlsx"), sheet = "3. Fiscal Year", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE))) %>% 
        select(1,3) %>% mutate_if(is.character,as.numeric) 
names(econ_10yr_fy) <- c("year", "gdp_fy")
econ_10yr_fy <- econ_10yr_fy %>% filter(!is.na(year) & year>=firstyr_proj)

# Ten-year projections, CY
econ_10yr_cy <- as.data.frame(t(read.xlsx(file.path(cbo_path, "Economic-Projections.xlsx"), sheet = "2. Calendar Year", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE)))
if (cbo_vintage == "2017123100") {
 econ_10yr_cy <- econ_10yr_cy %>% select(1,3,9,16,20,24,34,39,40,42,47,48,49,53,55,59,61,63,65,67,69,74,76,83) %>% mutate_if(is.character,as.numeric) 
 names(econ_10yr_cy) <- c("year", "gdp", "rgdp_index", "pce_deflator_index","cpiu_index","gdp_deflator_index",
              "u3","lfpr","emp_hh", "emp_est","tsy_10y", "tsy_3m", "ffr",
              "gdp_comp","gdp_wages", "gdp_proprietors_farm","gdp_proprietors_nonfarm","gdp_rent", "gdp_interest", "gdp_div", "gdp_corp",
              "gdp_c", "gdp_i","gdp_g")
} else {
 econ_10yr_cy <- econ_10yr_cy %>% select(1,3,7,38,42,46,48,58,61,62,64,75,76,77,81,83,87,89,91,93,95,97,102,104,111) %>% mutate_if(is.character,as.numeric) 
 names(econ_10yr_cy) <- c("year", "gdp", "rgdp_index", "pce_deflator_index","cpiu_index","ccpiu_index","gdp_deflator_index",
              "u3","lfpr","emp_hh", "emp_est","tsy_10y", "tsy_3m", "ffr",
              "gdp_comp","gdp_wages", "gdp_proprietors_farm","gdp_proprietors_nonfarm","gdp_rent", "gdp_interest", "gdp_div", "gdp_corp",
              "gdp_c", "gdp_i","gdp_g")
}
econ_10yr_cy <- econ_10yr_cy %>% filter(!is.na(year) & year>=firstyr_proj)
econ_10yr_cy$gdp_proprietors = econ_10yr_cy$gdp_proprietors_farm + econ_10yr_cy$gdp_proprietors_nonfarm

#LTBO, Econ
if (cbo_vintage == "2017123100") {
 econ_ltbo <- as.data.frame(t(read.xlsx(file.path(cbo_path, "LTBO.xlsx"), sheet = "2. Econ and Demographic Vars", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE))) %>% 
        select(1,10,11,12,13,15,26,27,32,33) %>% mutate_if(is.character,as.numeric) 
 names(econ_ltbo) <- c("year", "rgdp_index_gr", "gdp_gr", "lf_gr", "lfpr", "u3",
            "cpiu_index_gr", "gdp_deflator_index_gr", "tsy_10y", "avg_rate_debt") 
} else {
 econ_ltbo <- as.data.frame(t(read.xlsx(file.path(cbo_path, "LTBO-econ.xlsx"), sheet = "1. Econ Vars_Annual Rates", startRow = 8, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE))) %>%
        select(1,6,8,9,10,12,23,24,25,28,30) %>% mutate_if(is.character,as.numeric) 
 names(econ_ltbo) <- c("year", "rgdp_index_gr", "gdp_gr", "lfpr", "lf_gr", "u3",
            "pce_deflator_index_gr", "cpiu_index_gr", "gdp_deflator_index_gr", "tsy_10y", "avg_rate_debt") 
}
econ_ltbo_debt <- econ_ltbo[, c("year","avg_rate_debt")] %>% filter(!is.na(year) & year>=firstyr_proj & year<firstyr_ltbo)
econ_ltbo <- econ_ltbo %>% filter(!is.na(year) & year>=firstyr_ltbo)

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
 if (var =="emp_hh")	 gr_var <- "lf_gr"
 if (var =="emp_est") gr_var <- "lf_gr"
 for (y in firstyr_ltbo:lastyr_proj) {
  econ_all[econ_all$year == y, var] = econ_all[econ_all$year == y-1, var] * (1 + (econ_all[econ_all$year == y, gr_var]/100))
 } 
}

#d. Project forward using level differences for other variables
#TSY_3M, FFR: use differences in TSY_10Y
for (var in c("tsy_3m","ffr")) {
 if (var =="tsy_3m")	 diff_var <- "tsy_10y"
 if (var =="ffr")	  diff_var <- "tsy_10y"
 for (y in firstyr_ltbo:lastyr_proj) {
  econ_all[econ_all$year == y, var] = econ_all[econ_all$year == y-1, var] + (econ_all[econ_all$year == y, diff_var] - econ_all[econ_all$year == y-1, diff_var])
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
"pce_deflator_index"
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

#CPIU_IRS: grows by same rate as CPIU_INDEX
#CCPIU_IRS: grows by same rate as CCCPIU_INDEX
econ_all[econ_all$year == firstyr_proj, "cpiu_irs"] <- 1
econ_all[econ_all$year == firstyr_proj, "ccpiu_irs"] <- 1
for (y in (firstyr_proj+1):lastyr_proj) {
 econ_all[econ_all$year == y, "cpiu_irs"] = econ_all[econ_all$year == y-1, "cpiu_irs"] * (econ_all[econ_all$year == y, "cpiu_index"]/econ_all[econ_all$year == y-1, "cpiu_index"])
 econ_all[econ_all$year == y, "ccpiu_irs"] = econ_all[econ_all$year == y-1, "ccpiu_irs"] * (econ_all[econ_all$year == y, "ccpiu_index"]/econ_all[econ_all$year == y-1, "ccpiu_index"])
}

#For some variables, rescale as cumulative growth from first year of CBO forecast
for (var in c("rgdp", "cpiu", "ccpiu", "gdp_deflator", "pce_deflator", "awi")) {
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
budget_hist_tab2 <- read.xlsx(file.path(cbo_path, "Historical-Budget-Data.xlsx"), sheet = "2. Revenues", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
          mutate_if(is.character,as.numeric)
names(budget_hist_tab2) <- c("year","rev_iit", "rev_payroll","rev_corp","rev_excise","rev_estate","rev_customs","rev_misc", "rev")
budget_hist_tab2 <- budget_hist_tab2 %>% filter(!is.na(year))

#Table 3 (Outlays)
budget_hist_tab3 <- read.xlsx(file.path(cbo_path, "Historical-Budget-Data.xlsx"), sheet = "3. Outlays", startRow = 9, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
          select(X1, Discretionary,Net.Interest,Total) %>% mutate_if(is.character,as.numeric)
names(budget_hist_tab3) <- c("year","outlays_disc","outlays_ni","outlays")
budget_hist_tab3 <- budget_hist_tab3 %>% filter(!is.na(year))

#Table 5 (Outlays)
budget_hist_tab5 <- read.xlsx(file.path(cbo_path, "Historical-Budget-Data.xlsx"), sheet = "5. Mandatory Outlays", startRow = 8, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
 select(X1, Total, Social.Security, starts_with("Memorandum")) %>% mutate_if(is.character,as.numeric)
names(budget_hist_tab5) <- c("year","outlays_mand","outlays_mand_oasi","outlays_mand_health")
budget_hist_tab5$outlays_mand_other <- budget_hist_tab5$outlays_mand - budget_hist_tab5$outlays_mand_oasi - budget_hist_tab5$outlays_mand_health
budget_hist_tab5 <- budget_hist_tab5 %>% filter(!is.na(year))

#Combine all historical budget data
budget_hist <- budget_hist_tab2 %>%
 left_join(budget_hist_tab3, by = "year") %>%
 left_join(budget_hist_tab5, by = "year") %>%
 filter(year >= firstyr_hist, year <= firstyr_proj - 1)
budget_hist_order <- c("year", "rev", "rev_iit", "rev_payroll", "rev_corp", "rev_excise", "rev_estate", "rev_customs", "rev_misc",
            "outlays",	"outlays_disc",	"outlays_mand", "outlays_mand_oasi", "outlays_mand_health", "outlays_mand_other", "outlays_ni")
budget_hist <- budget_hist[, budget_hist_order]


#------------------------------
# B. PROJECTED BUDGET
#------------------------------
#Revenues (Table 1)
rev_10yr_proj <- as.data.frame(t(read.xlsx(file.path(cbo_path, "Revenue.xlsx"), sheet = "1. Revenue Projections", startRow = 7, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=FALSE)))
if (cbo_vintage == "2017123100") {
 rev_10yr_proj <- rev_10yr_proj %>% select(3,4,5,6,8,9,10,11,12,15) %>% mutate_if(is.character,as.numeric) 
} else {
 rev_10yr_proj <- rev_10yr_proj %>% select(2,3,4,5,7,8,9,10,11,13) %>% mutate_if(is.character,as.numeric) 
}
names(rev_10yr_proj) <- c("year", "rev_iit", "rev_payroll","rev_corp", "rev_excise", "rev_fed_remit", "rev_customs", "rev_estate", "rev_misc_fees", "rev")
rev_10yr_proj <- rev_10yr_proj %>% filter(!is.na(year) & year>=firstyr_proj)
rev_10yr_proj$rev_misc = rev_10yr_proj$rev_fed_remit + rev_10yr_proj$rev_misc_fees

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


#Outlays (LTBO Table 1)
if (cbo_vintage == "2017123100") {
 fig9 <- read.xlsx(file.path(cbo_path, "LTBO.xlsx"), sheet = "Figure 9", 
          startRow = 8, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
      mutate_if(is.character,as.numeric) 
 names(fig9) <- c("year","outlays_disc_gdp", "outlays_mand_other_gdp") 
 fig9 <- fig9 %>% filter(!is.na(year))
 
 outlays_proj <- read.xlsx(file.path(cbo_path, "LTBO.xlsx"), sheet = "1. Summary Extended Baseline", 
              startRow = 10, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
  select(Fiscal.Year,Social.Security,Medicarea,starts_with("Medicaid"), Net.Interest) %>% mutate_if(is.character,as.numeric) 
 names(outlays_proj) <- c("year", "outlays_mand_oasi_gdp", "Medicare", "Medicaid", "outlays_ni_gdp")
 outlays_proj <- outlays_proj %>% left_join(fig9, by="year")

} else {
 outlays_proj <- read.xlsx(file.path(cbo_path, "LTBO-budget.xlsx"), sheet = "1. Summary Ext Baseline", 
              startRow = 10, skipEmptyRows=TRUE, skipEmptyCols = TRUE, colNames=TRUE) %>%
  select(Fiscal.Year,Social.Security,Medicarea,starts_with("Medicaid"), Discretionary, Net.Interest, Other.Mandatory) %>% mutate_if(is.character,as.numeric) 
 names(outlays_proj) <- c("year", "outlays_mand_oasi_gdp", "Medicare", "Medicaid", 
              "outlays_disc_gdp", "outlays_ni_gdp", "outlays_mand_other_gdp")
}
outlays_proj$outlays_mand_health_gdp <- outlays_proj$Medicare + outlays_proj$Medicaid 
outlays_proj$outlays_mand_gdp <- outlays_proj$outlays_mand_oasi_gdp + outlays_proj$outlays_mand_health_gdp + outlays_proj$outlays_mand_other_gdp
outlays_proj$outlays_gdp <- outlays_proj$outlays_disc_gdp + outlays_proj$outlays_mand_gdp + outlays_proj$outlays_ni_gdp
outlays_proj <- outlays_proj %>% filter(!is.na(year) & year>=firstyr_proj) %>% select(year, starts_with("outlays_"))

#Project forward using GDP shares in LTBO
outlays_vars <- c("outlays", "outlays_disc","outlays_mand", "outlays_mand_oasi", "outlays_mand_health", "outlays_mand_other", "outlays_ni")
for (var in outlays_vars) {
 outlays_proj[, var] = NA
 for (y in firstyr_proj:lastyr_proj) {
  outlays_proj[outlays_proj$year == y, var] = econ_proj[econ_proj$year == y, "gdp_fy"] * (outlays_proj[outlays_proj$year == y, paste0(var,"_gdp")]/100)
 }
}
outlays_proj_order <- c("year", "outlays", "outlays_disc", "outlays_mand", "outlays_mand_oasi", "outlays_mand_health", "outlays_mand_other", "outlays_ni")
outlays_proj <- outlays_proj[, outlays_proj_order]

# Combine budget projections
budget_proj <- rev_proj %>% left_join(outlays_proj, by="year")

#Finally, set some revenue variables to missing for projected (these will be filled in by the Tax Simulator)
for (var in c("rev","rev_iit","rev_payroll")) {
 budget_proj[budget_proj$year>=firstyr_proj, var] = NA
}
 
#------------------------------

# 3. DEMOGRAPHIC PROJECTIONS

#------------------------------

#Initialize data frame for projections
demo_proj <- data.frame(seq.int(firstyr_proj,lastyr_proj))
names(demo_proj) <- c("year")
demo_vars <- c("pop", "pop_0_5", "pop_6_17", "pop_18_24", "pop_25_34", 
        "pop_35_44", "pop_45_54", "pop_55_64", "pop_65_74",
        "pop_75_84", "pop_85_plus")
for (var in demo_vars) {
 demo_proj[, var] = NA
}

for (y in firstyr_proj:lastyr_proj) {
 start_row <- 11 + 110*(y - firstyr_proj)
 demo_proj_raw <- read.xlsx(file.path(cbo_path, "Demographic-Projections.xlsx"), sheet = "Pop by Age, Sex, and Marital", 
               startRow = start_row, skipEmptyRows=FALSE, skipEmptyCols = TRUE, colNames=FALSE) %>%
         select(1,2,3,5,8,10)
 demo_proj_raw <- demo_proj_raw[1:101,]
 names(demo_proj_raw) <- c("age","total", "total_male","married_male","total_female","married_female")
 demo_proj_raw[demo_proj_raw$age=="100+", "age"] <- "100"
 demo_proj_raw <- demo_proj_raw %>% mutate_if(is.character,as.numeric)

 demo_proj_raw$married <- demo_proj_raw$married_male + demo_proj_raw$married_female
 demo_proj_raw$unmarried <- demo_proj_raw$total - demo_proj_raw$married
 
 demo_proj[demo_proj$year == y, "pop"]            = sum(demo_proj_raw[, "total"])    /1000000 
 demo_proj[demo_proj$year == y, "pop_married"]    = sum(demo_proj_raw[, "married"])  /1000000 
 demo_proj[demo_proj$year == y, "pop_unmarried"]  = sum(demo_proj_raw[, "unmarried"])/1000000 

 demo_proj[demo_proj$year == y, "pop_0_5"]     = sum(demo_proj_raw[demo_proj_raw$age>=0 & demo_proj_raw$age<=5, "total"])   /1000000 
 demo_proj[demo_proj$year == y, "pop_6_17"]    = sum(demo_proj_raw[demo_proj_raw$age>=6 & demo_proj_raw$age<=17, "total"])  /1000000 
 demo_proj[demo_proj$year == y, "pop_18_24"]   = sum(demo_proj_raw[demo_proj_raw$age>=18 & demo_proj_raw$age<=24, "total"]) /1000000 
 demo_proj[demo_proj$year == y, "pop_25_34"]   = sum(demo_proj_raw[demo_proj_raw$age>=25 & demo_proj_raw$age<=34, "total"]) /1000000 
 demo_proj[demo_proj$year == y, "pop_35_44"]   = sum(demo_proj_raw[demo_proj_raw$age>=35 & demo_proj_raw$age<=44, "total"]) /1000000 
 demo_proj[demo_proj$year == y, "pop_45_54"]   = sum(demo_proj_raw[demo_proj_raw$age>=45 & demo_proj_raw$age<=54, "total"]) /1000000 
 demo_proj[demo_proj$year == y, "pop_55_64"]   = sum(demo_proj_raw[demo_proj_raw$age>=55 & demo_proj_raw$age<=64, "total"]) /1000000 
 demo_proj[demo_proj$year == y, "pop_65_74"]   = sum(demo_proj_raw[demo_proj_raw$age>=65 & demo_proj_raw$age<=74, "total"]) /1000000 
 demo_proj[demo_proj$year == y, "pop_75_84"]   = sum(demo_proj_raw[demo_proj_raw$age>=75 & demo_proj_raw$age<=84, "total"]) /1000000 
 demo_proj[demo_proj$year == y, "pop_85_plus"] = sum(demo_proj_raw[demo_proj_raw$age>=85 & demo_proj_raw$age<=100, "total"])/1000000 
}


#------------------------------

# 4. ASSEMBLY AND EXPORT

#------------------------------
# Historical
historical <- econ_hist %>% left_join(budget_hist, by="year")
write.csv(historical, file = paste0(out_path,"historical.csv"), row.names = FALSE, na="")

# Projections
projections <-demo_proj %>% left_join(econ_proj, by="year") %>% left_join(budget_proj, by="year")
write.csv(projections, file = paste0(out_path,"projections.csv"), row.names = FALSE, na="")

# Dependencies
dependencies <- data.frame(ID = c("baseline","baseline"),
                           interface = c("SSA","CBO"),
                           version = c("2","2"),
                           vintage = c(ssa_vintage, cbo_vintage),
                           scenario = c("historical","baseline"))
write.csv(dependencies, file = paste0(gsub("/baseline/","/",out_path),"dependencies.csv"), row.names = FALSE, na="")
