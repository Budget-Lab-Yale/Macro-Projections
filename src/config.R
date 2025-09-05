#------------------------------
# config.R
#------------------------------

# Central parameter definitions for the Macro-Projections project

# Year ranges
firstyr_hist <- 1970
firstyr_proj <- 2025
firstyr_ltbo <- firstyr_proj + 11
lastyr_ltbo <- 2055
lastyr_proj <- 2098

# Data version and vintages
data_version <- 'v3'
CBO_Budget_Hist_vintage <- '20250130'
CBO_Budget_Proj_vintage <- '20250117'
CBO_Econ_Hist_vintage   <- '20250117'
CBO_Econ_Proj_vintage   <- '20250117'
CBO_Demographic_vintage <- '20250113'
CBO_LTBO_vintage        <- '20250327'
SSA_Demographic_vintage <- '20240506'
SSA_AWI_vintage         <- '20241010'

# Output vintage
out_vintage <- '2025040115'

# Data sources
sources_hist <- c('SSA_AWI', 'SSA_Demographic', 'CBO_Budget_Hist', 'CBO_Econ_Hist')
sources_proj <- c('CBO_Budget_Proj', 'CBO_Demographic', 'CBO_Econ_Proj', 'CBO_LTBO')

# Base directories
base_data_dir <- '/gpfs/gibbs/project/sarin/shared/raw_data'
output_root   <- '/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections'
