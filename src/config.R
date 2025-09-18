#--------------------------------
# config.R
#
# Centralised configuration for macro projections
#--------------------------------

library(tibble)

get_project_settings <- function() {
  first_proj <- 2025
  list(
    years = list(
      first_hist = 1970,
      first_proj = first_proj,
      first_ltbo = first_proj + 11,
      last_ltbo = 2055,
      last_proj = 2098
    ),
    data_version = 'v3',
    vintages = c(
      CBO_Budget_Hist = '20250130',
      CBO_Budget_Proj = '20250117',
      CBO_Econ_Hist = '20250117',
      CBO_Econ_Proj = '20250117',
      CBO_Demographic = '20250113',
      CBO_LTBO = '20250327',
      SSA_Demographic = '20240506',
      SSA_AWI = '20241010'
    ),
    output = list(
      vintage = '2025040115',
      base_dir = '/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections',
      scenario = 'baseline'
    ),
    sources = tibble::tibble(
      interface = c(
        'SSA_AWI',
        'SSA_Demographic',
        'CBO_Budget_Hist',
        'CBO_Econ_Hist',
        'CBO_Budget_Proj',
        'CBO_Demographic',
        'CBO_Econ_Proj',
        'CBO_LTBO'
      ),
      scenario = c(
        rep('historical', 4),
        rep('baseline', 4)
      )
    ),
    requirements_file = 'requirements.txt',
    project_root = '~/project/repositories/Macro-Projections'
  )
}
