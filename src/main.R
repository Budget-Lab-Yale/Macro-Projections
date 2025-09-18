#--------------------------------
# main.R
#
# Orchestrates the macro projection pipeline
#--------------------------------

source('src/config.R')
source('src/utils.R')
source('src/data_paths.R')
source('src/economic_data.R')
source('src/budget_data.R')
source('src/demographic_data.R')

run_macro_projections <- function() {
  settings <- get_project_settings()

  setwd(normalizePath(settings$project_root, mustWork = FALSE))
  load_required_packages(settings$requirements_file)

  parse_api_keys('~/project/api_keys.csv', keep = c('fred'))
  fredr::fredr_set_key(fred_key)

  source_paths <- create_source_paths(settings)
  paths <- source_paths %>% dplyr::select(interface, path) %>% tibble::deframe()
  output_paths <- ensure_output_paths(settings)

  economic <- assemble_economic_data(paths, settings$years)
  budget <- assemble_budget_data(paths, settings$years, economic$projections)
  demographics <- assemble_demographic_data(paths, settings$years)

  historical <- economic$historical %>%
    dplyr::left_join(budget$historical, by = 'year') %>%
    dplyr::left_join(demographics$historical, by = 'year')

  projections <- economic$projections %>%
    dplyr::left_join(budget$projections, by = 'year') %>%
    dplyr::left_join(demographics$projections, by = 'year')

  readr::write_csv(historical, output_paths$historical_file, na = '')
  readr::write_csv(projections, output_paths$projections_file, na = '')

  dependencies <- build_dependency_table(source_paths, settings)
  readr::write_csv(dependencies, output_paths$dependencies_file, na = '')
}

if (identical(environment(), globalenv())) {
  run_macro_projections()
}
