#--------------------------------
# data_paths.R
#
# Helper functions for building input and output paths
#--------------------------------

library(dplyr)
library(stringr)

create_source_paths <- function(settings) {
  settings$sources %>%
    mutate(
      vintage = settings$vintages[interface],
      base_path = file.path(
        '/gpfs/gibbs/project/sarin/shared/raw_data',
        str_replace_all(interface, '_', '-'),
        settings$data_version,
        vintage
      ),
      path = file.path(base_path, scenario, '')
    )
}

ensure_output_paths <- function(settings) {
  base_dir <- file.path(settings$output$base_dir, settings$data_version)
  version_dir <- file.path(base_dir, settings$output$vintage)
  scenario_dir <- file.path(version_dir, settings$output$scenario)

  dir.create(version_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(scenario_dir, showWarnings = FALSE, recursive = TRUE)

  list(
    base_dir = base_dir,
    version_dir = version_dir,
    scenario_dir = scenario_dir,
    historical_file = file.path(scenario_dir, 'historical.csv'),
    projections_file = file.path(scenario_dir, 'projections.csv'),
    dependencies_file = file.path(version_dir, 'dependencies.csv')
  )
}

build_dependency_table <- function(source_paths, settings) {
  source_paths %>%
    transmute(
      ID = settings$output$scenario,
      interface,
      version = str_remove(settings$data_version, 'v'),
      vintage,
      scenario
    )
}
