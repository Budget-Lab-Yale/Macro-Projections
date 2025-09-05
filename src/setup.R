#------------------------------
# setup.R
#------------------------------

#' Initialize project environment
#'
#' Loads packages, sources utilities, sets API keys, constructs data paths,
#' and creates the output directory.
#'
#' @return list with elements:
#'   - out_path: destination for generated CSV files
#'   - sources_hist_vintages: vintage tags for historical sources
#'   - sources_proj_vintages: vintage tags for projection sources
initialize <- function() {
  setwd('~/project/repositories/Macro-Projections')

  lapply(readLines('requirements.txt'), library, character.only = TRUE)

  source('src/utils.R')

  parse_api_keys('~/project/api_keys.csv', keep = c('fred'))
  fredr_set_key(fred_key)

  sources_hist_vintages <- map_chr(sources_hist, ~ get(paste0(.x, '_vintage')))
  sources_proj_vintages <- map_chr(sources_proj, ~ get(paste0(.x, '_vintage')))

  set_source_paths(sources_hist, sources_hist_vintages, data_version, base_data_dir, 'historical')
  set_source_paths(sources_proj, sources_proj_vintages, data_version, base_data_dir, 'baseline')

  out_path <- file.path(output_root, data_version, out_vintage, 'baseline')
  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

  list(
    out_path = out_path,
    sources_hist_vintages = sources_hist_vintages,
    sources_proj_vintages = sources_proj_vintages
  )
}
