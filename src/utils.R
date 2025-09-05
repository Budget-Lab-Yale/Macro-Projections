#--------------------------------
# utils.R 
#
# Miscellaneous helper functions
#--------------------------------

parse_api_keys <- function(
    keys_csv,
    keep = NA) {

  #----------------------------------------------------------------------------
  # Read in and store API keys as global environment values
  # 
  # Parameters:
  #   - keys_csv (str) : CSV file containing API keys in two columns, 'api' (name of API) and 'key'
  #   - keep (chr) : list of API keys to return (default is to return all)
  #
  # Returns: Global environment value(s) [API name]_key
  #----------------------------------------------------------------------------
  
  keys <- read.csv(keys_csv) 
  for (keynum in seq(1,nrow(keys))) {
    if (any(is.na(keep), (!is.na(keep) & any(keys[keynum,'api'] %in% keep)))) {
      assign(paste0(keys[keynum,'api'],'_key'),keys[keynum,'key'], envir = .GlobalEnv)
    }
  }
}

#----------------------------------------------------------------------------
# Create and assign data source paths
#
# Parameters:
#   - sources (chr): names of data sources
#   - vintages (chr): corresponding vintage identifiers
#   - data_version (chr): version string for data files
#   - base_dir (chr): root directory for raw data
#   - type (chr): subdirectory indicating scenario (e.g., 'historical' or 'baseline')
#
# Returns: Named character vector of paths; each path also assigned to the
#          global environment as [source]_path
#----------------------------------------------------------------------------
set_source_paths <- function(sources, vintages, data_version, base_dir, type) {
  paths <- file.path(base_dir, gsub('_', '-', sources), data_version, vintages, type)
  names(paths) <- paste0(sources, '_path')
  list2env(as.list(paths), envir = .GlobalEnv)
  paths
}
