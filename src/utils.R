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