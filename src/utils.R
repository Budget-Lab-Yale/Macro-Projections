#--------------------------------
# utils.R
#
# Miscellaneous helper functions
#--------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
})

#' Load API keys into the chosen environment
#'
#' @param keys_csv Character path to a CSV file with columns `api` and `key`.
#' @param keep Optional character vector limiting which APIs are loaded.
#' @param envir Environment where keys should be assigned. Defaults to `.GlobalEnv`.
#' @return Invisibly returns a tibble of the loaded keys.
parse_api_keys <- function(keys_csv, keep = NULL, envir = .GlobalEnv) {
  keys <- read_csv(keys_csv, show_col_types = FALSE) %>%
    mutate(api = trimws(api), key = trimws(key))

  if (!is.null(keep)) {
    keys <- filter(keys, api %in% keep)
  }

  walk2(keys$api, keys$key, ~ assign(paste0(.x, '_key'), .y, envir = envir))
  invisible(keys)
}

#' Apply multiplicative growth rates to extend a time series
#'
#' @param values Numeric vector containing the original series, including any
#'   missing values that should be projected forward.
#' @param growth Numeric vector of the same length containing multiplicative
#'   growth factors for each period (e.g., 1.02 for 2% growth).
#' @return Numeric vector with missing values forward-filled using the supplied
#'   growth rates.
propagate_growth <- function(values, growth) {
  stopifnot(length(values) == length(growth))

  accumulate(
    .x = seq_along(values),
    .f = function(previous, idx) {
      if (idx == 1) {
        return(values[[1]])
      }

      if (!is.na(values[[idx]])) {
        values[[idx]]
      } else if (!is.na(previous) && !is.na(growth[[idx]])) {
        previous * growth[[idx]]
      } else {
        NA_real_
      }
    },
    .init = NA_real_
  )[-1]
}

#' Apply additive differences to extend a time series
#'
#' @param values Numeric vector for the baseline series.
#' @param differences Numeric vector of period-over-period additive changes.
#' @return Numeric vector with missing values filled using additive differences.
propagate_difference <- function(values, differences) {
  stopifnot(length(values) == length(differences))

  accumulate(
    .x = seq_along(values),
    .f = function(previous, idx) {
      if (idx == 1) {
        return(values[[1]])
      }

      if (!is.na(values[[idx]])) {
        values[[idx]]
      } else if (!is.na(previous) && !is.na(differences[[idx]])) {
        previous + differences[[idx]]
      } else {
        NA_real_
      }
    },
    .init = NA_real_
  )[-1]
}

#' Load and attach all packages listed in a requirements file.
#' 
#' @param requirements_file Path to the plain-text file with one package per line.
load_required_packages <- function(requirements_file) {
  read_lines(requirements_file) %>%
    discard(~ .x == '') %>%
    walk(~ library(.x, character.only = TRUE))
  invisible(TRUE)
}
