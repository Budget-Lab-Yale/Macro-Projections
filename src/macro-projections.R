#------------------------------
# macro-projections.R
#------------------------------

# Master script orchestrating the macro projection workflow.

source('src/config.R')
source('src/setup.R')
source('src/economic.R')
source('src/budget.R')
source('src/demographic.R')

settings <- initialize()

econ <- build_economic_data()
budget <- build_budget_data(econ$proj)
demo <- build_demographic_data()

historical <- econ$hist %>% left_join(budget$hist, by = 'year') %>%
                              left_join(demo$hist, by = 'year')
write.csv(historical, file = file.path(settings$out_path, 'historical.csv'),
          row.names = FALSE, na = '')

projections <- econ$proj %>% left_join(budget$proj, by = 'year') %>%
                              left_join(demo$proj, by = 'year')
write.csv(projections, file = file.path(settings$out_path, 'projections.csv'),
          row.names = FALSE, na = '')

dependencies <- data.frame(
  ID = c(replicate((length(sources_hist)+length(sources_proj)),'baseline')),
  interface = c(sources_hist, sources_proj),
  version = c(replicate((length(sources_hist)+length(sources_proj)), gsub('v','',data_version))),
  vintage = c(settings$sources_hist_vintages, settings$sources_proj_vintages),
  scenario = c(replicate(length(sources_hist),'historical'), replicate(length(sources_proj),'baseline'))
)
write.csv(dependencies, file = file.path(dirname(settings$out_path), 'dependencies.csv'),
          row.names = FALSE, na = '')
