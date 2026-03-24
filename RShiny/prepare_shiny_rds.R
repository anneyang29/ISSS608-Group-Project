data_bundle <- readRDS("RShiny/data/shiny_base_data.rds")

saveRDS(
  list(base_data = data_bundle$base_data),
  "RShiny/data/shiny_base_data_light.rds"
)

saveRDS(
  list(time_data = data_bundle$time_data),
  "RShiny/data/shiny_time_data.rds"
)

cat("Done: created shiny_base_data_light.rds and shiny_time_data.rds\n")
