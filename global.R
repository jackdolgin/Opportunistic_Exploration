library(shiny)
library(shinyWidgets)

purrr::walk(
  c(
    "packages", "analysis_params", "preprocess_and_load_free_choices", "fit",
    "plotting_funcs"
  ),
  ~source(here::here("Analysis", "main_task", paste0(.x, ".R")), T)
)


shinyOptions(.guitheme = "meta-explore-shiny-root.css")