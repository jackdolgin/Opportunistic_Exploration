# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")

purrr::walk(
  c(
    "packages", "analysis_params", "preprocess_and_load_free_choices", "fit",
    "plotting_funcs"
  ),
  ~source(here::here("Analysis", "main_task", paste0(.x, ".R")))
)

if (medium == "manuscript"){
  source(here::here("Manuscript", "load_manuscript_variables.R"))
}