regenerate_preprocessed_df <<- FALSE # takes a long time to run
rerun_fit_models <<- "none"  # "none", "local", or "cluster"; takes a monumental amount of time to run
too_much_math_preference <<- 0.85
min_pilot_math_acc <<- 80
min_bandit_math_acc <<- 80
longest_time_for_baseline_math <<- 75
cheaters_detection_vars <<- c(15, 12, 10)
std_scale_of_diff <<- c(-1, 1)
exponential_prior_length <<- 20
gaus_prior <<- c(0, 20)
global_optimizer_lower_bound <<- -100
global_optimizer_upper_bound <<- 100
max_iterations_per_fit <<- 1000
medium <- "manuscript" # "manuscript" or "shiny"
ggplot_font <- "Fira Sans"