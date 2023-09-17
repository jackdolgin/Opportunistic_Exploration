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
library(papaja)

# Collect Methods Info ----------------------------------------------------

pilot_num_recruited_baseline <- pilot_data_raw$subid %>% n_distinct() 

pilot_num_basline_usasble <- pilot_data$Sub_ID %>% n_distinct()

pilot_baseline_gender_breakdown <- here("Data", "Pilot_Task", "sub_info.csv") %>%
  read_csv() %>%
  right_join(pilot_data, by = c("subid" = "Sub_ID")) %>%
  count(subid, gender) %>%
  count(gender)

pilot_num_females <- pilot_baseline_gender_breakdown %>%
  filter(gender == "Female") %>%
  pull(n)

pilot_num_males <- pilot_baseline_gender_breakdown %>%
  filter(gender == "Male") %>%
  pull(n)

pilot_num_other_gender <- pilot_baseline_gender_breakdown %>%
  filter(!gender %in% c("Female", "Male")) %>%
  pull(n) %>%
  sum



expl_num_recruited <- subids_with_enough_trials %>%
  filter(batch == "exploratory") %>%
  pull(subid) %>%
  n_distinct

rep_1_num_recruited <- subids_with_enough_trials %>%
  filter(batch == "replication") %>%
  pull(subid) %>%
  n_distinct

rep_2_num_recruited <- subids_with_enough_trials %>%
  filter(batch == "replication_2") %>%
  pull(subid) %>%
  n_distinct


usable_participants_by_exp <- free_choice_df %>%
  count(subid, batch) %>%
  count(batch)

expl_usable_participants <- usable_participants_by_exp %>%
  filter(batch == "exploratory") %>%
  pull(n)

rep_1_usable_participants <- usable_participants_by_exp %>%
  filter(batch == "replication") %>%
  pull(n)

rep_2_usable_participants <- usable_participants_by_exp %>%
  filter(batch == "replication_2") %>%
  pull(n)


gender_by_rep_exp <- free_choice_df %>%
  count(subid, batch) %>%
  left_join(read_csv(here("Data", "Main_Task", "Raw", "sub_info.csv"))) %>%
  count(batch, gender)

get_gender_count <- function(x, y){
  gender_by_rep_exp %>%
    filter(batch == x, gender == y) %>%
    pull(n)
}

gender_by_exp <- function(experiment){
  x_females <- get_gender_count(experiment, "Female")
  x_males <- get_gender_count(experiment, "Male")
  x_others <- gender_by_rep_exp %>%
    filter(batch == experiment) %>%
    pull(n) %>%
    sum %>%
    subtract(x_females) %>%
    subtract(x_males)
  return (list(x_females, x_males, x_others))
}

expl_gender <- gender_by_exp("exploratory")
rep_gender <- gender_by_exp("replication")
rep_2_gender <- gender_by_exp("replication_2")


innacurate_bandit_pcpts <- preprocessed_df %>%
  filter(
    subid %in% subids_with_enough_trials$subid
  ) %>%
  count(batch, subid, passes_threshold = sub_avg_accuracy >= .8) %>%
  count(batch, passes_threshold)

innac_by_exp <- function(experiment){
  innacurate_bandit_pcpts %>%
    filter(batch == experiment, !passes_threshold) %>%
    pull(n)
}

expl_innac <- innac_by_exp("exploratory")
rep_innac <- innac_by_exp("replication")
rep_2_innac <- innac_by_exp("replication_2")


didnt_do_tasks_in_head_pcpts <- anti_join(
  preprocessed_df %>%
    filter(
      subid %in% subids_with_enough_trials$subid,
      sub_avg_accuracy >= .8
    ) %>%
    count(subid, batch),
  preprocessed_df %>%
    filter(
      subid %in% subids_with_enough_trials$subid,
      q1 == "Not at all",
      q2 == "Not at all",
      q3 == "Always",
      sub_avg_accuracy >= .8
    )
) %>%
  count(batch)

didnt_in_head_by_exp <- function(experiment){
  didnt_do_tasks_in_head_pcpts %>%
    filter(batch == experiment) %>%
    pull(n)
}


too_many_responses_in_same_bandit <- first_layer_of_filtering %>%
  filter(! subid %in% potential_cheaters) %>%
  group_by(subid) %>%
  mutate(perc_mult = mean(selection == "mult")) %>%
  filter(
    !between(perc_mult, .15, .85)
  ) %>%
  ungroup() %>%
  count(batch, subid) %>%
  count(batch)

too_many_resp_by_exp <- function(experiment){
  too_many_responses_in_same_bandit %>%
    filter(batch == experiment) %>%
    pull(n)
}


num_baseline_pairs <- pilot_data_summarized %>% count(Low_Num, High_Num) %>% nrow


task_inaccuracy <- free_choice_df %>%
  filter(batch != "exploratory") %>%
  count(accuracy) %>%
  mutate(percent = n / sum(n)) %>%
  filter(accuracy == 0) %>%
  pull(percent) %>%
  percent(.1)




potential_cheaters_removed <- first_layer_of_filtering %>%
  group_by(subid) %>%
  mutate(perc_mult = mean(selection == "mult")) %>%
  filter(
    between(perc_mult, .15, .85),
    game_max_choice_time_so_far < 180 | batch %in% c("exploratory", "replication")
  ) %>%
  pull(subid) %>%
  unique %>%
  intersect(potential_cheaters) %>%
  length

perc_mult_chosen <- free_choice_df %>%
  filter(grepl("replication", batch)) %>%
  filter(accuracy == 1) %>%
  count(selection) %>%
  mutate(perc = n / sum(n)) %>%
  filter(selection == "mult") %>%
  pull(perc) %>%
  percent(.1)







# The following function and variables generate the t-tests used in the
# data tables

compare_paired_fits <- function(pair_compairison, coef_of_interest, keep_batch,
                                keep_separate_by_exposure,
                                keep_iv, keep_aggregation, keep_continuum,
                                group_1, group_2, group_1_exposure,
                                group_2_exposure){
  
  paired_fits <- load_paired_fits(
    keep_batch, keep_separate_by_exposure, keep_iv,
    keep_aggregation, keep_continuum, group_1, group_2, group_1_exposure,
    group_2_exposure
  ) %>%
    arrange(subid, free_choice_label)
  
  t.test(
    paired_fits %>%
      filter(free_choice_label == group_1) %>%
      pull(!!coef_of_interest),
    paired_fits %>%
      filter(free_choice_label == group_2) %>%
      pull(!!coef_of_interest),
    paired = T,
    alternative = pair_compairison
  )
}

expl_diff_sig_rt_perc <- compare_paired_fits(
  "greater", "difficulty_coef", "exploratory", F, "rt", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_diff_sig_rt_raw <- compare_paired_fits(
  "greater", "difficulty_coef", "exploratory", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_diff_sig_diff_perc <- compare_paired_fits(
  "greater", "difficulty_coef", "exploratory", F, "difficulty", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_diff_sig_diff_raw <- compare_paired_fits(
  "greater", "difficulty_coef", "exploratory", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_info_sig_rt_perc <- compare_paired_fits(
  "less", "information_coef", "exploratory", F, "rt", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_info_sig_rt_raw <- compare_paired_fits(
  "less", "information_coef", "exploratory", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_info_sig_diff_perc <- compare_paired_fits(
  "less", "information_coef", "exploratory", F, "difficulty", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_info_sig_diff_raw <- compare_paired_fits(
  "less", "information_coef", "exploratory", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_noise_sig_rt_perc <- compare_paired_fits(
  "less", "temp_coef", "exploratory", F, "rt", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_noise_sig_rt_raw <- compare_paired_fits(
  "less", "temp_coef", "exploratory", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_noise_sig_diff_perc <- compare_paired_fits(
  "less", "temp_coef", "exploratory", F, "difficulty", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_noise_sig_diff_raw <- compare_paired_fits(
  "less", "temp_coef", "exploratory", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_side_sig_rt_perc <- compare_paired_fits(
  "less", "side_coef", "exploratory", F, "rt", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_side_sig_rt_raw <- compare_paired_fits(
  "less", "side_coef", "exploratory", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_side_sig_diff_perc <- compare_paired_fits(
  "less", "side_coef", "exploratory", F, "difficulty", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

expl_side_sig_diff_raw <- compare_paired_fits(
  "less", "side_coef", "exploratory", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_diff_sig_rt_perc <- compare_paired_fits(
  "greater", "difficulty_coef", "replication", F, "rt", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_diff_sig_rt_raw <- compare_paired_fits(
  "greater", "difficulty_coef", "replication", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_diff_sig_diff_perc <- compare_paired_fits(
  "greater", "difficulty_coef", "replication", F, "difficulty", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_diff_sig_diff_raw <- compare_paired_fits(
  "greater", "difficulty_coef", "replication", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_info_sig_rt_perc <- compare_paired_fits(
  "less", "information_coef", "replication", F, "rt", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_info_sig_rt_raw <- compare_paired_fits(
  "less", "information_coef", "replication", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_info_sig_diff_perc <- compare_paired_fits(
  "less", "information_coef", "replication", F, "difficulty", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_info_sig_diff_raw <- compare_paired_fits(
  "less", "information_coef", "replication", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_noise_sig_rt_perc <- compare_paired_fits(
  "less", "temp_coef", "replication", F, "rt", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_noise_sig_rt_raw <- compare_paired_fits(
  "less", "temp_coef", "replication", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_noise_sig_diff_perc <- compare_paired_fits(
  "less", "temp_coef", "replication", F, "difficulty", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_noise_sig_diff_raw <- compare_paired_fits(
  "less", "temp_coef", "replication", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_side_sig_rt_perc <- compare_paired_fits(
  "less", "side_coef", "replication", F, "rt", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_side_sig_rt_raw <- compare_paired_fits(
  "less", "side_coef", "replication", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_side_sig_diff_perc <- compare_paired_fits(
  "less", "side_coef", "replication", F, "difficulty", "percentile",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print

rep_1_side_sig_diff_raw <- compare_paired_fits(
  "less", "side_coef", "replication", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
) %>% apa_print



# The following function and variables generate the Wilcoxon tests used in the
# data tables

compare_paired_fits <- function(pair_compairison, coef_of_interest, keep_batch,
                                keep_separate_by_exposure,
                                keep_iv, keep_aggregation, keep_continuum,
                                group_1, group_2, group_1_exposure,
                                group_2_exposure){
  
  paired_fits <- load_paired_fits(
    keep_batch, keep_separate_by_exposure, keep_iv,
    keep_aggregation, keep_continuum, group_1, group_2, group_1_exposure,
    group_2_exposure
  ) %>%
    arrange(subid, free_choice_label)
  
  # paired_fits
  coin::wilcoxsign_test(
    # coin::wilcoxsign_test(
    paired_fits %>%
      filter(free_choice_label == group_1) %>%
      pull(!!coef_of_interest) ~
      paired_fits %>%
      filter(free_choice_label == group_2) %>%
      pull(!!coef_of_interest),
    # paired = T,
    alternative = pair_compairison
  )
}


rep_2_diff_sig_rt_perc <- compare_paired_fits(
  "greater", "difficulty_coef", "replication_2", F, "rt", "percent",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_diff_sig_rt_raw <- compare_paired_fits(
  "greater", "difficulty_coef", "replication_2", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_diff_sig_diff_perc <- compare_paired_fits(
  "greater", "difficulty_coef", "replication_2", F, "difficulty", "percent",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_diff_sig_diff_raw <- compare_paired_fits(
  "greater", "difficulty_coef", "replication_2", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_info_sig_rt_perc <- compare_paired_fits(
  "less", "information_coef", "replication_2", F, "rt", "percent",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_info_sig_rt_raw <- compare_paired_fits(
  "less", "information_coef", "replication_2", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_info_sig_diff_perc <- compare_paired_fits(
  "less", "information_coef", "replication_2", F, "difficulty", "percent",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_info_sig_diff_raw <- compare_paired_fits(
  "less", "information_coef", "replication_2", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_noise_sig_rt_perc <- compare_paired_fits(
  "less", "temp_coef", "replication_2", F, "rt", "percent",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_noise_sig_rt_raw <- compare_paired_fits(
  "less", "temp_coef", "replication_2", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_noise_sig_diff_perc <- compare_paired_fits(
  "less", "temp_coef", "replication_2", F, "difficulty", "percent",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_noise_sig_diff_raw <- compare_paired_fits(
  "less", "temp_coef", "replication_2", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_side_sig_diff_raw <- compare_paired_fits(
  "less", "side_coef", "replication_2", F, "rt", "percent",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_side_sig_diff_raw <- compare_paired_fits(
  "less", "side_coef", "replication_2", F, "rt", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_side_sig_diff_raw <- compare_paired_fits(
  "less", "side_coef", "replication_2", F, "difficulty", "percent",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)

rep_2_side_sig_diff_raw <- compare_paired_fits(
  "less", "side_coef", "replication_2", F, "difficulty", "raw",
  "continuous", "Short horizon, free choice 1", "Long horizon, free choice 1",
  F, F
)




effect_of_math_type <- pilot_data_filtered_and_summarized %>%
  lm(Difficulty ~ Task, .) %>%
  report::report_text()


effect_of_rew <- free_choice_df %>%
  filter(grepl("replication", batch)) %>%
  filter(accuracy == 1) %>%
  mutate(point_diff = ifelse(selection == "mult",
                             mult_rew_exposed_cum_mean - add_rew_exposed_cum_mean,
                             add_rew_exposed_cum_mean - mult_rew_exposed_cum_mean)) %>%
  select(selection, mult_rew_exposed_cum_mean, add_rew_exposed_cum_mean, point_diff) %>%
  mutate(point_diff = standardise(point_diff)) %>%
  mutate(selection = if_else(selection == "mult", 1, 0)) %>%
  glm(selection ~ point_diff, family = "binomial", data = .) %>%
  # apa_print() %>%
  # pluck('full_result')
  broom::tidy() %>%
  filter(row_number() == 2) %>%
  mutate(lower_estimate = estimate - 1.96*std.error) %>%
  mutate(upper_estimate = estimate + 1.96*std.error) %>%
  mutate(across(c(estimate, lower_estimate, upper_estimate), exp))

effect_of_diff <- free_choice_df %>%
  filter(grepl("replication", batch)) %>%
  filter(accuracy == 1) %>%
  mutate(selection = if_else(selection == "mult", 1, 0)) %>%
  # mutate(difficulty_diff = RoundTo(rt_diff, 2)) %>%
  # group_by(rt_diff) %>%
  # summarise(selection = mean(selection)) %>%
  # ggplot(aes(rt_diff, selection)) +
  # geom_point() +
  # geom_smooth()
  mutate(rt_diff = standardise(rt_diff)) %>%
  glm(selection ~ rt_diff, family = "binomial", data = .) %>%
  # apa_print() %>%
  # pluck('full_result')
  broom::tidy() %>%
  filter(row_number() == 2) %>%
  mutate(lower_estimate = estimate - 1.96*std.error) %>%
  mutate(upper_estimate = estimate + 1.96*std.error) %>%
  mutate(across(c(estimate, lower_estimate, upper_estimate), exp))



knitr::write_bib(
  c(
    "base",
    "tidyverse",
    "arrow",
    "cowplot",
    "patchwork",
    "gghalves",
    "RcppDE"
  ))


correlation_RT_Diff <- cor.test(pilot_data_filtered_and_summarized$RT,
                                pilot_data_filtered_and_summarized$Difficulty)

correlation_Acc_Diff <- cor.test(pilot_data_filtered_and_summarized$Accuracy,
                                 pilot_data_filtered_and_summarized$Difficulty)

correlation_between_diff_and_rt <- correlation_RT_Diff %>%
  pluck(4) %>%
  round(2) %>%
  as.numeric


all_number_combos <- pilot_data_filtered_and_summarized %>%
  group_by(Low_Num, High_Num) %>%
  mutate(across(
    c(Accuracy, RT, Difficulty), ~ lead(.) - ., .names = "{col}_diff"
  )) %>%
  fill(ends_with("diff")) %>%
  ungroup() %>%
  mutate(across(Task, ~if_else(. == "Addition", "add", "mult"))) %>%
  pivot_wider(
    names_from = Task,
    names_glue = "{Task}_{.value}",
    values_from = c(Accuracy, RT, Difficulty)
  ) %>%
  arrange(-RT_diff)

biggest_combo_diff <- slice_max(all_number_combos, RT_diff)

smallest_combo_diff <- slice_min(all_number_combos, RT_diff)
