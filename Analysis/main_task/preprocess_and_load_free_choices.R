source(here("Analysis", "pilot_task", "prep_pilot_analysis.R"))

no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

# this file is too large to be placed on Github. It can be found at https://osf.io/zvfky
free_choice_df_path <<- here("Data", "Main_Task", "free_choices.csv")

preprocessed_df_path <- here("Data", "Main_Task", "preprocessed_df.parquet")

if (regenerate_preprocessed_df){
  
  pilot_data_filtered_and_summarized %>%
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
    clean_names() %>%
    right_join(
      read_csv(here("Data", "Main_Task", "Raw", "experimental_trials.csv")),
      by = c("low_num" = "stim_1", "high_num" = "stim_2")
    ) %>%
    group_by(across(c(forced_choice, low_num, high_num, ends_with("diff")))) %>%
    nest %>%
    group_by(forced_choice) %>%
    mutate(across(ends_with("diff"), percent_rank, .names = "{col}_perc")) %>%
    ungroup() %>%
    unnest(data) %>%
    rename(selection = type) %>%
    arrange(subid, game_nr, trial_nr) %>%
    mutate(
      across(horizon, ~ . - 4),
      horizon_type = if_else(horizon == 1L, "Short", "Long"),
      across(choice_time, ~ . / 1000),                                            # Convert responses from ms to seconds
      across(accuracy, ~ifelse(forced_choice == 0, ., NA_integer_)),            # If trial is a free choice, keep accuracy as is; otherwise, accuracy should be NA
      across(ends_with("rew"), list(exposed = ~ ifelse(                         # If the name of the column at hand (considering only the `mult_rew` and `add_rew` columns) equals the glue...
        cur_column() == glue("{selection}_rew"), ., NA_integer_)))              # ...of the selection and the suffix 'rew', copy that col into a new col; otherwise new col gets NA
    ) %>%
    rename_with(~glue("{.x}_latent"), ends_with("rew")) %>%                     # Rename `mult_rew` and `add_rew` to further distinguish them from `..._exposed` columns
    group_by(subid, game_nr) %>%
    mutate(
      free_choice_trial = row_number() - 4,
      free_choice_label = glue(
        "{horizon_type} horizon, free choice {free_choice_trial}"
      ),
      game_trials_afterwards = n() - row_number(),
      game_avg_accuracy = mean(accuracy, na.rm = TRUE)
    ) %>%
    group_by(subid, game_nr, isna = is.na(accuracy)) %>%
    mutate(
      game_avg_accuracy_so_far = cummean(accuracy),
      game_max_choice_time_so_far = cummax(choice_time)
    ) %>%
    group_by(subid) %>%
    select(-isna) %>%
    mutate(
      sub_avg_accuracy = mean(accuracy, na.rm = TRUE),
      overall_trial = row_number()
    ) %>%
    group_by(subid, game_nr, selection) %>%
    mutate(
      across(ends_with("exposed"),                                              # From what the rewards that participants have seen so far...
             list(                                                              # ...(e.g., `mult_rew_exposed` and `add_rew_exposed), the...
               cum_mean = cummean,                                              # ...mean of choosing that side
               cum_count = seq_along,                                           # ...number of times they've chosen that side
               cum_sd = ~ rollapplyr(., 10, sd, partial = TRUE)                 # ...standard deviation of the rewards on that side
             )),
      across(ends_with("count"),                                                # If the side wasn't chosen, its count column becomes NA otherwise count...
             ~ if_else(str_detect(cur_column(), selection), ., NA_integer_)     # ...will represent the count of whatever was the  chosen side on that trial
      ),
      map2_dfr(side, selection, function(side, selection){                      # Indicate which side multiplication is for that trial and which side...
        map_dfc(c("mult", "add"), ~tibble(                                      # ...addition is; if no response was given, then both mult_side and add_side...
          "{.x}_side" := ifelse(                                                # ...get assigned to "-1"
            selection %in% c(.x, "-1"), side, setdiff(c("right", "left"), side)
          ),
          "{.x}_rand" := runif(1)                                               # Assign a random value to the mult side and a random value for the add sign
        ))
      }),
      across(ends_with("_side"), ~case_when(
        . == "-1" ~ -1L, . == "left" ~ 0L, . == "right" ~ 1L                    # Convert `mult_side` and `add_side` from character vector to -1, 0, or 1
      ))
    ) %>%
    group_by(subid) %>%
    mutate(across(ends_with("_side"), ~if_else(. == -1L, Mode(.), .))) %>%
    group_by(subid, game_nr) %>%
    fill(contains("cum")) %>%                                                   # Fills the rows that were NA with the correct count for that side
    mutate(across(contains("cum"), lag)) %>%                                    # Changes cumulative columns so they reflect what participants knew at start, not end, of that trial
    group_by(subid, game_nr, forced_choice) %>%
    mutate(across(
      ends_with("cum_count"), min, .names = "{.col}_at_first_fc"                # Creates column for `mult` and `add`...
    )) %>%                                                                      # ... that is equal to what...
    group_by(subid, game_nr) %>%                                                # ...`cum_count` is during that...
    mutate(across(ends_with("at_first_fc"), ~max(., na.rm = TRUE))) %>%         # ...game's first free choice trial
    ungroup() %>%
    mutate(equal_information_at_first_fc = if_else(
      near(
        mult_rew_exposed_cum_count_at_first_fc,
        add_rew_exposed_cum_count_at_first_fc
      ),
      T, F
    )) %>%
    mutate(future_pmap_dfr(., function(...){                                    # Determines, for several reference points, whether mult or add or neither is greater
      row_df <- data.frame(...)
      row_list <- list(...)
      
      xmap_dfc(
        list(
          c(
            "side", "rand", "rew_latent",
            map_chr(c("mean", "count", "sd"), ~ glue("rew_exposed_cum_{.x}"))
          ),
          c(
            "bigger",
            "smaller"
          )
        ),
        function(x, y){
          
          mult_val <- pluck(row_list, glue("mult_{x}"))
          add_val <- pluck(row_list, glue("add_{x}"))
          
          val1 <- if_else(y == "bigger", "mult", "add")
          val2 <- if_else(y == "bigger", "add", "mult")
          
          reference_col_df <- row_df %>%
            transmute("{{y}}_{{x}}" := case_when(
              mult_val > add_val ~ val1,
              mult_val < add_val ~ val2,
              mult_val == add_val ~ "tie",
              TRUE ~ NA_character_
            )) %>%
            clean_names()
          
          row_df %>%
            select(
              reference_col_df %>%
                pull %>%
                starts_with
            ) %>%
            rename_with(~str_replace(
              .x,
              "^[:alpha:]+(?=_)",
              glue("{y}_{x}")
            )) %>%
            bind_cols(reference_col_df)
        }
      )
    })) %>%
    left_join(
      read_csv(here("Data", "Main_Task", "Raw", "sub_info.csv")) %>%
        select(-ends_with("side")),
      by = "subid",
      suffix = c("_main_exp", "_demographics_form")
    ) %>%
    write_parquet(preprocessed_df_path)
  
}

preprocessed_df <- read_parquet(preprocessed_df_path)

subids_with_enough_trials <- preprocessed_df %>%
  group_by(batch) %>%
  count(subid) %>%
  slice_max(n)

first_layer_of_filtering <- preprocessed_df %>%
  filter(
    subid %in% subids_with_enough_trials$subid,
    q1 == "Not at all",
    q2 == "Not at all",
    q3 == "Always",
    sub_avg_accuracy >= .8,
    forced_choice == 0
  )

potential_cheaters <- first_layer_of_filtering %>%
  group_by(subid) %>%
  filter(
    batch == "replication_2",
    selection == "mult",
    rt_diff > 12
  ) %>%
  summarise(median_choice_time = median(choice_time), num_diff_choices = n()) %>%
  filter(median_choice_time < 10000, num_diff_choices >= 15) %>%
  pull(subid)

if (regenerate_preprocessed_df){
  
  first_layer_of_filtering %>%
    filter(! subid %in% potential_cheaters) %>%
    group_by(subid) %>%
    mutate(perc_mult = mean(selection == "mult")) %>%
    filter(
      between(perc_mult, .15, .85),
      game_max_choice_time_so_far < 180 | batch %in% c("exploratory", "replication")
    ) %>%
    ungroup() %>%
    mutate(across(
      c(horizon, horizon_type, free_choice_label, forced_choice, selection,
        equal_information_at_first_fc),
      as_factor)) %>%
    write_csv(free_choice_df_path)
}


free_choice_df <<- read_csv(free_choice_df_path)

