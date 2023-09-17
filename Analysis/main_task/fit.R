if (rerun_fit_models %in% c("local", "cluster")){
  
  model_combos_per_participant <- expand_grid(
    separate_by_exposure = c(T, F),
    iv = c("rt", "difficulty"),
    aggregation = c("raw", "percentile"),
    continuum = c("continuous", "ordinal")
  )
  
  if (rerun_fit_models == "local"){
    plan(multisession, workers = availableCores())
  } else if (rerun_fit_models == "cluster"){
    plan(
      batchtools_slurm,
      workers = nrow(model_combos_per_participant),
      resources=list(
        job_name = "run_on_cluster",
        log_file_output = "run_on_cluster_output.log",
        log_file_error = "run_on_cluster_error.log",
        ntasks = 1,
        nnodes = 1,
        ncpus = 1,
        memory = '7g',
        walltime = "96:00:00"),
      template = here("Analysis", "main_task", "template_slurm.tmpl")
    )
  }
  
  optimization_func <- function(par, data, eq_exposure){
    data %>%
      transmute(
        ifelse(eq_exposure, 0, par[4]) %>%
          multiply_by(information_diff) %>%
          add(rew_diff) %>%
          add(par[2] * iv_diff) %>%
          add(par[3]) %>%
          divide_by(sqrt(2) * par[1]) %>%
          exp %>%
          add(1) %>%
          raise_to_power(-1) %>%
          subtract((. * 2 - 1) * mult_chosen)
      ) %>%
      pull %>%
      c(
        if_else(eq_exposure, 1, dnorm(par[4], mean = gaus_prior[1], sd = gaus_prior[2])), # sets the if value to 1 since it gets logged at the next step, which stops it from having any effect
        dnorm(par[2], mean = gaus_prior[1], sd = gaus_prior[2]),
        dnorm(par[3], mean = gaus_prior[1], sd = gaus_prior[2])
      ) %>%
      log %>%
      c(-par[1] / exponential_prior_length) %>% # could move this before the log, use exp to wrap it, and set a bound on optim values so we never reach 709, which sends val to infinity
      multiply_by(-1) %>%
      sum
  }
  
  pmap_dfr(model_combos_per_participant, function(separate_by_exposure, iv,
                                                 aggregation, continuum){
      
      
      df <- free_choice_df %>%
        mutate(across(
          horizon_type, ~fct_relevel(., ~sort(., decreasing = T))
        )) %>%
        filter(game_avg_accuracy_so_far == 1L) %>%
        rowwise() %>%
        mutate(
          equal_exposure = ifelse(
            separate_by_exposure,
            add_rew_exposed_cum_count == mult_rew_exposed_cum_count,
            F
          ),
          iv_diff = !!sym(glue(
            "{iv}_diff{ifelse(aggregation == 'raw', '', '_perc')}"
          ))
        ) %>%
        ungroup()
      
      if (aggregation == "percentile"){
        df <- mutate(df, across(iv_diff, ~change_scale(., std_scale_of_diff)))  # Sets range from -1 to 1
      }
      
      if (continuum == "ordinal"){
        df <- mutate(df, across(
          iv_diff, ~ .x %>%
            dense_rank %>%
            multiply_by(7) %>%
            divide_by(max(dense_rank(.x))) %>%
            ceiling
        ))
      }
      
      df %>%
        group_by(
          batch, subid, horizon, horizon_type, free_choice_trial,
          free_choice_label, equal_exposure
        ) %>%
        transmute(
          iv_diff,
          rew_diff = mult_rew_exposed_cum_mean - add_rew_exposed_cum_mean,
          information_diff =
            add_rew_exposed_cum_count - mult_rew_exposed_cum_count,
          across(information_diff, ~case_when(
            . >= 1 ~ 1, . <= -1 ~ -1, T ~ 0
          )),
          mult_chosen = if_else(selection == 'mult', 1, 0),
          total_mult_chosen = sum(mult_chosen),
          total_trials = n()
        ) %>%
        group_by(across(-c(ends_with("diff"), "mult_chosen"))) %>%
        nest() %>%
        ungroup() %>%
        mutate(future_pmap_dfr(
          list(data, equal_exposure),
          function(my_data, my_equal_exposure){
            DEoptim(
              optimization_func,
              c(
                .0001,
                global_optimizer_lower_bound,
                global_optimizer_lower_bound,
                if(!my_equal_exposure) global_optimizer_lower_bound
              ),
              c(
                rep(global_optimizer_upper_bound, 3),
                if(!my_equal_exposure) global_optimizer_upper_bound
              ),
              DEoptim.control(itermax = max_iterations_per_fit, trace = FALSE),
              data = my_data,
              eq_exposure = my_equal_exposure
            ) %>%
              pluck('optim', 'bestmem') %>%
              map2_dfc(
                c("temp_coef", "difficulty_coef", "side_coef", if(!my_equal_exposure) "information_coef"),
                function(x, y) tibble("{y}" := x)
              ) %>%
              mutate(separate_by_exposure = separate_by_exposure, iv = iv,
                     aggregation = aggregation, continuum = continuum)
            
          },
          .options = furrr_options(seed = TRUE)
        )) %>%
        ungroup()
    }) %>%
    write_rds(here("Analysis", "main_task", "fits.rds"), "xz")
  
}




load_paired_fits <<- function(keep_batch, keep_separate_by_exposure, keep_iv,
                             keep_aggregation, keep_continuum, group_1, group_2,
                             group_1_exposure, group_2_exposure){
  here("Analysis", "main_task", "fits.rds") %>%
    read_rds() %>%
    filter(
      batch %in% keep_batch,
      separate_by_exposure == keep_separate_by_exposure,
      iv == keep_iv,
      aggregation == keep_aggregation,
      continuum == keep_continuum,
      (free_choice_label == group_1 & equal_exposure == group_1_exposure) |
        (free_choice_label == group_2 & equal_exposure == group_2_exposure)
    ) %>%
    group_by(subid) %>%
    filter(
      min(total_mult_chosen) >= 3,
      max(total_trials - total_mult_chosen) >= 3
    ) %>%
    ungroup() %>%
    mutate(across(c(free_choice_trial), as_factor))
}

