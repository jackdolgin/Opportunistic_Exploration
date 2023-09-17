if (medium == "manuscript"){
  input <- list()
  input$batch_fits <- "replication"
  input$iv_fits <- "rt"
  input$aggregation_fits <- "raw"
  input$separate_by_exposure_fits <- FALSE
  input$continuum_fits <- "continuous"
  input$group_1_fits <- "Short horizon, free choice 1"
  input$group_2_fits <-"Long horizon, free choice 1"
  input$batch_choice_curve <- "replication"
  input$y_var_choice_curve <- "mult"
  input$x_raw_or_percent <- "percent"
  input$choice_combos_choice_curve <- c(
    "Long horizon, free choice 1",
    "Short horizon, free choice 1"
  )
  input$x_comparison_group <- "mult"
  input$x_comparison_dimension <- "rt"
  input$show_CI_choice_curve <- T
  input$batch_learning_curve <- "replication"
  input$y_var_learning_curve <- "mult"
  input$show_CI_learning_curve <- T
}

font_add_google(ggplot_font, ggplot_font)
showtext_auto()

shared_graph_theme_all_three <- list(
  theme(
    strip.text = element_text(size = 16, family = ggplot_font),
    axis.title.x = element_text(size = 18, family = ggplot_font),
    axis.title.y = element_text(size = 18, family = ggplot_font),
    axis.text.x = element_text(size = 13, family = ggplot_font),
    axis.text.y = element_text(size = 13, family = ggplot_font),
  )
)

choice_and_learning_plots <- function(plot_type){
  
  grab <- function(x) {
    
    if (x %in% c(
      "x_comparison_group",
      "x_comparison_dimension",
      "x_raw_or_percent"
    )){
      if (plot_type == "choice_curve" | x == "x_raw_or_percent"){
        pluck(input, x)
      } else {                                                                  # specifying some value here so we don't wind up with input
        "rand"                                                                  # of variance, which messes up learning curve graph
      }
    } else {
      pluck(input, glue("{x}_{plot_type}"))
    }
  }
  
  remove_incorrect_trials <<- if_else(grab("y_var") == "accuracy", F, T)
  iv_diff_col_grouping <- if (plot_type == "choice_curve"){
    "iv_diff"
  } else {
    "filler_group"
  }
  
  walk(
    list(
      list("rew_exposed_cum_sd", -12, 12, 1, "identity", "reward variability"),
      list("rew_exposed_cum_mean", -30, 30, 10, "identity", "expected reward"),
      list("rew_latent", -30, 30, 10, "identity", "underlying reward"),
      list("rt", -1, 10, 1, "reverse", "baseline RT"),
      list("difficulty", 0, 100, 10, "reverse", "baseline difficulty"),
      list("rand", 1, 7, 1, "identity", "random generated number")
    ),
    ~{
      if (.x[[1]] == grab("x_comparison_dimension")){
        if(grab("x_raw_or_percent") == "percent"){
          x_smallest <<- 1
          x_biggest <<- 7
          x_interval <<- 1
        } else{
          x_smallest <<- .x[[2]]
          x_biggest <<- .x[[3]]
          x_interval <<- .x[[4]]
        }
        x_direction <<- .x[[5]]
        x_differences_name <<- .x[[6]]
      }
    }
  )
  
  grab_comparison_group <- glue("{grab('x_comparison_group')}_")
  
  walk(
    list(
      list(
        "mult",
        "short description",
        c("mult", "multiplication", "add", "addition sides", "")
      ),
      list(
        "side",
        "short description",
        c(
          "bigger",
          "right",
          "smaller",
          "left",
          grab_comparison_group
        )
      ),
      list(
        "rt",
        "short description",
        c(
          "bigger",
          "harder",
          "smaller",
          "easier sides (baseline RT)",
          grab_comparison_group
        )
      ),
      list(
        "difficulty",
        "short description",
        c(
          "bigger",
          "harder",
          "smaller",
          "easier sides (baseline self-rated difficulty)",
          grab_comparison_group
        )
      ),
      list(
        "rand",
        "short description",
        c(
          "bigger",
          "one random side",
          "smaller",
          "another",
          grab_comparison_group
        )
      ),
      list(
        "rew_exposed_cum_sd",
        "long description",
        c(
          "bigger",
          "more  variability",
          "smaller",
          "less variability",
          grab_comparison_group
        )
      ),
      list(
        "rew_exposed_cum_mean",
        "long description",
        c(
          "bigger",
          "greater mean reward so far",
          "smaller",
          "lesser mean reward so far",
          grab_comparison_group
        )
      ),
      list(
        "rew_latent",
        "long description",
        c(
          "bigger",
          "greater underlying reward",
          "smaller",
          "lesser underlying reward",
          grab_comparison_group
        )
      )
    ),
    ~{
      if (.x[[1]] == grab("x_comparison_group")){
        
        get_group <- function(a_num){
          sym(glue(
            "{.x[[3]][{a_num}]}_{.x[[3]][5]}",
            "{grab('x_comparison_dimension')}"
          ))
        }
        
        first_group <<- get_group(1)
        second_group <<- get_group(3)
        
        x_axis_first_part <<- ifelse(
          .x[[2]] == "short description",
          glue(
            "Difference in {x_differences_name} between ",
            "{.x[[3]][2]} and {.x[[3]][4]}"
          ),
          glue(
            "Difference in {x_differences_name} between side with ",
            "{.x[[3]][2]} and side with {.x[[3]][4]}"
          )
        )
        
        x_axis_label <<- x_axis_first_part %>%
          paste0(ifelse(
            grab("x_raw_or_percent") == "percent",
            ", lumped into 7 bins (smaller difference -> smaller bin number)",
            ""
          )) %>%
          str_wrap(80)
      }
    }
  )
  
  walk(
    list(
      list(
        "mult",
        "identity",
        "Prev. forced choices to multiplication side:",
        "facet_col",
        sym("selection"),
        "Choosing Multiplication"
      ),
      list(
        "mult",
        "identity",
        "Prev. forced choices to multiplication side:",
        "facet_col",
        sym("selection"),
        "Choosing Multiplication"
      ),
      list(
        "accuracy",
        "sym",
        "No facets",
        "",
        1L,
        "Answering Math Correctly"
      ),
      list(
        "bigger_rand",
        "sym",
        "Prev. forced choices to side with now higher random value:",
        "facet_col",
        sym("selection"),
        "Choosing a Given Random Bandit"
      ),
      list(
        "bigger_side",
        "sym",
        "Prev. forced choices to right side:",
        "facet_col",
        sym("selection"),
        "Choosing Right Side Bandit"
      ),
      list(
        "bigger_rew_exposed_cum_mean",
        "sym",
        "Prev. forced choices to side with now higher expected value:",
        "facet_col",
        sym("selection"),
        "Choosing Bandit with Greater Expected Reward"
      ),
      list(
        "bigger_rew_exposed_cum_count",
        "sym",
        "No facets",
        "",
        sym("selection"),
        "Choosing Bandit with More Exposure"
      ),
      list(
        "bigger_rew_latent",
        "sym",
        "Prev. forced choices to side with now higher underlying value:",
        "facet_col",
        sym("selection"),
        "Choosing Bandit with Greater Underlying Reward"
      ),
      list(
        "bigger_rew_exposed_cum_sd",
        "sym",
        "Prev. forced choices to side with now greater SD:",
        "facet_col",
        sym("selection"),
        "Choosing Bandit with More Reward Variability"
      )
    ),
    ~{
      if (.x[[1]] == grab("y_var")){
        y_var_col <<- exec(.x[[2]], .x[[1]])
        facet_title <<- str_wrap(.x[[3]], 50)
        facet_col_grouping <<- .x[[4]]
        selection_or_accuracy <<- .x[[5]]
        y_axis_label <<- str_wrap(paste("Probability of", .x[[6]]), 80)
        plot_title_dv_part <<- .x[[6]]
        
      }
    }
  )
  
  
  if (medium == "shiny"){
    prep_graph_df <- filter(free_choice_df, batch == grab("batch"))
  } else if(medium == "manuscript") {
    prep_graph_df <- free_choice_df
  }
    
  prep_graph_df <- prep_graph_df %>%
    mutate(
      iv_diff = !!first_group %>%
        subtract(!!second_group) %>%
        exec(.fn = ifelse(
          grab("x_raw_or_percent") == "percent" |
            grab("x_comparison_dimension") == "rand",
          "percent_rank",
          "identity"
        ))
    ) %>%
    filter(
      game_avg_accuracy_so_far >= remove_incorrect_trials,
      !!y_var_col != "tie"
    ) %>%
    mutate(iv_diff = case_when(
      grab("x_raw_or_percent") == "percent" |
        grab("x_comparison_dimension") == "rand" ~
        RoundTo(
          iv_diff,
          FUN = floor,
          multiple = 1 %>%
            divide_by(x_biggest) %>%
            round(3)
        ) %>%
        multiply_by(x_biggest) %>%
        round %>%
        add(1),
      iv_diff < x_smallest ~ x_smallest,
      iv_diff > x_biggest ~ x_biggest,
      TRUE ~ RoundTo(iv_diff, x_interval)
    )) %>%
    mutate(
      batch = case_when(
        batch == "exploratory" ~ "Experiment 1",
        batch == "replication" ~ "Experiment 2",
        batch == "replication_2" ~ "Experiment 3"
      ),
      facet_col = case_when(
        medium == "shiny" ~
          paste(
            facet_title,
            !!ifelse(
              facet_col_grouping  == "",
              NA_character_,
              sym(glue("{grab('y_var')}_rew_exposed_cum_count_at_first_fc")
              )
            )
          ),
        medium == "manuscript" ~ batch
      ),
      across(facet_col, as_factor),
      across(
        free_choice_label,
        ~fct_reorder2(., game_trials_afterwards, horizon, .desc = TRUE)
      ),
      across(c(horizon_type, facet_col), ~fct_relevel(., sort)),
      prob_y = !!selection_or_accuracy == !!y_var_col
    ) %>%
    filter(!is.na(iv_diff)) %>% # do i ever need this line?
    mutate(filler_group = 0) %>%
    group_by(
      !!sym(iv_diff_col_grouping), free_choice_label, horizon_type,
      free_choice_trial, !!sym(facet_col_grouping), subid
    ) %>%
    summarise(across(prob_y, mean), .groups = "drop_last") %>%
    summarise(
      sem = sd(prob_y) / (sqrt(n())),
      across(prob_y, mean)
    ) %>%
    ungroup()
  
  
  shared_graph_features <- function(){
    
    layers_list <- list(
      geom_point(size = 4),
      geom_line(size=1.5),
      theme_bw(),
      removeGridX(),
      shared_graph_theme_all_three,
      theme(
        plot.title = element_text(
          size = 26, hjust = 0.5, margin = margin(t = 10, b = 10, l = 0, r = 0),
          family = ggplot_font
        ),
        legend.text = element_text(size = 16, family = ggplot_font),
        legend.title = element_text(size = 20, family = ggplot_font)
      )
    )
    
    if (grab("show_CI")) layers_list <- append(layers_list, geom_linerange(
      aes(ymin = prob_y - sem, ymax = prob_y + sem),
      alpha = .5,
      size = 2
    ))
    
    if(facet_col_grouping == "facet_col"){
      layers_list <- append(
        layers_list,
        facet_wrap(vars(facet_col))
      )
    }
    
    layers_list
  }
  
  if (plot_type == "choice_curve"){
    
    if (medium == "shiny"){
      plot_title <- glue(
        "Effect of {x_axis_first_part} on {plot_title_dv_part}"
      ) %>%
        tools::toTitleCase() %>%
        str_wrap(80)
      room_for_legend <- list()
    } else if (medium == "manuscript"){
      plot_title <- ""
      room_for_legend <- list(
        scale_y_continuous(breaks = seq(.2, .8, .1), limits = c(.15, .7)),
        theme(legend.position = c(0.46, 0.9))
      )
    }
    
    prep_graph_df %>%
      filter(free_choice_label %in% grab("choice_combos")) %>%
      ggplot(aes(
        iv_diff, prob_y,
        group = free_choice_label, colour = free_choice_label
      )) +
      theme(
        # legend.position = c(.5, .88),
        # legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(.2, 'lines')
      ) +
      scale_color_viridis_d(
        name = "Game Stages",
        option="plasma",
        end = .7,
        direction = -1
      ) +
      labs(title = plot_title, x = x_axis_label, y = y_axis_label) +
      scale_x_continuous(
        breaks = seq(x_smallest, x_biggest, x_interval),
        trans = x_direction
      ) +
      shared_graph_features() +
      room_for_legend
    
    
  } else if (plot_type == "learning_curve"){
    
    plot_title <- glue(
      "{y_axis_label} over the course of a trial"
    ) %>%
      tools::toTitleCase() %>%
      str_wrap(80)
    
    ggplot(prep_graph_df, aes(
      free_choice_trial, prob_y, group = horizon_type, colour = horizon_type
    )) +
      labs(
        title = plot_title,
        x = "Free-Choice Trial Number",
        y = y_axis_label,
        colour = "Horizon"
      ) +
      shared_graph_features() +
      scale_color_viridis_d(option="plasma", end = .7) # need to change this to match color in other plot, including horizon 4 being the orange one and horizon 1 being the blue one
    
  }
}

correlation_plot <- function(){
  
  free_choice_df %>%
    filter(
      batch == input$batch_correlation,
      game_avg_accuracy_so_far >= remove_incorrect_trials
    ) %>%
    mutate(
      diff1 = !!sym(glue("mult_{input$diff1_category}")) %>%
        subtract(!!sym(glue("add_{input$diff1_category}"))),
      diff2 = !!sym(glue("mult_{input$diff2_category}")) %>%
        subtract(!!sym(glue("add_{input$diff2_category}")))
    ) %>%
    group_by(free_choice_trial) %>%
    summarize(corr = cor(diff1, diff2)) %>%
    ggplot(aes(free_choice_trial, corr)) +
    geom_line() +
    geom_point(size = 3) +
    theme_bw() +
    labs(
      x = "Free-choice trial number",
      y = "Correlation"
    )
}

fits_plot <- function(outer_iterator_vec){
  
  grab <- function(x) pluck(input, glue("{x}_fits"))
  
  last_digit_group_1 <- str_extract(grab("group_1"), "[:digit:]")
  last_digit_group_2 <- str_extract(grab("group_2"), "[:digit:]")
  sep_expo <- as.logical(grab("separate_by_exposure")) %>%
    and(as.numeric(last_digit_group_1) %% 2 == 1) %>%
    and(as.numeric(last_digit_group_2) %% 2 == 1)
  
  
  matching_conditions <- function(y){
    map_chr(1:2, ~ str_extract(pattern = y, grab(glue("group_{.x}")))) %>%
      n_distinct %>%
      equals(1)
  }
  
  if (matching_conditions(".* horizon")){
    grouping_col_fits <<- sym("free_choice_trial")
    x_axis_prefix <<- "Free Choice Trial "
    x_axis_suffix <<- ""
    abbr_group_1 <<- last_digit_group_1
    abbr_group_2 <<- last_digit_group_2
  } else if(matching_conditions("free choice .*")){
    grouping_col_fits <<- sym("horizon_type")
    x_axis_prefix <<- ""
    x_axis_suffix <<- sym(" Horizon")
    abbr_group_1 <<- str_extract(grab("group_1"), "^([:alpha:])*")
    abbr_group_2 <<- str_extract(grab("group_2"), "^([:alpha:])*")
  } else{
    grouping_col_fits <<- sym("free_choice_label")
    x_axis_prefix <<- ""
    x_axis_suffix <<- ""
    abbr_group_1 <<- grab("group_1")
    abbr_group_2 <<- grab("group_2")
  }
  
  # if (medium == "shiny"){
  #   outer_iterator_vec <- "filler_text"
  # } else if (medium == "manuscript"){
  #   outer_iterator_vec <- c("Difficulty", "Information", "Decision Noise")
  # }
  
  # map(outer_iterator_vec, function(outer_iterator){
  map(outer_iterator_vec, function(outer_iterator){
    map(
      # c("dynamite", "dynamite_effect_within", "dynamite_diff"),
      c("dynamite", "dynamite_effect_within", "dynamite_diff", "scatter"),
      function(graph_type){
        map2(
          c("Unequally", if(sep_expo & medium == "shiny") "Equally"),
          c(F, if(sep_expo & medium == "shiny") T),
          function(plot_title, y){
            
            # if (medium == "shiny"){
            #   keep_batch <- grab("batch")
            # } else if (medium == "manuscript"){
            #   keep_batch <- c("exploratory", "replication", "replication_2")
            # }
            
            paired_df <- load_paired_fits(
              c("exploratory", "replication", "replication_2"), sep_expo,
              grab("iv"), grab("aggregation"), grab("continuum"),
              grab("group_1"), grab("group_2"), y, y
              # keep_batch, sep_expo, grab("iv"), grab("aggregation"),
              # grab("continuum"), grab("group_1"), grab("group_2"), y, y
            ) %>%
              mutate(across(
                !!grouping_col_fits,
                ~fct_relevel(., abbr_group_1, after = 0L)
              ))
            
            add_title <- if (sep_expo) {
              list(
                ggtitle(paste(
                  "Bandits had been", plot_title, "Sampled"
                )),
                theme(plot.title =  element_text(
                  size = 28, family = ggplot_font, hjust = 0.5,
                  margin = margin(t = 10, b = 10, l = 0, r = 0)
                ))
              )
            }
            
            if (y %in% paired_df$equal_exposure){
              
              possible_info_col <- if (y) NULL else as.name("information_coef")
              
              if (graph_type == "dynamite_effect_within"){
                paired_df <- paired_df %>%
                  group_by(subid, batch) %>%
                  arrange(desc(!!grouping_col_fits)) %>%
                  summarise(across(
                    c(!!possible_info_col, difficulty_coef, temp_coef), diff
                  ))
              }
              
              elong_df <- paired_df %>%
                pivot_longer(
                  c(!!possible_info_col, difficulty_coef, temp_coef),
                  names_to = "coef",
                  values_to = "coef_val"
                ) %>%
                mutate(across(coef, ~str_remove(., "_coef"))) %>%
                mutate(across(coef, ~case_when(
                  . == "information" ~ "Information",
                  . == "temp" ~ "Decision Noise",
                  T ~ "Difficulty"
                ))) %>%
                mutate(across(coef, as_factor)) %>%
                mutate(across(coef,  ~fct_relevel(., "Difficulty", after = 0L)))
              
              intra_diffs <- function(avar, color_scheme, x0, x1, x2, x3,
                                      side_1, old_df, acoef){
                df <- filter(old_df, !!sym(acoef) == avar)
                coef_vals <- pull(df, coef_val)
                cis <- CI(coef_vals)[1] - CI(coef_vals)[3]
                list(
                  geom_point(
                    data = df, color = color_scheme, size = 1.5,
                    position = position_nudge(x = x0), alpha = .6
                  ),
                  geom_half_boxplot(
                    data = df, position = position_nudge(x = x1),
                    side = "r", outlier.shape = NA, center = TRUE,
                    errorbar.draw = FALSE, width = .2, fill = color_scheme
                  ),
                  geom_half_violin(
                    data = df, position = position_nudge(x = x2 * .3),
                    side = side_1, fill = color_scheme
                  ),
                  geom_point(
                    data = df, aes(y = mean(coef_val)),
                    position = position_nudge(x = x3 * .13), color = color_scheme,
                    alpha = .6, size = 1.5
                  ),
                  geom_errorbar(
                    data = df,
                    aes(
                      y = mean(coef_val),
                      ymin = mean(coef_val) - cis,
                      ymax = mean(coef_val) + cis
                    ),
                    position = position_nudge(x3 * .13), color = color_scheme,
                    width = 0.05, size = 0.4, alpha = .5
                  )
                )
              }
              
              if (medium == "shiny"){
                coef_or_batch <- sym("coef")
                of_vs_per <- "per"
                inner_iterator <- c(
                  "Difficulty", if(!y) "Information", "Decision Noise"
                )
                elong_df <- elong_df %>%
                  filter(batch == grab("batch"))
              } else if (medium == "manuscript"){
                coef_or_batch <- sym("batch")
                of_vs_per <- "of"
                inner_iterator <- c(
                  "Experiment 1", "Experiment 2", "Experiment 3"
                )
                elong_df <- elong_df %>%
                  filter(coef == outer_iterator) %>%
                  # filter(coef == "Difficulty") %>%
                  mutate(batch = case_when(
                    batch == "exploratory" ~ "Experiment 1",
                    batch == "replication" ~ "Experiment 2",
                    batch == "replication_2" ~ "Experiment 3"
                  ))
              }
              
              setup_y_labels <- function(input1, input2){
                list(ylab(ifelse(
                    input1 %in% c("Difficulty", "Experiment 1"),
                    ifelse(medium == "shiny", input2, str_wrap(input2, 20)),
                    ""
                )))
              }
              
              grp_nm <- "{x_axis_prefix}{eval(grouping_col_fits)}{x_axis_suffix}"
              
              if (graph_type == "dynamite"){
                
                font_size_adj_manuscript <- if (medium == "manuscript"){
                  list(
                    theme(
                      strip.text.x = element_text(size = 21, vjust = 1),
                      axis.text.x = element_text(size = 14),
                      axis.title.y = element_text(size = 14, family = ggplot_font),
                      axis.text.y = element_text(size = 11, family = ggplot_font),
                    )
                  )
                }
                
                elong_df %>%
                  group_by(!!coef_or_batch) %>%
                  mutate(grand_mean = mean(coef_val)) %>%
                  group_by(coef, subid) %>%
                  mutate(
                    coef_val_effect_within = 
                      coef_val - mean(coef_val) + grand_mean
                  ) %>%
                  group_by(!!grouping_col_fits, !!coef_or_batch) %>%
                  summarise(across(coef_val_effect_within, list(
                    mean = mean,
                    sem = ~ sd(.) / (sqrt(n()))
                  ), .names = "{.fn}")) %>%
                  mutate(!!grouping_col_fits := glue(grp_nm)) %>%
                  ggplot(aes(
                    !!grouping_col_fits, mean, fill = !!grouping_col_fits,
                    ymin = mean - sem, ymax = mean + sem
                  )) +
                  geom_bar(stat = "identity", alpha = .8) +
                  geom_errorbar() +
                  theme_minimal() +
                  shared_graph_theme_all_three +
                  labs(x = "", y = "Coefficient Weight") +
                  add_title +
                  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
                  facet_wrap(vars(!!coef_or_batch), scales = "free") +
                  theme(
                    strip.text.x = element_text(size = 28, vjust = 1),
                    axis.title.x=element_blank(),
                    axis.text.x = element_text(size = 18),
                  ) +
                  font_size_adj_manuscript
                
              } else if (graph_type == "dynamite_effect_within") {
                
                font_size_adj_manuscript <- if (medium == "manuscript"){
                  list(
                    theme(
                      axis.title.y = element_text(size = 14, family = ggplot_font),
                      axis.text.y = element_text(size = 11, family = ggplot_font),
                    )
                  )
                }
                
                map(inner_iterator, function(thecol){
                  ggplot(elong_df, aes(x = !!coef_or_batch, y = coef_val)) +
                    geom_hline(yintercept = 0, linetype = "dashed", alpha = .4) +
                    intra_diffs(
                      thecol, "grey", -.2, .05, .6 , -.5, "r", elong_df,
                      coef_or_batch
                    ) +
                    theme_minimal() +
                    shared_graph_theme_all_three +
                    theme(
                      # axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      panel.grid.major.x = element_blank()
                    ) +
                    setup_y_labels(thecol, paste(
                      "Within-Participant Difference", of_vs_per,
                      "Coefficient"
                    )) +
                    xlab("") +
                    font_size_adj_manuscript
                }) %>%
                  wrap_plots()
                
              } else if (graph_type == "dynamite_diff"){
                
                font_size_adj_manuscript <- if (medium == "manuscript"){
                  list(
                    theme(
                      axis.text.x = element_text(size = 14),
                      axis.title.y = element_text(size = 14, family = ggplot_font),
                      axis.text.y = element_text(size = 11, family = ggplot_font),
                    )
                  )
                }
                
                map(inner_iterator, function(x){
                  
                  # elong_df <- filter(elong_df, coef == x)
                  elong_df <- elong_df %>%
                    filter(!!coef_or_batch == x) %>%
                    mutate(!!grouping_col_fits := glue(grp_nm))
                  
                  ggplot(elong_df, aes(x = !!grouping_col_fits, y = coef_val)) +
                    geom_hline(yintercept = 0, linetype = "dashed") +
                    geom_line(
                      aes(group = subid), color = 'lightgray', alpha = .3
                    ) +
                    pmap(
                      list(
                        c(
                          glue("{x_axis_prefix}{abbr_group_1}{x_axis_suffix}"),
                          glue("{x_axis_prefix}{abbr_group_2}{x_axis_suffix}")
                        ),
                        c("#D55E00", "#0072B2"),
                        0, c(.095, -.195), c(.7, -.7), c(.4, -.4), c("r", "l"),
                        list(elong_df), c(grouping_col_fits)
                      ), 
                      intra_diffs
                    ) +
                    xlab("") +
                    setup_y_labels(x, paste(
                      "Coefficient", of_vs_per, "Participant"
                    )) +
                    theme_minimal() +
                    shared_graph_theme_all_three +
                    theme(axis.text.x = element_text(size = 18)) +
                    font_size_adj_manuscript
                  
                }) %>%
                  wrap_plots()
                
              } else if (graph_type == "scatter"){
                
                font_size_adj_manuscript <- if (medium == "manuscript"){
                  list(
                    theme(
                      axis.text.x = element_text(size = 11),
                      axis.title.x = element_text(size = 14, family = ggplot_font),
                      axis.title.y = element_text(size = 14, family = ggplot_font),
                      axis.text.y = element_text(size = 11, family = ggplot_font),
                    )
                  )
                }
                
                elong_df %>%
                  pivot_wider(
                    id_cols = c(subid, !!coef_or_batch),
                    names_from = !!grouping_col_fits,
                    values_from = coef_val,
                    names_glue = glue_collapse(grp_nm)
                  ) %>%
                  ggplot(aes_string(sym(colnames(.)[3]), sym(colnames(.)[4]))) +
                  geom_point(size = .3) +
                  geom_abline(slope = 1, linetype = "dashed") +
                  labs(
                    x = glue(
                      "{x_axis_prefix}{abbr_group_1}{x_axis_suffix} Coefficient"
                    ),
                    y = glue(
                      "{x_axis_prefix}{abbr_group_2}{x_axis_suffix} Coefficient"
                    )
                  ) +
                  theme_minimal() +
                  shared_graph_theme_all_three +
                  theme(strip.text.x = element_blank()) +
                  font_size_adj_manuscript +
                  facet_wrap(vars(!!coef_or_batch), scales = "free")
              } 
            }
          }) %>%
          wrap_plots(nrow = 1) +
          plot_layout(guides = 'collect') &
          theme(legend.position = 'none')
      }) %>%
      reduce(`/`) +
      plot_layout(guides = 'collect')  
  })
  
}

indiv_diffs_math <- function(){
  
  summarized_pilot_data <- pilot_data %>%
    filter(Accuracy == 100) %>%
    group_by(Task, Sub_ID) %>%
    summarise(RT = mean(RT, na.rm = FALSE))
  
  intra_diffs <- function(task, color_scheme, x1, x2, x3, side_1){
    df <- filter(summarized_pilot_data, Task == task)
    rts <- pull(df, RT)
    cis <- CI(rts)[1] - CI(rts)[3]
    list(
      geom_point(data = df, color = color_scheme, size = 1.5, alpha = .6),
      geom_half_boxplot(data = df, position = position_nudge(x = x1),
                        side = "r", outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = .2, fill = color_scheme),
      geom_half_violin(data = df,position = position_nudge(x = x2 * .3),
                       side = side_1, fill = color_scheme),
      geom_point(data = df, aes(y = mean(rts)),
                 position = position_nudge(x = x3), color = color_scheme,
                 alpha = .6, size = 1.5),
      geom_errorbar(data = df, position = position_nudge(x = x3),
                    aes(y = mean(rts), ymin = mean(rts) - cis,
                        ymax = mean(rts) + cis), color = color_scheme,
                    width = 0.05, size = 0.4, alpha = .5)
    )
  }
  
  ggplot(summarized_pilot_data, aes(x = Task, y = RT)) +
    intra_diffs("Addition", "#0072B2", -.19, -.72, -.05, "l") +
    intra_diffs("Multiplication", "#D55E00", .09, .72, .05, "r") +
    geom_line(aes(group = Sub_ID), color = 'lightgray', alpha = .3) +
    labs(
      x = "",
      y = "Average Response Time (seconds)"
    ) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 15, family = ggplot_font),
      axis.text.x = element_text(size = 15, family = ggplot_font),
      axis.text.y = element_text(size = 11, family = ggplot_font),
    )
}

heat_plotter <- function(measure, bottom_x_axis, big_title,
                         compared_min, compared_max, compared_midpont) {
  
  shared_graph_theme <- list(
    theme(
      axis.title.x = element_text(family = ggplot_font),
      axis.title.y = element_text(family = ggplot_font),
      axis.text.x = element_text(family = ggplot_font),
      axis.text.y = element_text(family = ggplot_font),
    )
  )
  
  heat_plot <- pilot_data_summarized %>%
    arrange(desc(Task)) %>%
    group_by(Low_Num, High_Num) %>%
    mutate(comparison = lead(!!sym(measure))) %>%
    slice_head() %>%
    mutate(`Mult - Add` = !!sym(measure) - comparison) %>%
    ggplot(aes(High_Num, Low_Num, fill=`Mult - Add`)) +
    geom_tile() +
    scale_fill_gradient2(low="#0C6291", mid="#FBFEF9", high="#A63446",
                         na.value = "#802835", midpoint = compared_midpont,
                         limits=c(compared_min, compared_max)) +
    scale_x_continuous(breaks=seq(2, 24, 2)) +
    shared_graph_theme +
    theme(
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        # size = 26, hjust = 0.5, margin = margin(t = 10, b = 10, l = 0, r = 0),
        family = ggplot_font
      ),
      legend.text = element_text(family = ggplot_font),
      legend.title = element_text(family = ggplot_font)
    ) +
    scale_y_continuous(breaks=seq(2, 24, 2)) +
    labs(title = big_title, x = "First Number", y = "Second Number")
  
  hist_of_heat <- pilot_data_summarized %>%
    arrange(desc(Task)) %>%
    group_by(Low_Num, High_Num) %>%
    mutate(comparison = lead(Difficulty)) %>%
    slice_head() %>%
    mutate(`Mult - Add` = Difficulty - comparison) %>%
    ggplot(aes(x = `Mult - Add`)) +
    geom_histogram(binwidth = 8, fill = "#d8d4d4", color = "black") +
    shared_graph_theme +
    labs(
      x = paste(bottom_x_axis, "Difference between Mult. and Add"),
      y = "Count"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(-8, 64, 8)) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  heat_plot / hist_of_heat
}
