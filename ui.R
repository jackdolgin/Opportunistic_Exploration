library(shiny)
library(shinyWidgets)

addResourcePath("sdcwww", here("www"))

shinyUI(
  navbarPage(
    id="mainnav",
    theme = paste0("sdcwww/", getShinyOption(".guitheme")),
    "Opportunistic Exploration",
    
    tabPanel("Choice Curves", fluidRow(
      column(
        2,
        list(fluidRow(column(
          12,
          prettyRadioButtons(
            "batch_choice_curve",
            "Data set",
            c(
              "Exploratory" = "exploratory",
              "Replication 1" = "replication",
              "Replication 2" = "replication_2"
            ),
            "replication"
          ),
          prettyRadioButtons(
            "x_comparison_group",
            "Bandits to compare",
            c(
              "Multiplication vs. addition" = "mult",
              "More vs. less reward variability" = "rew_exposed_cum_sd",
              "More vs. less mean reward" = "rew_exposed_cum_mean",
              "More vs. less underlying reward" = "rew_latent",
              "Right vs. left" = "side",
              "One random vs. the other" = "rand"
            ),
            "mult"
          ),
          prettyRadioButtons(
            "x_comparison_dimension",
            "Dimension to compare bandits (x-axis)",
            c(
              "Baseline RT" = "rt",
              "Expected reward" = "rew_exposed_cum_mean",
              "Underlying reward" = "rew_latent",
              "Baseline self-rated difficulty" = "difficulty",
              "Randomly generated number" = "rand",
              "Reward variability" = "rew_exposed_cum_sd"
            ),
            "rt"
          ),
          prettyRadioButtons(
            "x_raw_or_percent",
            "Convert independent variable to a rank-ordered scale or keep as raw",
            c("Raw" = "raw", "Rank-ordered" = "percent"),
            "percent"
          ),
          prettyRadioButtons(
            "y_var_choice_curve",
            "Dependent variable (y-axis)",
            c(
              "Choosing multiplication" = "mult",
              "Answering math correctly" = "accuracy",
              "Choosing given random bandit" = "bigger_rand",
              "Choosing right side bandit" = "bigger_side",
              "Choosing higher valued bandit" = "bigger_rew_exposed_cum_mean",
              "Choosing more exposed bandit" = "bigger_rew_exposed_cum_count",
              "Choosing bandit w/ more reward" = "bigger_rew_latent",
              "Choosing more variable bandit" = "bigger_rew_exposed_cum_sd"
            ),
            "mult"
          ),
          prettyCheckboxGroup(
            "choice_combos_choice_curve",
            "Trials to keep (legend)",
            free_choice_df %>%
              arrange(desc(horizon_type), free_choice_trial) %>%
              pull(free_choice_label) %>%
              unique %>%
              as.character %>%
              str_sort(numeric = T),
            c(
              "Long horizon, free choice 1",
              "Short horizon, free choice 1"
            )
          ),
          prettyRadioButtons(
            "show_CI_choice_curve",
            "Show confidence interval",
            c("Yes" = T, "No" = F),
            T
          )
        ))),
        class="wb_sidebar"
      ),
      column(10, plotOutput("choice_curve_plot", height = '600px'), class="wb-maincolumn")
    )),
    
    tabPanel("Model Fits", fluidRow(
      column(
        2,
        list(fluidRow(column(
          12,
          prettyRadioButtons(
            "batch_fits",
            "Data set",
            c(
              "Exploratory" = "exploratory",
              "Replication 1" = "replication",
              "Replication 2" = "replication_2"
            ),
            "replication"
          ),
          prettyRadioButtons(
            "iv_fits",
            "Independent variable",
            c(
              "Response Time" = "rt",
              "Difficulty" = "difficulty"
            ),
            "rt"
          ),
          prettyRadioButtons(
            "aggregation_fits",
            "Convert independent variable to a rank-ordered scale or keep as raw",
            c(
              "Raw" = "raw",
              "Rank-ordered" = "percentile"
            ),
            "raw"
          ),
          prettyRadioButtons(
            "separate_by_exposure_fits",
            "Analyze trials separately depending on whether each side has been viewed equally",
            c(
              "Together" = FALSE,
              "Separately" = TRUE
            ),
            FALSE
          ),
          prettyRadioButtons(
            "continuum_fits",
            "Subsequently cluster independent variable into bins of seven",
            c(
              "Don't cluster" = "continuous",
              "Cluster" = "ordinal"
            ),
            "continuous"
          ),
          prettyRadioButtons(
            "group_1_fits",
            "First trial type for comparison",
            free_choice_df %>%
              pull(free_choice_label) %>%
              unique %>%
              as.character,
            "Short horizon, free choice 1"
          ),
          prettyRadioButtons(
            "group_2_fits",
            "Second trial type for comparison",
            free_choice_df %>%
              pull(free_choice_label) %>%
              unique %>%
              as.character,
            "Long horizon, free choice 1"
          )
        ))),
        class="wb_sidebar"
      ),
      column(10, plotOutput("fits_plot", height = '1100px'), class="wb-maincolumn")
    )),
    
    tabPanel("Learning Curves", fluidRow(column(
      2,
      list(fluidRow(column(
        12,
        prettyRadioButtons(
          "batch_learning_curve",
          "Data set",
          c(
            "Exploratory" = "exploratory",
            "Replication 1" = "replication",
            "Replication 2" = "replication_2"
          ),
          "exploratory"
        ),
        prettyRadioButtons(
          "y_var_learning_curve",
          "Independent variable",
          c(
            "Choosing multiplication" = "mult",
            "Answering math correctly" = "accuracy",
            "Choosing given random bandit" = "bigger_rand",
            "Choosing right side bandit" = "bigger_side",
            "Choosing higher valued bandit" = "bigger_rew_exposed_cum_mean",
            "Choosing more exposed bandit" = "bigger_rew_exposed_cum_count",
            "Choosing bandit w/ more reward" = "bigger_rew_latent",
            "Choosing more variable bandit" = "bigger_rew_exposed_cum_sd"
          ),
          "mult"
        ),
        prettyRadioButtons(
          "show_CI_learning_curve",
          "Show confidence interval",
          c("Yes" = T, "No" = F),
          T
        )
      ))),
      class="wb_sidebar"
    ),
    column(10, plotOutput("learning_curve_plot", height = '600px'), class="wb-maincolumn")
  ))#,
    
    # tabPanel("Correlation Plot", column(
    #   4,
    # )),
    # # tabPanel("Correlation Plot", fluidRow(
    # #   column(
    # #     2,
    # #     list(fluidRow(column(
    # #       12,
    # #       # rest of ui code goes here
    # #     ))
    # #     ),
    # #     class="wb_sidebar"
    # #   ),
    # #   column(10, plotOutput("mygraph1", height = '1100px'), class="wb-maincolumn")
    # # )),
    # tabPanel("About/Help", uiOutput("ui_about"))
    
  )
)
