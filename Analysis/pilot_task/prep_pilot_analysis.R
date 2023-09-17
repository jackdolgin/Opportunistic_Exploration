pilot_data_raw <- here("Data", "Pilot_Task", "experimental_trials.csv") %>%
  read_csv %>%
  mutate(
    across(type, ~if_else(. == "add", "Addition", "Multiplication")),
    across(c(ends_with("rt"), "time_elapsed"), ~divide_by(., 1000)),
    across(accuracy, ~multiply_by(., 100)),
    )

pilot_data <- pilot_data_raw %>%
  group_by(subid) %>%
  filter(mean(accuracy) >= min_pilot_math_acc) %>%
  ungroup() %>%
  filter(math_rt < longest_time_for_baseline_math)

names(pilot_data) <- c(
  "Sub_ID", "Response", "Answer", "Task", "High_Num", "Low_Num", "Accuracy",
  "RT", "Difficulty", "Slider_RT", "Trial", "Time_Elapsed")

pilot_data_summarizer <- function(x, y) {
  pilot_data %>%
    mutate(
      across(c(RT, Difficulty), ~if_else(y & Accuracy == 0, NA_real_, .))
    ) %>%
    group_by(!!!x) %>%
    summarise(across(c(Accuracy, RT, Difficulty), ~mean(., na.rm = TRUE)))
}

pilot_data_summarized <- pilot_data_summarizer(quos(Low_Num, High_Num, Task), F)

pilot_data_high_nums <- pilot_data_summarizer(quos(High_Num, Task), F)

pilot_data_filtered_and_summarized <-
  pilot_data_summarizer(quos(Low_Num, High_Num, Task), T)
