# need to filter out participants who were wrong at least a certain portion of the time
# Import Packages, Import Data, and Summarize Data ------------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(ggpubr, Rmisc, tidyverse, here, magrittr, patchwork, gghalves)

pilot_data <- read_csv(here("Data", "Pilot_Task", "pilot.csv")) %>%
  mutate(across(type, ~if_else(. == "add", "Addition", "Multiplication")),
         across(c(ends_with("rt"), "time_elapsed"), ~divide_by(., 1000)),
         across(accuracy, ~multiply_by(., 100))) %>%
  filter(math_rt < 75)
  
names(pilot_data) <- c(
  "Sub_ID", "Response", "Answer", "Task", "High_Num", "Low_Num", "Accuracy",
  "RT", "Difficulty", "Slider_RT", "Trial", "Time_Elapsed")
  
pilot_data_summarizer <- function (x) {
  pilot_data %>%
    group_by(!!!x) %>%
    summarise(across(c(Accuracy, RT, Difficulty), ~mean(., na.rm = TRUE)))
}

pilot_data_summarized <- pilot_data_summarizer(quos(Low_Num, High_Num, Task))

pilot_data_high_nums <- pilot_data_summarizer(quos(High_Num, Task))



# Plotting Functions ------------------------------------------------------

plotter <- function(df, x, y, x_label, y_label, min_x, max_x, min_y, max_y,
                     label_nudge, y_seq_start, y_seq_end, y_seq_delta, geom) {
  ggplot(eval(sym(df)), aes(!!sym(x), !!sym(y), color = Task)) +
    geom +
    labs(title = paste0("Relationship between\n", x_label, " and ", y_label),
         subtitle = "as a function of the type of math problem",
         x = x_label,
         y = y_label) +
    stat_cor(aes(label = paste(..r.label.., sep = "~‘")),
             position = position_nudge(y = label_nudge),
             show.legend = FALSE) +
    coord_cartesian(xlim = c(min_x, max_x), ylim = c(min_y, max_y)) +
    scale_y_continuous(breaks=seq(y_seq_start, y_seq_end, y_seq_delta))
}

heat_plotter <- function(measure, measure_text, by_task_min, by_task_max,
                         by_task_midpoint, compared_min, compared_max,
                         compared_midpont, low_color, mid_color,
                         high_color) {
  
  heat_by_task <- function(task) {
    pilot_data_summarized %>%
      filter(Task == task) %>%
      ggplot(aes(High_Num, Low_Num, fill=!!sym(measure))) +
      geom_tile() +
      scale_fill_gradient2(low=low_color, mid=mid_color, high=high_color,
                           na.value = "#802835", midpoint = by_task_midpoint,
                           limits=c(by_task_min, by_task_max)) +
      labs(subtitle =paste(sym(measure_text), "for", task, "Task,\nby",
                           if_else(task == "Addition", 
                                   "Addends",
                                   "Multiplicands")),
           x = "", y = "")
  }
  
  heats_compared <- pilot_data_summarized %>%
    arrange(desc(Task)) %>%
    group_by(Low_Num, High_Num) %>%
    mutate(comparison = lead(!!sym(measure))) %>%
    filter(row_number() == 1) %>%
    mutate(`Mult - Add` = !!sym(measure) - comparison) %>%
    ggplot(aes(High_Num, Low_Num, fill=`Mult - Add`)) +
    geom_tile() +
    scale_fill_gradient2(low=low_color, mid=mid_color, high=high_color,
                         na.value = "#802835", midpoint = compared_midpont,
                         limits=c(compared_min, compared_max)) +
    scale_x_continuous(breaks=1:24) +
    theme(panel.grid.minor.x = element_blank()) +
    scale_y_continuous(breaks=seq(2, 24, 2)) +
    labs(title = paste("Multiplication", sym(measure_text), "\n- Addition",
                       sym(measure_text)),
         x = "", y = "")
  
  (c("Addition", "Multiplication") %>%
      map(heat_by_task) %>%
      reduce(`+`) + plot_layout(guides = 'collect') +
      theme(legend.position = "right")) %>%
    divide_by(heats_compared)
}

intra_diffs <- function(task, color_scheme, x1, x2, side_1){
  df <- filter(d, Task == task)
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
               position = position_nudge(x = x2 * .13), color = color_scheme,
               alpha = .6, size = 1.5),
    geom_errorbar(data = df, aes(y = mean(rts), ymin = mean(rts) - cis,
                                 ymax = mean(rts) + cis),
                  position = position_nudge(x2 * .13), color = color_scheme,
                  width = 0.05, size = 0.4, alpha = .5)
  )
}

plot_5_theme <- list(
  theme_minimal(),
  theme(#axis.text.x = element_blank(),
    axis.line=element_line(color="white"), 
    plot.title = element_text(color="white"),
    plot.background = element_rect(fill="#1e394a"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    panel.background=element_blank(),
    axis.text.x=element_text(angle=0, color="white", size=16, vjust= 0.5),
    axis.text.y=element_text(angle=0, color="white", size=16), 
    strip.text=element_text(face="bold", size=22),
    axis.line.x = element_line(color="white", size = 0.5),
    axis.line.y = element_line(color="white", size = 0.5),
    axis.ticks = element_blank(),
    strip.text.y=element_text(color="white"),
    strip.background = element_rect(fill="#1e394a"),
    legend.position="none",
    axis.title = element_text(size=24, face="bold", color="white"),
    panel.spacing.x = unit(1.25,"lines"),
    panel.spacing.y = unit(1.75,"lines"))
)

# Plot 1 ------------------------------------------------------------------

plotter("pilot_data", "RT", "Difficulty", "Response Time",
        "Rated Difficulty", 0, 75, 0, 110, 10, 0, 100, 10,
        list(geom_point(size = .4))) +
  theme_classic() +
  labs(caption = paste("Each of the",
                       nrow(pilot_data),
                       "dots represents one trial for one",
                       "participant\n(filtered for trials with RT > 75)"))


# Plot 2 ------------------------------------------------------------------

list(
  "pilot_data_summarized",
  c("RT", "RT", "Accuracy"),
  c("Difficulty", "Accuracy", "Difficulty"),
  c("Response Time (seconds)", "Response Time (seconds)", "Accuracy (%)"),
  c("Rated Difficulty", "Accuracy (%)", "Rated Difficulty"),
  c(0, 0, 35),
  c(30, 30, 100),
  c(0, 35, 0),
  c(75, 110, 75),
  c(0, 10, 6),
  c(0, 30, 0),
  c(80, 100, 80),
  10,
  list(geom_point(size = 1))
) %>%
  pmap(plotter) %>%
  reduce(`+`) +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Averages for Each Combination of Addends and Multiplicands",
    subtitle = paste("Each of the",
                    nrow(pilot_data_summarized),
                    "dots per graph represents one trial",
                    "averaged across subgroups of approximately",
                    round(n_distinct(pilot_data$Sub_ID) / 4),
                    "participants\n"))


# Plot 3 ------------------------------------------------------------------

list(
  "pilot_data_high_nums",
  "High_Num",
  c("Accuracy", "RT", "Difficulty"),
  "Largest Addend or Multiplicand",
  c("Accuracy (%)", "Response Time (seconds)", "Rated Difficulty"),
  0,
  25,
  c(65, 0, 0),
  c(100, 15, 50),
  50,
  c(60, 0, 0),
  c(100, 15, 50),
  c(10, 3, 10),
  list(
    list(geom_line(size = 1), geom_point(size = 2))
  )
) %>%
  pmap(plotter) %>%
  reduce(`+`) +
  plot_layout(guides = 'collect') +
  plot_annotation(title = "Averages for Largest Addend and Multiplicand",
                  subtitle = paste("Each value is collapsed across",
                                  n_distinct(pilot_data$Sub_ID),
                                  "participants\n"))


# Plot 4 ------------------------------------------------------------------

list(
  c("RT", "Accuracy", "Difficulty"),
  c("Response Time", "Accuracy", "Rated Difficulty"),
  c(0, 40, 0),
  c(25, 100, 100),
  c(0, 100, 0),
  c(-2, -50, -10),
  c(20, 10, 60),
  0,
  c("#0C6291", "#A63446", "#0C6291"),
  "#FBFEF9",
  c("#A63446", "#0C6291", "#A63446")
) %>%
  pmap(heat_plotter) %>%
  reduce(`|`) +
  plot_annotation(title = "Addition and Multiplication Problems, and their Difference",
                  subtitle = "grouped, left to right, by response time, accuracy, and self-reported difficulty\n")


# Plot 5 ------------------------------------------------------------------

plot_5_data <- pilot_data %>%
  filter(Accuracy == 100) %>%
  group_by(Low_Num, High_Num, Task) %>%
  summarise(across(RT, mean)) %>%
  ungroup() %>%
  # unite("Combo", Low_Num:High_Num) %>%
  # arrange(Combo, Task) %>%
  mutate(across(RT, ~. - lag(.))) %>%
  filter(Task == "Multiplication") %>%
  arrange(RT) %>%
  mutate(#across(Combo, as_factor),
         Change = lead(RT) - RT,
         row_rank = 100 * row_number() / n())

small_5_plot <- plot_5_data$RT %>% min %>% floor
big_5_plot <- plot_5_data$RT %>% max %>% ceiling
plot_5_data_expanded <- plot_5_data %>%
  expand(nesting(row_rank, Change),
         RT = full_seq(c(small_5_plot, big_5_plot), 1))

(plot_5_data %>%
    group_by(ceiling(RT)) %>%
    mutate(Group_Count = n()) %>%
    ggplot(aes(row_rank, Group_Count)) +
    geom_bar(stat='identity', width=1) +
    plot_5_theme +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank())) /
  
(ggplot(plot_5_data, aes(row_rank, RT)) +
  geom_raster(data = plot_5_data_expanded,
              aes(row_rank, RT, fill = Change), interpolate = TRUE) +
  geom_line(aes(x=row_rank, RT), color="white",size=1.5) +
  # scale_fill_gradient(low="#1e394a", high="#d26700") +
  scale_fill_gradient2(low="#1e394a", mid="#bc6c0d", high="#d26700", midpoint = 1.1) +
  plot_5_theme +
  coord_cartesian(ylim = c(small_5_plot + 1, big_5_plot -1)) +
  scale_x_continuous(breaks=seq(25, 75, 25)))


# Plot 6 ------------------------------------------------------------------

d <- pilot_data %>%
  filter(Accuracy == 100) %>%
  group_by(Task, Sub_ID) %>%
  summarise(RT = mean(RT, na.rm = FALSE))

ggplot(data = d, aes(x = Task, y = RT)) +
  intra_diffs("Addition", "#0072B2", -.28, -1, "l") +
  intra_diffs("Multiplication", "#D55E00", .18, 1, "r") +
  geom_line(aes(group = Sub_ID), color = 'lightgray', alpha = .3) +
  xlab("Condition") + ylab("Value") +
  ggtitle('Figure 6: Repeated measures with box- and violin plots') +
  theme_minimal()


# Plot 7 ------------------------------------------------------------------

df_ranked <- pilot_data_summarized %>%
  group_by(Task) %>%
  mutate(myrank = percent_rank(RT)) %>%
    ungroup() %>%
  filter(Task == "Multiplication")

df_ranked %>%
  filter(myrank > .5) %>%
  left_join(pilot_data,
            by = c("Low_Num", "High_Num", "Task"),
            suffix = c("_agg", "")) %>%
  filter(Accuracy == 100) %>%
  group_by(Task, Sub_ID) %>%
  summarise(RT = mean(RT, na.rm = FALSE)) %>%
  ungroup() %>%
  ggplot(aes(x =Task , y = RT)) +
  geom_violin(size = .1) +
  geom_boxplot(position = position_nudge(x = 1), size = .1) +
  geom_point(size = .1)


# Plot 8 ------------------------------------------------------------------


pilot_data %>%
  filter(Task == "Multiplication", Accuracy == 100) %>%
  group_by(Low_Num, High_Num) %>%
  mutate(Combo_RT_mean = mean(RT)) %>%
  ungroup() %>%
  mutate(df_rank_percentile = percent_rank(Combo_RT_mean)) %>%
  group_by(Sub_ID,
           sweetspot = df_rank_percentile >= .85 & df_rank_percentile < .95) %>%
  summarise(across(RT, mean),
            total = n() ) %>%
  ungroup() %>%
  filter(total >= 3) %>%
  group_by(Sub_ID) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(-total) %>%
  pivot_wider(names_from = sweetspot, values_from = RT) %>%
  magrittr::set_names(c("Sub_ID", "Full", "Sample")) %>%
  ggplot(aes(x=Sample, y=Full)) +
  geom_point() +
  geom_smooth() +
  stat_cor(aes(label = paste(..r.label.., sep = "~‘")),
           # position = position_nudge(y = RT),
           show.legend = FALSE)
# 
# 
# df_ranked_filtered <- df_ranked %>%
#   left_join(pilot_data,
#             by = c("Low_Num", "High_Num", "Task"),
#             suffix = c("_agg", "")) %>%
#   filter(Accuracy == 100) %>%
#   mutate(small_group =case_when(
#     (myrank > .7 & myrank < .75) ~ "Sample",
#     myrank >= .3 ~ "Full",
#     TRUE ~ "Toss"))
# 
# df_ranked_filtered %>%
#   group_by(small_group, Sub_ID) %>%
#   summarise(RT = median(RT, na.rm = FALSE)) %>%
#   ungroup() %>%
#   pivot_wider(names_from = small_group, values_from = RT) %>%
#   ggplot(aes(x=Sample, y=Full)) +
#   geom_point() +
#   geom_smooth() +
#   stat_cor(aes(label = paste(..r.label.., sep = "~‘")),
#            # position = position_nudge(y = RT),
#            show.legend = FALSE)
# 
# 
# 

# Plot 9 ------------------------------------------------------------------

max_range <- 10

df_plot_9_setup <- pilot_data %>%
  filter(Task == "Multiplication", Accuracy == 100) %>%
  group_by(Low_Num, High_Num) %>%
  mutate(RT_mean = mean(RT))

df_plot_9 <- df_plot_9_setup %>%
  summarise(across(RT, mean), .groups = "drop") %>%
  mutate(df_rank_absolute = dense_rank(desc(RT)),
         df_rank_percentile = percent_rank(RT))#

df_plot_9_setup %<>% left_join(df_plot_9, by = c("Low_Num", "High_Num"))

boomer <- df_plot_9 %>%
  expand(nesting(Low_Num, High_Num, df_rank_absolute, RT, df_rank_percentile),
         next_rows = full_seq(4:max_range, 1)) %>%
  ungroup() %>%
  filter(df_rank_absolute + 5 <  max(df_rank_absolute) ) %>%
  # filter(next_rows <= df_rank_absolute) %>%
  filter(row_number() < 15) %>% #remove this after testing
  mutate(Correlation = pmap_dbl(., function(df_rank_absolute, next_rows, ...){
    
    tryCatch(
      {
        df_rank_low <- df_rank_absolute
        df_rank_high <- df_rank_absolute + next_rows
        
        ready_for_cor <- df_plot_9_setup %>%
          group_by(df_rank_absolute >= df_rank_low &
                     df_rank_absolute < df_rank_high) %>%
          group_split() %>%
          map_df(function(z){
            z %>%
              group_by(Sub_ID) %>%
              summarise(RT_mean = mean(RT.x),
                        total_instances = n(),
                        .groups="drop") %>%
              mutate(rand_name = runif(1, min=0, max=99999999999))
          }) %>%
          group_by(Sub_ID) %>%
          filter(min(total_instances) >= 3) %>%
          ungroup() %>%
          select(-total_instances) %>%
          pivot_wider(names_from = rand_name, values_from = RT_mean) %>%
          filter(across(everything(), ~ !is.na(.x)))
        
        cor(pull(ready_for_cor, 2),
            pull(ready_for_cor, 3))
      },
      error=function(e) {
        return(0)
      }
    )
    # return (out)
  }
  )
)

ggplot(boomer, aes(df_rank_percentile, next_rows, fill= Correlation)) +
   geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(direction = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(.1, .9, .1)) +
  scale_y_continuous(breaks = seq(1, 10, 1))
   

# Plot 10 -----------------------------------------------------------------

pilot_data %>%
  group_by(Sub_ID) %>%
  filter(Task == "Multiplication") %>%
  summarise(Performance = mean(Accuracy),
            Response_Time = mean(RT)) %>%
  ggplot(aes(Response_Time, Performance)) +
  geom_point()
