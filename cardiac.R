# DR. RUGURU data

library('rio') # import
library('janitor') # cleaning column names
library('ggplot2') # visualization
library('tidyverse') # data management
library('dplyr')  # data management

cr <- import('CardiacRehabilitatio-Functionalcapacitype_DATA_LABELS_2024-02-08_1528 (1).csv')

cr <- clean_names(cr)
names(cr)

# Objective
# 1. Compare Week 1's target, what the device recorded and what the participant reported.
# 2. Compare trend of subsequent weeks


# Data Cleaning and Transformation

# First obtain the participants asked to exercise
# get rows 1:93, those who were asked to exercise

exercise <- cr[1:93, ]

exercise_hbcr <- exercise[grepl("HBCR", exercise$event_name), ]

# WEEK 1 STEP COUNT COMPARISON
week_1 <- exercise_hbcr[c("wk_1_step_count_target","wk_1_step_count_achieved_device")]

# merge study IDs to steps device
id <- exercise_hbcr[,1:2]
# bind week 1 with study id
week_1_df <- cbind(id, week_1)



## dropping study ID 5, value appears to be an outlier
week_1_df <- week_1_df[-1, ]
rownames(week_1_df) <- NULL



## Plot week 1 comparison: Target vs Achieved
# Pivoted the data frame to a long format
long_week_1 <- week_1_df %>% 
  pivot_longer(
    cols = c("wk_1_step_count_target","wk_1_step_count_achieved_device"),
    names_to = 'study_weeks',
    values_to = 'steps_taken'
  )


# change study_id column to character
# So that y axis isn't a continuous variable
long_week_1$study_id <- as.character(long_week_1$study_id)

# visualize the plot
long_week_1 %>% 
  ggplot(aes(x=steps_taken, y=study_id, group= study_id, color=study_weeks))+
  geom_line()+
  geom_point()+
  labs(x = "Step_Counts", y = "Study IDs", title = "Week 1 Step Count Target vs Achieved")+
  theme_minimal()














## Those told not to monitor

exercise <- cr[94:225, ] # those who were told not to monitor steps
# 2nd, obtain the Arm: Home Based row
exercise_hbcr <- exercise[grepl("HBCR", exercise$event_name), ]

# column names
names(exercise_hbcr)
# not to monitor
week_1 <- exercise_hbcr[c("wk_1_step_count_achieved_participant_report_via_phone","step_count_achieved_participant_report_via_phone")]


# merge study IDs to steps device
id <- exercise_hbcr[,1:2]
# bind week 1 with study id
week_1_df <- cbind(id, week_1)



## dropping study ID 5, value appears to be an outlier
# week_1_df <- week_1_df[-1, ]
rownames(week_1_df) <- NULL

## in this case, we did not have study id 5

## Plot week 1 comparison vs followup
# Pivoted the data frame to a long format
long_week_1 <- week_1_df %>% 
  pivot_longer(
    cols = c("wk_1_step_count_achieved_participant_report_via_phone","step_count_achieved_participant_report_via_phone"),
    names_to = 'study_weeks',
    values_to = 'steps_taken'
  )


# change study_id column to character
# So that y axis isn't a continuous variable
long_week_1$study_id <- as.character(long_week_1$study_id)

# visualize the plot
long_week_1 %>% 
  ggplot(aes(x=steps_taken, y=study_id, group= study_id, color=study_weeks))+
  geom_line()+
  geom_point()+
  labs(x = "Step_Counts", y = "Study IDs", title = "Week 1 Step Count  vs Week 2")+
  theme_minimal()














# Objective 2
# Trends Over the weeks
steps <- cr[, grepl("Step count", names(cr))]
steps_device <- cr[, grepl("device", names(cr))]

# merge study IDs to steps device
study_id <- cr[,1:2]

## clean column names
names(steps_device) <- NULL
# New column names
names <- c("1","2","3","4","5","6","7","8","9","10","11","12")
# Assign new column names
names(steps_device) <- names


study_steps_device <- cbind( study_id, steps_device)
names(study_steps_device)



## those told to monitor

# get rows 1:93, those who were asked to exercise
e_study_steps_device <- study_steps_device[1:93, ]


# get HBCR row
study_hbcr <- e_study_steps_device[grepl("HBCR", e_study_steps_device$event_name), ]

# drop 2nd column
study_hbcr <- study_hbcr[,-2]
rownames(study_hbcr) <- NULL

names(study_hbcr)



### Visualization

long_hbcr <- study_hbcr %>%
  pivot_longer(
    cols = 2:13,
    names_to = "weeks",
    values_to = "step_taken"
  )

# change weeks to time format
# -change to numeric

class(long_hbcr$weeks)
long_hbcr$weeks <- as.numeric(long_hbcr$weeks)

long_hbcr$study_id <- as.character(long_hbcr$study_id)



reference_date <- as.Date("2024-01-01")
long_hbcr$weeks <- as.Date( reference_date + (long_hbcr$weeks *7))



# ggplotly
library("plotly")
# line plot
long_hbcr_poltly <- long_hbcr %>% 
  ggplot(aes(x=weeks, y= step_taken, color=study_id))+
  geom_point()+
  geom_line()
  # scale_x_discrete(labels = c('week 1','week 2', 'week 3', 'week 4', 'week 5',
  #                             'week 6', 'week 7', 'week 8', 'week 9', 'week 10',
  #                             'week 11', 'week 12'))

plot_ids <- ggplotly(long_hbcr_poltly)
plot_ids
