# DR. RUGURU data

library('readr')
library('janitor')
library('dplyr')
library('ggplot2')
library('tidyverse')

cr <- read_csv("C:/Users/Hp/OneDrive/OLD FILES/Documents/PROJECTS/MEDICINE/cardiac/CardiacRehabilitatio-Functionalcapacitype_DATA_LABELS_2024-02-08_1528 (1).csv")
cr <- clean_names(cr)



# Data Cleaning
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



# get rows 1:93
e_study_steps_device <- study_steps_device[1:93, ]
# get HBCR row
hbcr <- e_study_steps_device[grepl("HBCR", e_study_steps_device$event_name), ]

# drop 2nd column
hbcr <- hbcr[,-2]
row.names(hbcr) <- NULL

names(hbcr)




### Visualization

long_hbcr <- hbcr %>%
  pivot_longer(
    cols = 2:13,
    names_to = "weeks",
    values_to = "stepz"
  )

# change weeks to time format
# -change to numeric
class(long_hbcr$weeks)
long_hbcr$weeks <- as.numeric(long_hbcr$weeks)
long_hbcr$study_id <- as.character(long_hbcr$study_id)

reference_date <- as.Date("2024-01-01")
long_hbcr$weeks <- as.Date( reference_date + (long_hbcr$weeks *7))

# line plot

long_hbcr %>% 
  ggplot()+
  geom_line(aes(x=weeks, y= stepz, color=study_id))


