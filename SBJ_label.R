## Working code for subject labels.

data <- as_tibble(read.csv("2020-01-07_tx03_DailyMeasures - Sheet1.csv"))
data
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggrepel)
## Date
## Currently, the date code operates specifically for the example data set. 
## It needs to be converted in a way where some input, such as starting date
## can be entered and a function can execute the generation of "time elapsed"

# The function below takes in your data, assumes your date column is titled "date"
## Function returns dataframe "data_new" with included column for "Day"
dt_t_tm <- function (file, orig_dt) {
  data <- as_tibble(read.csv(file))
  data %<>% 
    mutate(date = as.POSIXct(date, format="%m/%d/%Y")) 
  ## Use POSIXct for changing date from char to date
  original_date <- as.Date(orig_dt, tz = "MST") 
  #good_date <- strptime(date, "%m/%d/%Y")
  time_elapsed <- difftime(data$date, original_date, units = 'days')
  time_elapsed_rounded <- round(time_elapsed)
  time_elapsed_rounded
  data_new <- data %>%
    mutate(time_elapsed_rounded)
  return(data_new)
  # Use time_elapsed rounded 
}
data_new <- dt_t_tm("2020-01-07_tx03_DailyMeasures - Sheet1.csv", "2020-01-07")

ggplot(data=data_new, 
       mapping=aes(x=time_elapsed_rounded, 
                   y=body_mass_g, 
                   color=treatment,
                   group=sbj
       )) +
  geom_line() +
  ## below Allows for plotting of geom_text as a result of the x axis being a particular value.
  ## IF x = 42, only then is label=sbj applied. If not, "" means blank character.
  geom_label_repel(aes(label=ifelse(time_elapsed_rounded==max(time_elapsed_rounded),sbj,"")),
                   nudge_x=5,
                   max.overlaps=Inf,
                   segment.color="black",
                   segment.linetype = 2,
                   show.legend=FALSE) 
## Checking Unique number of subjects
#sbjs <- unique(data_new$sbj)
#length(sbjs)