library(tidyverse)

data <- read.csv("data/state_WA.csv")
lars_names <- read.csv("data/lars_lookup.csv")

data$action_taken

colnames(data)

unique(data$derived_sex)

lars_names %>% filter(lars_names == "action_taken") %>% pull(value)

selected <- "File closed for incompleteness"
lars_names %>% filter(lars_names == "action_taken", value == selected) %>% pull(key)
