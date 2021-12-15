library(tidyverse)

data <- read.csv("data/state_WA.csv")

data$action_taken

colnames(data)

unique(data$derived_sex)

lars_names %>% filter(lars_names == "action_taken") %>% pull(value)

selected <- "File closed for incompleteness"
lars_names %>% 
  filter(lars_names == "action_taken", value == selected) %>% 
  pull(key)



data %>% filter(lei == "549300HW662MN1WU8550") %>% pull(census_tract)
  

data$ffiec_msa_md_median_family_income




