library(tidyverse)

wash_init <- read.csv("data/wash_init.csv")
lei_names <- read.csv("data/lei_name.csv")

lei_msa <- wash_init %>% select(lei, derived_msa.md) %>% distinct()

joined <- left_join(
  lei_msa,
  lei_names
)

write_csv(joined, "data/lei_names_msa.csv")

lei_names_msa <- read.csv("data/lei_names_msa.csv")

