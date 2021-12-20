library(tidyverse)

washington <- read.csv("../hmda//data/state_WA.csv")

wash_init <- washington %>% filter(loan_purpose == "1", occupancy_type == "1") %>%  
  wash_init %>% select(lei, derived_msa.md, derived_sex, action_taken) %>% 
  filter(lei == "549300MGPZBLQDIL7538", derived_msa.md == "38900") %>% 
  filter(action_taken %in% c(1,3)) %>% 
  count(action_taken, derived_sex) %>% 
  ggplot() + 
  aes(x = derived_sex, y = n, fill = action_taken) + 
  geom_col(position= "stack", stat="identity")


+
  facet_wrap(~derived_sex, nrow=1)
