library(tidyverse)

washington <- read.csv("data/state_WA.csv")

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

lei_names <- read.csv("data/lei_name.csv")
washington <-read_csv('data/state_WA.csv')
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 
washington %>% mutate(name_trunc = name[1:20])

w5 <- washington %>% head(5)
substr(w5$name, 1,20)
