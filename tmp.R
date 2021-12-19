library(tidyverse)

washington <- read.csv("../hmda//data/state_WA.csv")
wash_init <- read.csv("data/wash_init.csv")
lars_names <- read.csv("data/lars_lookup.csv")
lei_names <- read.csv("data/lei_name.csv")


washington$action_taken

colnames(washington)

unique(washington$derived_sex)

lars_names %>% filter(lars_names == "action_taken") %>% pull(value)

selected <- "File closed for incompleteness"
lars_names %>% 
  filter(lars_names == "action_taken", value == selected) %>% 
  pull(key)



washington %>% filter(lei == "549300HW662MN1WU8550") %>% pull(census_tract)
  

washington$ffiec_msa_md_median_family_income


washington %>% ggplot()


count(wash_init, derived_msa.md) # 16
count(washington, county_code) # 40
count(washington, census_tract) # 1442

count(washington, purchaser_type) # 
count(washington, derived_loan_product_type) # 
count(washington, loan_type) # 
count(washington, loan_purpose) # 
count(washington, occupancy_type) # 


wash_init %>% 

one_lei <- wash_init %>% filter(lei == "WKN6AF1FCL7BBYGTGI83")

wi_eth_tib <- wash_init %>% filter(derived_ethnicity == "Hispanic or Latino") %>%
  count(action_taken)

lei_eth_tib <- one_lei %>% filter(derived_ethnicity == "Hispanic or Latino") %>%
  count(action_taken)


eth_compare <- left_join(lei_eth_tib, wi_eth_tib, by = "action_taken")

wash_init <- washington %>% filter(loan_purpose == "1", occupancy_type == "1") 
write_csv(wash_init, "data/wash_init.csv")

lei_names %>% filter(name == "21ST MORTGAGE CORPORATION") %>% pull(lei)

wash_init %>% filter(lei == "WKN6AF1FCL7BBYGTGI83")

tmp <- count(wash_init, derived_msa.md) # 16
write_csv(tmp, "data/tmp.csv")

count(wash_init, lei)
lei_names_msa <- left_join(lei_names,  wash_init) %>% select(lei, name, derived_msa.md)
write_csv(lei_names, "data/lei_names.csv")


if (input$select_msa == "WA") {
  { ifelse(Y ,add(.,1), . ) }
  {if (y=="") filter(., x>3) else filter(., x<3)} %>% 
    {if (y=="") filter(., x>3) else filter(., x<3)} %>% 
    
  
wash_init %>% { ifelse( "WA" == "foo", filter(county_code==53033), filter(county_code==53033))}

wash_init %>% filter(if (TRUE == TRUE) {TRUE} else {county_code==53033})













