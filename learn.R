library(tidyverse)
library(httr)

data <- read.csv("data/state_WA.csv")

url <- 'https://ffiec.cfpb.gov/v2/data-browser-api/view/filers?states=WA&years=2020'
response <- GET(url) 
lei <- content(response, type = 'text') %>% 
  jsonlite::fromJSON() %>% 
  .[[1]] %>%                      # Select the first element of the resulting list
  as_tibble()


data_lei <- count(data, lei)
lei_names <- merge(tibble(lei = unique(data$lei)), select(lei, name, lei)) %>%
              arrange(name)

lei_names <- lei_names %>% mutate(name = replace(name, lei == "549300QSUEE20YO86W39", "Specialized Loan Servicing Llc")) 
lei_names <- lei_names %>% mutate(name = replace(name, lei == "549300UL36AJZ0WZ4U93", "Closing Mark Home Loans"))
lei_names <- lei_names %>% arrange(name)
write_csv(lei_names, "data/lei_name.csv")

colnames(data)

data$derived_loan_product_type

data %>% filter(aus.2 == 1111)

df <- tibble(`a 1` = 1, `a 2` = 2)

tibble('1' = "foo", '2' = "woo")
