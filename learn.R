library(tidyverse)

data <- read.csv("data/state_WA.csv")

count(data, lei)

# 'https://ffiec.cfpb.gov/v2/data-browser-api/view/filers?states=WA&years=2020'

library("rjson")
result <- fromJSON(file ="data/lei_name.json")

json_data_frame <- as.data.frame(fromJSON(file ="data/lei_name.json"))
lei_list <- as.list(fromJSON(file ="data/lei_name.json"))

lei_num <- "549300ILXET2VTO1HD84"

one_instit <- filter(data, lei == lei_num)

data %>% filter(lei == lei_num) %>% select(action_taken)

lei_list <- as.list(json_data_frame)

tmp <- filter(lei_tib, lei == lei_num) 
tmp$name 

lei_nums <- lei_list[seq(1, length(lei_list) - 1, 4)]
lei_names <- lei_list[seq(2, length(lei_list), 4)]
lei_tib <- tibble(lei = unlist(unname(lei_nums)), name = unlist(unname(lei_names)))

lei_tib

lei_tib %>% slice(3)

tmp <- lei_list[seq(1, length(lei_list) - 1, 4)]
unlist(unname(tmp))


library(httr)
library(tidyverse)

url <- 'https://ffiec.cfpb.gov/v2/data-browser-api/view/filers?states=WA&years=2020'
response <- GET(url) 
lei <- content(response, type = 'text') %>% 
  jsonlite::fromJSON() %>% 
  .[[1]] %>%                      # Select the first element of the resulting list
  as_tibble()
lei

