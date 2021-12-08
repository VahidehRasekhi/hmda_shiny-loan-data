library(tidyverse)

data <- read.csv("data/state_WA.csv")

count(data, lei)

# 'https://ffiec.cfpb.gov/v2/data-browser-api/view/filers?states=WA&years=2020'

library("rjson")
result <- fromJSON(file ="data/lei_name.json")
json_data_frame <- as.data.frame(result)
