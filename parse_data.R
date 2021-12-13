library(tidyverse)

data <- read.csv("data/state_WA.csv")
lars_names <- read.csv("data/lars_lookup.csv")

data$action_taken

colnames(data)

unique(data$derived_sex)

lars_names %>% filter(lars_names == "action_taken") %>% pull(value)

selected <- "File closed for incompleteness"
lars_names %>% 
  filter(lars_names == "action_taken", value == selected) %>% 
  pull(key)



###############################################################################
data <- read_lines("data/lar_data_fields.txt")
lars_names <- c()
options <- c()
for (i in 1:length(data)){
  if (grepl( "^1 - ", data[i],)) {
    print(data[i - 3])
    j <- 0
    while (grepl( " - ", data[i + j],)) {
      options <- c(options, data[i + j])
      lars_names <- c(lars_names, data[i - 3])
      j <- j + 1
    }
  }
}
lars_tib <- tibble(lars_names = lars_names, options = options)
lars_tib <- lars_tib %>% 
  separate(options, c("key", "value"), " - ", extra = "merge")
write_csv(lars_tib, "data/lars_lookup.csv")
###############################################################################

data <- read_lines("data/lar_data_fields.txt")
lars_names <- c()
options <- c()
for (i in 1:length(data)) {
  if (grepl("Values:", data[i],)) {
    # works every time except line 67 where 0 starts the option list
    if (grepl("^1 - ", data[i+1],)) {
      print(data[i - 2])
      j <- 1
      while (grepl(" - ", data[i + j],)) {
        options <- c(options, data[i + j])
        lars_names <- c(lars_names, data[i - 2])
        j <- j + 1
      }
    }
  }
}
lars_tib <- tibble(lars_names = lars_names, options = options)
lars_tib <- lars_tib %>% 
  separate(options, c("key", "value"), " - ", extra = "merge")
write_csv(lars_tib, "data/lars_lookup.csv")












