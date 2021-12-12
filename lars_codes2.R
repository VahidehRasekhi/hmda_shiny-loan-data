library(tidyverse)

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
lars_tib <- lars_tib %>% separate(options, c("key", "value"), " - ", extra = "merge")
write_csv(lars_tib, "data/lars_lookup.csv")