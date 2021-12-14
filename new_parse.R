library(tidyverse)

data <- read_lines("data/lar_data_fields.txt")
lars_names <- c()
options <- c()
for (i in 1:length(data)) {
  if (grepl("Values:", data[i],) ||
      grepl("binned", data[i],)) {
    # Ignore this type, will have to be handled differently
    if (grepl("Varying values$", data[i+1],)) {
      next
    }
    print(data[i - 2])
    lars_name <- data[i - 2]
    i <- i + 1
    # This is a code lookup
    if (grepl("^[01] - ", data[i],)) {
        while (grepl(" - ", data[i],)) {
          lars_names <- c(lars_names, lars_name)
          options <- c(options, data[i])
          i <- i + 1
        }
        next
    }
    # These are values as is
    while (!grepl("Description", data[i+1],) && i < length(data)) {
      lars_names <- c(lars_names, lars_name)
      options <- c(options, paste(data[i], data[i], sep=" - "))
      i <- i + 1
    }
  }
}
    
lars_tib <- tibble(lars_names = lars_names, options = options)
lars_tib <- lars_tib %>%
  separate(options, c("key", "value"), " - ", extra = "merge")
write_csv(lars_tib, "data/lars_lookup.csv")












