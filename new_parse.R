library(tidyverse)

data <- read_lines("data/lar_data_fields.txt")
lars_names <- c()
options <- c()
for (i in 1:length(data)) {
  if (grepl("Values:", data[i],)) {
    # Ignore this type, will have to be handled differently
    if (grepl("Varying values$", data[i+1],)) {
      next
    }
    # This is a code lookup
    if (grepl("^[01] - ", data[i+1],)) {
        # print(data[i - 2])
        j <- 1
        while (grepl(" - ", data[i + j],)) {
          options <- c(options, data[i + j])
          lars_names <- c(lars_names, data[i - 2])
          j <- j + 1
        }
        i <- j
        next
    }
    # These are values as is
    parameter_name <- data[i - 2]
    while (!grepl("Description", data[i+2],)) {
      # print(i + 1)
      # print(data[i + 1])
      options <- c(options, parameter_name)
      lars_names <- c(lars_names, data[i - 2])
      i <- i + 1
      test_var <- TRUE
      option_bot <- i - 1
    }
    print(c(parameter_name, option_bot))
    break
  # These are binned values
  } else if (grepl("binned", data[i+1],)) {
    next
  }
}
    
lars_tib <- tibble(lars_names = lars_names, options = options)
lars_tib <- lars_tib %>%
  separate(options, c("key", "value"), " - ", extra = "merge")
write_csv(lars_tib, "data/lars_lookup.csv")












