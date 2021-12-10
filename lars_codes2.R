data <- read_lines("data/lar_data_fields.txt")
lars_names <- c()
options <- c()
for (i in 1:length(data)){
  if (grepl( "^1 - ", data[i],)) {
    print(data[i - 3])
    j <- 0
    # options <- c(data[i])
    while (grepl( " - ", data[i + j],)) {
      options <- c(options, data[i + j])
      lars_names <- c(lars_names, data[i - 3])
      
      # splitted <- str_split(options[1], " - ")
      # name <- c(name, splitted[[1]][1])
      # value <- c(value, splitted[[1]][2])
      j <- j + 1
    }
    # print(options)
    
  }
}
lars_tib <- tibble(lars_names = lars_names, options = options)

lars_tib %>% separate(options, c("key", "value"), " - ", extra = "merge")

write_csv(lars_tib, "data/lars_lookup.csv")













data[700:710]

as.character(data[700])

tmp <- data[[700]][[0]]
tmp

glimpse(data)

data[700:800]

('aus', (1, 2, 3), (yes no na))
('aus1', (1, 2, 3), (yes no na))
('aus2', (1, 2, 3), (yes no na))
('aus3', (1, 2, 3), (yes no na))
('aus4', (1, 2, 3), (yes no na))

splitted <- str_split(options[1], " - ")
splitted[[1]][2]
