data <- read_lines("data/lar_data_fields.txt")

length(data)

data[700]

lars_names <- c()
for (i in 1:length(data)){
  if (grepl( "^1 - ", data[i],)) {
    lars_names <- c(lars_names, data[i - 3])
    j <- 1
    options <- c(data[i])
    while (grepl( " - ", data[i + j],)) {
      options <- c(options, data[i + j])
      j <- j + 1
    }
    print(options)
  }
}

data[700:710]

as.character(data[700])

tmp <- data[[700]][[0]]
tmp

glimpse(data)

data[700:800]



splitted <- str_split(options[1], " - ")
splitted[[1]][2]
