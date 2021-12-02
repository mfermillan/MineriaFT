text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

#construir el DF

library(dplyr)
text_df <- tibble(line = 1:4, text = text)

text_df

#tokenizar

library(tidytext)

text_df %>%
  unnest_tokens(word, text)

