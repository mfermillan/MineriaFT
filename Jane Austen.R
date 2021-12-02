#Paquetes a instalar

#install.packages("tidytext") # Provee funciones adicionales de mineria de textos
#install.packages("janeaustenr") # provee las novelas de Jane Austen
#install.packages("tidyverse") # manipulación de información y plotting

library(dplyr) # Manipulación de gramatica de datos. 
library(janeaustenr) # Novelas
library(tidytext) # stop words
library(ggplot2) # Crear graficos basados en datos

# cocntar el numero de veces que una palabra es usada en cada libro
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) #Contar valores unicos de estas 2 variables

# Calcular el total de las palabras por libro 
total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

# modificar el df con un leftjoin para conservar las palabras más usadas y el total de palabras (para poder graficar)
book_words <- left_join(book_words, total_words)

book_words

#term frequency: numero de veces que una palabra aparece en una novela divido entre el total de palabras


ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) + # histograma (superficie de cada barra es proporcional a la frecuencia de los valores)
  xlim(NA, 0.0009) + #delimitar el valor de x
  facet_wrap(~book, ncol = 2, scales = "free_y") #para agregar las gráficas de los diferentes libros



# la idea del tf-idf es encontrar las palabras importantes para el contenido de cada documentos decrementando el peso de las palabras más usadas 
#e incremendado el peso de las palabras menos usadas


book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n) #calcula y une el tf y el idf de una tidy text
book_tf_idf #palabras extremandamente comunes

book_tf_idf %>%
  select(-total) %>% #excluir el total
  arrange(desc(tf_idf))

library(forcats)

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>% #selecciona los maximos valores de la variable
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
