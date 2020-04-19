# funcao clean_corpus
clean_corpus <- function(corpus)
{
  corpus <- tm_map(corpus, stripWhitespace) # retira espacos em branco
  corpus <- tm_map(corpus, content_transformer(tolower)) # transforma em minusculas
  corpus <- tm_map(corpus, removePunctuation) # retira pontuacao
  corpus <- tm_map(corpus, removeNumbers) # retira numeros
  #corpus <- tm_map(corpus.tmp, removeWords, stopwords('english'))
  myStopwords <- c(stopwords('portuguese'), 'nao', 'pag', 'nº')
  corpus <- tm_map(corpus, removeWords, myStopwords)
  return(corpus)
}