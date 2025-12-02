## Special Condition Removals (URLs, Hyphens, Special Characters)
removeURL <- function(x) gsub("(f|ht)tp(s?)://\\S+", "", x)
rmHyphens <- function(x) gsub("-", " ", x)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", "", x)


## Performs and Returns Cleaned Corpus (Not Stemmed)
## For Data Frame Only
clean.corpus.df <- function(corpus){
  corpus <- tm_map(corpus, removeURL)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, rmHyphens)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeSpecialChars)
  return(corpus)
}


## Performs and Returns Cleaned Corpus (Stemmed)
## For Data Frame Only
clean.corpus.df.stemmed <- function(corpus){
  corpus <- tm_map(corpus, removeURL)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, rmHyphens)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeSpecialChars)
  corpus <- tm_map(corpus, stemDocument, language = "english")
  return(corpus)
}


## Creates Term-Document Matrix from Cleaned Corpus and Returns a Sorted Frequency Data Frame
## For Data Frame Only
df.corpus <- function(corpus){
  tdm <- TermDocumentMatrix(corpus)
  tdm <- removeSparseTerms(tdm, 0.75)
  matrix <- as.matrix(tdm) 
  words <- sort(rowSums(matrix), decreasing = TRUE) 
  df <- data.frame(word = names(words), freq = words)
  return(df)
}


## Performs and Returns Cleaned Corpus 
## For Data Frame and Dictionary
clean.corpus.dict <- function(corpus){
  corpus <- tm_map(corpus, removeURL)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, rmHyphens)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeSpecialChars)
  corpus_dictionary <- strsplit(corpus$content, " +")[[1]]
  corpus_dictionary_all <- corpus$content
  corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus_stemmed_all <- corpus$content
  return(list(corpus = corpus, dictionary = corpus_dictionary, stemmed_words = corpus_stemmed_all, dictionary_all = corpus_dictionary_all))
}


## Creates Term-Document Matrix from Cleaned Corpus and Returns a Sorted Frequency Data Frame
## For Data Frame and Dictionary
dict.corpus <- function(corpus, dictionary){
  tdm <- TermDocumentMatrix(corpus)
  tdm <- removeSparseTerms(tdm, 0.75)
  matrix <- as.matrix(tdm) 
  words <- sort(rowSums(matrix), decreasing = TRUE) 
  dict <- data.frame(word = names(words), freq = words)
  dict$word <- stemCompletion(x = dict$word, dictionary = dictionary)
  return(dict)
}
