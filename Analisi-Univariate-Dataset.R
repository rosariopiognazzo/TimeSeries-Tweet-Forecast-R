library(jsonlite)#per convertire hashtags
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

#leggiamo il dataset
Sentiment_fr_tweet_2023 <- read_csv2("Sentiment_fr_tweet_2023.csv")

#Non si opera mai sul dataset originale
dataset <- Sentiment_fr_tweet_2023

str(dataset)
#si nota che il formato degli elementi non è corretta per la maggior parte

#aggiustiamo il formato delle variabili
dataset$score <- as.numeric(dataset$score)
dataset$is_retweet <- as.factor(dataset$is_retweet)
dataset$is_quote_status <- as.factor(dataset$is_quote_status)
dataset$sentiment <- as.factor(dataset$sentiment)
# sostituisco le virgolette singole con doppie virgolette
dataset$hashtags <- gsub("'", '"', dataset$hashtags)
# sostituisco stringhe vuote con "[]" per evitare errori di parsing
dataset$hashtags[dataset$hashtags == ""] <- "[]"
# convertirto ogni elemento in JSON per poter accederci in maniera più immediata
dataset$hashtags <- lapply(dataset$hashtags, fromJSON)

write_csv2(dataset, "dataset.csv")#salvato nuovo dataset








