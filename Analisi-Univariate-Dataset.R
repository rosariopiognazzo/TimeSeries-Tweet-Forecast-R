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
dataset <- dataset %>%
  mutate(is_retweet = as.factor(is_retweet),
         is_quote_status = as.factor(is_quote_status),
         sentiment = as.factor(sentiment),
         score = as.numeric(score))

write_csv2(dataset, "dataset.csv")#salvato nuovo dataset

## questa conversione dobbiamo capire se farla a priori o solo quando usiamo le istanze della variabile
# sostituisco le virgolette singole con doppie virgolette
dataset$hashtags <- gsub("'", '"', dataset$hashtags)
# sostituisco stringhe vuote con "[]" per evitare errori di parsing
dataset$hashtags[dataset$hashtags == ""] <- "[]"
# convertirto ogni elemento in JSON per poter accederci in maniera più immediata
dataset$hashtags <- lapply(dataset$hashtags, fromJSON)








