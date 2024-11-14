library(readr)
library(dplyr)      
library(corrplot)
library(dplyr)
library(ggplot2)
library(factoextra)

#carichiamo il dataset in R
dataset <- read_csv2("C:/Users/Utente/Documenti/Dataset.csv")

# Selezioniamo le variabili quantitative per l'analisi
quantitative_vars <- dataset %>%
  select(followers, following, totaltweets, retweetcount, favorite_count)

# Calcoliamo la matrice di correlazione
cor_matrix <- cor(quantitative_vars, use = "complete.obs")

# Stampiamo la matrice di correlazione
print(cor_matrix)


# Creiamo la heatmap della matrice di correlazione
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


#+1 indica una correlazione perfetta positiva
#-1 indica una correlazione perfetta negativa (una variabile aumenta mentre l'altra diminuisce).
#0 indica nessuna correlazione
#La maggior parte delle correlazioni tra le variabili sono 
#molto basse (vicine a 0), il che indica che queste variabili 
#sono quasi indipendenti tra loro
#C'è una correlazione positiva moderata di 0.27 tra following e 
#totaltweets. Questo significa che gli utenti che seguono più 
#persone tendono a fare un numero maggiore di tweet, anche se 
#la correlazione non è molto forte.

# Calcoliamo la media di retweetcount e favorite_count per ciascun 
#tipo di sentiment
mean_values <- dataset %>%
  group_by(sentiment) %>%
  summarise(mean_retweetcount = mean(retweetcount, na.rm = TRUE),
            mean_favorite_count = mean(favorite_count, na.rm = TRUE))

# Stampa delle medie
print(mean_values)

# Boxplot per visualizzare la distribuzione di retweetcount per ciascun sentiment
ggplot(dataset, aes(x = sentiment, y = retweetcount, fill = sentiment)) +
  geom_boxplot() +
  labs(title = "Distribuzione dei retweet per ciascun sentiment",
       x = "Sentiment", y = "Retweet Count") +
  theme_minimal()

# Boxplot per visualizzare la distribuzione di favorite_count per ciascun sentiment
ggplot(dataset, aes(x = sentiment, y = favorite_count, fill = sentiment)) +
  geom_boxplot() +
  labs(title = "Distribuzione dei like per ciascun sentiment",
       x = "Sentiment", y = "Favorite Count") +
  theme_minimal()

#Osservazione:
#il numero di retweet ha molti outlier per ciascuna categoria, 
#indicando che ci sono tweet che ottengono molti più retweet 
#rispetto alla maggior parte degli altri.
#La maggior parte dei dati si concentra attorno a valori bassi 
#di retweet per tutte e tre le categorie (tra 0 e 100), 
#ma ci sono alcuni tweet che superano i 2000, 4000, e anche 
#6000 retweet.
#La mediana del numero di retweet è abbastanza bassa in tutte 
#le categorie, suggerendo che i tweet con molti retweet sono 
#eventi rari.

# Conversione delle colonne in formato temporale
dataset$created_at <- as.POSIXct(dataset$tweetcreatedts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dataset$user_created_at <- as.POSIXct(dataset$usercreatedts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Creare una colonna con la data (senza ora)
dataset$date <- as.Date(dataset$created_at)

# Contare il numero di tweet per giorno
tweets_per_day <- dataset %>%
  group_by(date) %>%
  summarise(tweet_count = n())

# Visualizzare la frequenza dei tweet nel tempo
ggplot(tweets_per_day, aes(x = date, y = tweet_count)) +
  geom_line() +
  labs(title = "Frequenza dei Tweet nel Tempo",
       x = "Data", y = "Numero di Tweet") +
  theme_minimal()

# Creare una colonna per il numero di follower
dataset$followers_day <- as.Date(dataset$created_at)

# Raggruppare per utente e data per analizzare l'evoluzione 
#dei follower
followers_over_time <- dataset %>%
  group_by(userid, followers_day) %>%
  summarise(follower_count = max(followers, na.rm = TRUE))

# Visualizzare l'evoluzione dei follower nel tempo per un 
#utente specifico
# Raggruppare per userid e contare i tweet per ogni utente
tweet_counts <- dataset %>%
  group_by(userid) %>%
  summarise(tweet_count = n()) %>%
  arrange(desc(tweet_count))  # Ordinare in ordine decrescente

# Estrai l'user_id dell'utente con il maggior numero di tweet
user_with_most_tweets <- tweet_counts[1, "userid"]

# Stampa l'user_id
print(user_with_most_tweets)

# Ora prova a filtrare per l'user_id "1.59e18" come stringa
ggplot(followers_over_time %>% filter(userid == 2,21E+09), aes(x = followers_day, y = follower_count)) +
  geom_line() +
  labs(title = "Evoluzione dei Follower nel Tempo",
       x = "Data", y = "Numero di Follower") +
  theme_minimal()


# Calcolare il z-score per identificare i picchi
tweets_per_day$z_score <- (tweets_per_day$tweet_count - mean(tweets_per_day$tweet_count)) / sd(tweets_per_day$tweet_count)

# Identificare i picchi (ad esempio, dove il z-score è maggiore di 2)
peaks <- tweets_per_day %>%
  filter(z_score > 2)

# Visualizzare i picchi nel grafico
ggplot(tweets_per_day, aes(x = date, y = tweet_count)) +
  geom_line() +
  geom_point(data = peaks, aes(x = date, y = tweet_count), color = "red", size = 3) +
  labs(title = "Frequenza dei Tweet con Picchi Rilevanti",
       x = "Data", y = "Numero di Tweet") +
  theme_minimal()
#fine


# Selezione delle colonne pertinenti
dataset_filtered <- dataset %>%
  select(retweetcount, favorite_count, sentiment)

# Conversione della variabile sentiment in fattore e poi in numerico
dataset_filtered$sentiment <- as.numeric(factor(dataset_filtered$sentiment))

# Standardizzazione dei dati
dataset_scaled <- scale(dataset_filtered)

# Determinazione del numero ottimale di cluster usando il metodo del gomito
set.seed(42)
wss <- sapply(1:10, function(k){
  kmeans(dataset_scaled, centers = k, nstart = 25)$tot.withinss
})

# Visualizzazione del metodo del gomito
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Numero di Cluster K",
     ylab = "Somma delle distanze al quadrato (WSS)",
     main = "Metodo del Gomito per determinare il numero ottimale di cluster")

# Esecuzione di k-means con un numero ottimale di cluster (ad esempio, 3)
set.seed(42)
kmeans_result <- kmeans(dataset_scaled, centers = 3, nstart = 25)

# Aggiunta delle informazioni di clustering al dataset originale
dataset_filtered$cluster <- as.factor(kmeans_result$cluster)

# Visualizzazione dei cluster
fviz_cluster(kmeans_result, data = dataset_scaled,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#Osservazione:
#La maggior parte dei dati sembra essere concentrata vicino 
#all'origine, mentre un punto del cluster 3 (quadrato grigio) 
#è notevolmente distante dagli altri, probabilmente un outlier 
#o un'anomalia nei dati.
#Le due dimensioni (Dim1 e Dim2) spiegano rispettivamente 
#il 33.8% e il 33.3% della varianza, quindi insieme 
#rappresentano circa il 67% della varianza totale. 
#Ciò significa che i dati sono parzialmente ben rappresentati 
#in questo spazio bidimensionale, ma ci potrebbero essere altre 
#caratteristiche rilevanti non visualizzate in questo grafico.
# Visualizza la struttura del dataset
# Verifica che ci siano dati per l'user_id "1.59e18"


