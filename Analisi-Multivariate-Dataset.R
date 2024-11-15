library(readr)
library(dplyr)      
library(corrplot)
library(dplyr)
library(ggplot2)
library(factoextra)
library(scatterplot3d)

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

# Contare il numero di tweet per giorno e per sentiment
tweets_per_day_sentiment <- dataset %>%
  group_by(date, sentiment) %>%
  summarise(tweet_count = n()) %>%
  ungroup()

# Calcolare la proporzione di ciascun sentiment per giorno
tweets_per_day_sentiment <- tweets_per_day_sentiment %>%
  group_by(date) %>%
  mutate(total_tweets = sum(tweet_count),
         proportion = tweet_count / total_tweets) %>%
  ungroup()

# Visualizzazione con un grafico a barre impilato per la proporzione di sentiment per giorno
ggplot(tweets_per_day_sentiment, aes(x = date, y = proportion, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Proporzione di tweet per tipo di sentiment nel tempo",
       x = "Data", y = "Proporzione") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#fine


# Seleziona solo le variabili di interesse per la correlazione
cor_data <- dataset[, c("followers", "following", "retweetcount", "favorite_count")]

# Calcola la matrice di correlazione
cor_matrix <- cor(cor_data, use = "complete.obs")
print(cor_matrix)


# Boxplot per retweet e sentiment
ggplot(dataset, aes(x = sentiment, y = retweetcount)) + 
  geom_boxplot() +
  labs(title = "Distribuzione dei Retweet per Sentiment", y = "Retweet Count")

# Trasformazione logaritmica delle variabili a scala elevata
dataset$log_followers <- log(dataset$followers)
dataset$log_following <- log(dataset$following)
dataset$log_totaltweets <- log(dataset$totaltweets)
dataset$log_retweetcount <- log(dataset$retweetcount)
dataset$log_favorite_count <- log(dataset$favorite_count)

dataset$log_followers[is.infinite(dataset$log_followers) & dataset$log_followers == -Inf] <- 0

dataset$log_following[is.infinite(dataset$log_following) & dataset$log_following == -Inf] <- 0

dataset$log_totaltweets[is.infinite(dataset$log_totaltweets) & dataset$log_totaltweets == -Inf] <- 0

dataset$log_retweetcount[is.infinite(dataset$log_retweetcount) & dataset$log_retweetcount == -Inf] <- 0

dataset$log_favorite_count[is.infinite(dataset$log_favorite_count) & dataset$log_favorite_count == -Inf] <- 0

# Boxplot per retweet e sentiment
ggplot(dataset, aes(x = sentiment, y = log_retweetcount)) + 
  geom_boxplot() +
  labs(title = "Distribuzione dei Retweet per Sentiment", y = "Retweet Count")



# Converte le date in formato Date
dataset$tweetcreatedts <- as.POSIXct(dataset$tweetcreatedts, format="%Y-%m-%dT%H:%M:%SZ")

# Raggruppa i dati per giorno e somma i retweet
daily_data <- dataset %>%
  mutate(date = as.Date(tweetcreatedts)) %>%
  group_by(date) %>%
  summarize(daily_retweets = sum(retweetcount, na.rm = TRUE))

# Grafico della serie temporale
ggplot(daily_data, aes(x = date, y = daily_retweets)) +
  geom_line() +
  labs(title = "Andamento dei Retweet nel Tempo", x = "Data", y = "Numero di Retweet")




# Seleziona le variabili numeriche per il clustering
data_cluster <- dataset[, c("followers", "following", "totaltweets")]

# Normalizza i dati (opzionale, ma consigliato)
data_cluster <- scale(data_cluster)

# Esegui il clustering K-means
set.seed(123)  # Per risultati ripetibili
kmeans_result <- kmeans(data_cluster, centers = 3)  # 3 è il numero di cluster che puoi scegliere

# Aggiungi i risultati del clustering al dataset
dataset$cluster <- kmeans_result$cluster


# Visualizzazione 3D con scatterplot3d
scatterplot3d(dataset$followers, dataset$following, dataset$totaltweets,
              color = dataset$cluster, pch = 16,  # Punti colorati in base ai cluster
              main = "Visualizzazione Cluster K-means 3D", 
              xlab = "Followers", ylab = "Following", zlab = "Total sweets")
#UTILE PER VEDERE SE KMEANS INDIVIDUA BENE LE TRE CLASSI NEU, POS, NEG oppure individua altri cluster
#NOTA: effetto outliers

# Calcola il K-means per un intervallo di numeri di cluster
wss <- sapply(1:10, function(k) {kmeans(data_cluster, centers = k, nstart = 25)$tot.withinss})

# Traccia il grafico dell'Elbow
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Numero di cluster", ylab = "Totale Within-cluster Sum of Squares")
## per decidere conviene usare non solo elbow method ma anche misure più quantitative come HC

# Seleziona le variabili numeriche per il clustering
data_cluster <- dataset[, c("followers", "following", "totaltweets")]

# Normalizza i dati (opzionale, ma consigliato)
data_cluster <- scale(data_cluster)
s
# Esegui il clustering K-means
set.seed(123)  # Per risultati ripetibili
kmeans_result <- kmeans(data_cluster, centers = 5)  

# Aggiungi i risultati del clustering al dataset
dataset$cluster <- kmeans_result$cluster

# Visualizzazione 3D con scatterplot3d
scatterplot3d(dataset$followers, dataset$following, dataset$totaltweets,
              color = dataset$cluster, pch = 16,  # Punti colorati in base ai cluster
              main = "Visualizzazione Cluster K-means 3D", 
              xlab = "Followers", ylab = "Following", zlab = "Total Tweets")

