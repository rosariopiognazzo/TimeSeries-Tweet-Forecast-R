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



