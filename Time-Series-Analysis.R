library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highfrequency)
library(xts)
library(forecast)
library(lubridate)

Sentiment_fr_tweet_2023 <- read_csv2("C:/Users/rosar/Desktop/UNISA/Magistrale - Informatica/SAD/Dataset.csv")
Sentiment_fr_tweet_2023 <- read_csv2("C:/Users/rosar/Desktop/SAD/Sentiment_fr_tweet_2023.csv")


dataset <- Sentiment_fr_tweet_2023
dataset <- dataset %>%
  mutate(userid = as.character(userid),
         following = as.numeric(following),
         followers = as.numeric(followers),
         totaltweets = as.numeric(totaltweets),
         tweetid = as.character(tweetid),
         retweetcount = as.numeric(retweetcount),
         favorite_count = as.numeric(favorite_count),
         is_retweet = as.factor(ifelse(is_retweet == FALSE, 0, 1)),
         is_quote_status = as.factor(ifelse(is_quote_status == FALSE, 0, 1)),
         sentiment = as.factor(sentiment),
         score = as.numeric(score))

# Preparazione dei dati
df_diff <- dataset %>%
  arrange(tweetcreatedts) %>%
  group_by(sentiment) %>%
  mutate(score_diff = score - lag(score)) %>%  # Calcolo della differenza
  select(tweetcreatedts, score_diff, sentiment) %>%
  ungroup()

# Creazione del grafico1
ggplot(df_diff, aes(x = tweetcreatedts, y = score_diff, color = sentiment)) +
  geom_line() +
  facet_wrap(~ sentiment, scales = "free_y") + # Grafici separati per ogni categoria
  labs(
    title = "Variazione dello Score nel Tempo per Categoria di Sentiment",
    x = "Data e Ora",
    y = "Variazione dello Score"
  ) +
  theme_minimal()

#creazione del grafico2
ggplot(dataset, aes(x = tweetcreatedts, y = score, color = sentiment)) +
  geom_line() +
  facet_wrap(~ sentiment, scales = "free_y") + # Grafici separati per ogni categoria
  labs(
    title = "Variazione dello Score nel Tempo per Categoria di Sentiment",
    x = "Data e Ora",
    y = "Score"
  ) +
  theme_minimal()
#
## osserviamo un altissima frequenza nei dati: PROBLEMA --> = alta è la frequenza maggiore è il noise
## inoltre per loro natura hanno una struttura non-omogenea (non sono ugualmente distanziati nel tempo)
## Trasformare da non omegenea ad omogena:
## - Previous-tick interpolation
## - Linear interpolation 
## (differenza è trascurabile per dati ad altissima frequenza)
#
# ----------------------------------------------------------------------#
### PLOT DELLE TIME SERIES DELLE DIFFERENZE DELLO SCORE PER SENTIMENT ###
# ----------------------------------------------------------------------#
# Funzione per creare il plot
create_ts_plot <- function(data, var_num, var_fact, var_temp, tmp, diff) {
  # dati ordinati
  data <- data %>%
    arrange(!!sym(var_temp))
  
  # Creazione della lista di serie temporali per ogni categoria della variabile categoriale
  series_list <- split(data, data[[var_fact]])
  
  # facciamo l'aggregazione omogenea per ogni categoria
  series_homogeneous <- lapply(series_list, function(group) {
    # Conversione in oggetto xts
    ts_data <- xts(group[[var_num]], order.by = group[[var_temp]])
    
    # Estrazione delle unità di tempo e del periodo
    align_by <- tmp[1] # Es. "hours", "minutes"
    align_period <- tmp[2] # Es. 1, 2, ecc.
    
    # Aggregazione
    aggregated_data <- aggregateTS(ts_data, alignBy = align_by, alignPeriod = as.integer(align_period))
    
    # Creazione di un dataframe con i dati aggregati
    data.frame(
      time = as.POSIXct(index(aggregated_data)),
      value = as.numeric(aggregated_data)
    )
  })
  
  # Combiniamo i dati aggregati
  df_homogeneous <- bind_rows(
    Map(function(data, category) {
      data$category <- category
      data
    }, series_homogeneous, names(series_homogeneous))
  )
  
  # Calcolo delle differenze, se richiesto
  if (diff) {
    df_homogeneous <- df_homogeneous %>%
      group_by(category) %>%
      arrange(time) %>%  # Riordina per sicurezza
      mutate(value = value - lag(value)) %>%  # Differenza rispetto al valore precedente
      ungroup()
  }
  
  # Creazione del grafico
  p <- ggplot(df_homogeneous, aes(x = time, y = value, color = category)) +
    geom_line() +
    facet_wrap(~ category, scales = "free_y") +
    labs(
      title = ifelse(diff, 
                     "Variazione della Variabile Numerica nel Tempo", 
                     "Andamento della Variabile Numerica nel Tempo"),
      x = "Tempo",
      y = ifelse(diff, "Variazione", "Valore")
    ) +
    theme_minimal()
  
  print(p)#plotto
  
  return(series_homogeneous)
}

# Eseguiamo la funzione
scoreTS_diff <- create_ts_plot(
  data = dataset, 
  var_num = "score", 
  var_fact = "sentiment", 
  var_temp = "tweetcreatedts", 
  tmp = c("hours", 10), 
  diff = TRUE
)

# Eseguiamo la funzione
scoreTS <- create_ts_plot(
  data = dataset, 
  var_num = "score", 
  var_fact = "sentiment", 
  var_temp = "tweetcreatedts", 
  tmp = c("minutes", 30), 
  diff = FALSE
)

# Eseguiamo la funzione
scoreTS_day <- create_ts_plot(
  data = dataset, 
  var_num = "score", 
  var_fact = "sentiment", 
  var_temp = "tweetcreatedts", 
  tmp = c("days", 1), 
  diff = FALSE
)

ts_list <- lapply(scoreTS, function(df)ts(data = df))
ts_list


# Output
ts_list <- as.data.frame(ts_list)

GGally::ggpairs(ts_list[c("neg.value", "pos.value", "neu.value")])
#non c'è dipendenza tra le serie storiche dello score


<<<<<<< HEAD

=======
## analisi dal libro
autoplot(ts_list$neg[, 2])

dfts <- cbind(ts_list$neg, ts_list$pos[,2])
autoplot(dfts[, c("ts_list$neg.value", "ts_list$pos[, 2]")], facets = TRUE)

dfts <- as.data.frame(dfts)
dfts %>%
  ggplot(aes(x = `ts_list$neg.value`, y=`ts_list$pos[, 2]` ))+
  geom_point()

gglagplot(ts_list$neg[,2])

ggAcf(ts_list$neg[,2])
>>>>>>> 20cd6838047c4e92b7a28245d6a4c82642aadcd0



