library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(moments)
library(purrr)
library(gridExtra)
library(ggpubr)
library(nortest)

#carichiamo il dataset in R
Sentiment_fr_tweet_2023 <- read_csv2("C:/Users/rosar/Desktop/UNISA/Magistrale - Informatica/SAD/Sentiment_fr_tweet_2023.csv")
Sentiment_fr_tweet_2023 <- read_csv2("C:/Users/rosar/Desktop/SAD/Sentiment_fr_tweet_2023.csv")
Sentiment_fr_tweet_2023 <- read_csv2("C:/Users/Utente/Documenti/Sentiment_fr_tweet_2023.csv")

dataset <- read_csv2("C:/Users/Utente/Documenti/Dataset.csv")

#non si lavora mai sul dataset raw, ma su una copia
dataset <- Sentiment_fr_tweet_2023

#correggiamo il formato delle variabili
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

write_csv2(dataset, "C:/Users/rosar/Desktop/UNISA/Magistrale - Informatica/SAD/Dataset.csv")

#salviamoci le variabili numeriche
var_numeriche <- dataset %>%
  select_if(is.numeric)

#salviamoci le stringhe
var_char <- dataset %>%
  select_if(is.character)

#salviamoci le variabili categoriali
var_fact <- dataset %>%
  select_if(is.factor)


## ------------------ ANALISI SULLE VARIABILI NUMERICHE -------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#osserviamo statistiche di base:
metriche <- var_numeriche %>%
  summarise(across(everything(), list(
    media = mean,
    mediana = median,
    sdev = sd,
    Q1_25 = ~ quantile(. , 0.25),
    Q3_75 = ~ quantile(. , 0.75),
    minimo = min,
    massimo = max,
    asimmetria = skewness,
    curtosi = kurtosis,
    mad = mad,
    tot = ~ sum(!is.na(.)),
    NAs = ~ sum(is.na(.))
  ), .names = "{.col}-{.fn}")) %>% # pivoting in formato lungo
  pivot_longer(
    cols = everything(),
    names_to = c("variabile", "metrica"),
    names_sep = "-",
    values_to = "valore"
  ) %>% # pivoting in formato largo con metriche come righe e variabili come colonne
  pivot_wider(
    names_from = variabile,
    values_from = valore
  )

metriche
###
#osservando i valori ottenuti si posso dedurre alcune caratteristiche presenti nei dati:
#
# - la MEDIA sia di followrs, following che totaltweets è molto più grande rispetto al valore MEDIANO, questo ci indica
# la presenza di utenti molto popolari, con molti tweet che attraggono la media verso di loro.
#
# - la DEVIAZIONE STANDARD e il MAD sono entrambe molto grandi per le variabili followers, following  e totaltweets 
# questo indica forte variabilità nei dati (un altro indizio sulla presenza di grandi outliers)
#
# - ASIMMETRIA positiva molto forte nelle variabili followers e favourite_count indica che la maggior parte degli
# utenti ha pochi followers e pochi like, ma ci sono forti outliers che spingono i valori verso destra
#
# - CURTOSI infatti è molto alta in quelle variabili, il che indica code pesanti quindi presenza di valori estremamente
# alti e fuori dalla media
###
# Applichiamo la trasformazione logaritmica a tutte le variabili numeriche
dataset$log_followers <- log(ifelse(dataset$followers > 0, dataset$followers, NA))
dataset$log_following <- log(ifelse(dataset$following > 0, dataset$following, NA))
dataset$log_totaltweets <- log(ifelse(dataset$totaltweets > 0, dataset$totaltweets, NA))
dataset$log_retweetcount <- log(ifelse(dataset$retweetcount > 0, dataset$retweetcount, NA))
dataset$log_favorite_count <- log(ifelse(dataset$favorite_count > 0, dataset$favorite_count, NA))


# Sostituiamo var_numeriche con le nuove variabili logaritmiche
# Selezioniamo solo le variabili logaritmiche
var_numeriche_log <- dataset %>%
  select(log_followers, log_following, log_totaltweets, log_retweetcount, log_favorite_count)

# Iteriamo su ogni variabile numerica per plottare i grafici
for (col in names(var_numeriche_log)) {
  
  # Creiamo un dataframe temporaneo con la variabile corrente
  data <- var_numeriche_log %>%
    select(all_of(col)) %>%
    drop_na()  # Rimuoviamo eventuali NA
  
  # Nome della variabile corrente
  col_name <- colnames(data)[1]
  
  # Calcoliamo la media e mediana della variabile
  media <- mean(data[[col_name]])
  mediana <- median(data[[col_name]])
  
  # Grafico 1: Istogramma con densità
  hist_plot <- ggplot(data, aes_string(x = col_name)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.6) +
    geom_density(color = "red", size = 1) +
    geom_vline(aes(xintercept = media), color = "blue", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mediana), color = "darkgreen", linetype = "dotted", size = 1) +
    labs(title = paste("Distribuzione di", col_name),
         x = col_name, y = "Densità") +
    theme_minimal()
# L'istogramma mostra che la variabile non segue una distribuzione normale, 
#  ma è fortemente asimmetrica verso sinistra (positiva). 
#  Questo significa che la maggior parte dei 
#   valori è concentrata in un range basso di log_favorite_count, mentre 
#   ci sono pochi valori estremamente alti.
# #Media e Mediana:
# #La media è rappresentata dalla linea tratteggiata blu ed è maggiore 
#   rispetto alla mediana (linea tratteggiata verde). Questo è un segnale 
#   tipico di distribuzioni con una coda lunga verso destra, dove i valori 
#   elevati influiscono sulla media.
  
  # Grafico 2: Boxplot
  box_plot <- ggplot(data, aes_string(y = col_name)) +
    geom_boxplot(fill = "orange", color = "black", outlier.color = "red", outlier.size = 3) +
    labs(title = paste("Boxplot di", col_name),
         y = col_name) +
    theme_minimal()
  # Outlier: Dal boxplot si nota la presenza di molti outlier sopra la 
  # soglia massima (rappresentati dai punti rossi). Questi outlier indicano 
  # utenti con valori di log_favorite_count molto elevati rispetto alla 
  # maggior parte degli altri.
  # I Quartili: La lunghezza della scatola (distanza tra il primo e il 
  # terzo quartile) evidenzia una forte variabilità nei valori centrali.
  # Gli outlier possono essere analizzati più in dettaglio per comprendere 
  # le caratteristiche degli utenti che generano un numero significativamente 
  # alto di "favorite".
  
  # Test di normalità alternativo: Anderson-Darling
  if (nrow(data) > 5000) {
    sample_data <- data[[col_name]][1:5000]  # Sottocampioniamo le prime 5000 osservazioni
    ad_test <- ad.test(sample_data)
  } else {
    ad_test <- ad.test(data[[col_name]])
  }
  
  normality_p_value <- round(ad_test$p.value, 4)
  normality_result <- ifelse(normality_p_value < 0.05, "Non Normale", "Normale")
  
  # Stampiamo il risultato del test
  cat("\nVariabile:", col_name, 
      "\nP-value Test di Anderson-Darling:", normality_p_value,
      "\nDistribuzione:", normality_result, "\n\n")
  
  # QQ-plot per la normalità
  qq_plot <- ggplot(data, aes(sample = data[[col_name]])) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste("QQ-Plot di", col_name),
         x = "Teorico Quantile", y = "Quantile Osservato") +
    theme_minimal()
  
  # Il QQ-plot conferma che la variabile non segue una distribuzione normale. 
  # La maggior parte dei punti si discosta dalla linea rossa, soprattutto 
  # nelle code.
  # I valori estremi (coda destra) sono molto lontani dalla linea teorica, 
  # evidenziando che i dati sono pesantemente influenzati da pochi valori 
  # molto alti.
  # Questa conferma è anche coerente con il test di Anderson-Darling che 
  # abbiamo eseguito, il quale probabilmente ha restituito un p-value < 0.05, 
  # indicando che i dati non sono normali.
  
  
  # Combinazione dei grafici
  combined_plot <- grid.arrange(hist_plot, box_plot, qq_plot, ncol = 3)
  
  # Pausa tra i grafici
  readline(prompt = "Premi [Invio] per continuare...")
}


###--------------------------------------------------------------------------------------------------------------------------------------------## 
# Funzione per creare e visualizzare il confronto fra plot di una variabile
#
create_comparison_plot <- function(var_name, plot_type, bins=NULL) {
  
  # Estraiamo i quartili e IQR dal dataset metriche
  Q1 <- metriche %>% filter(metrica == "Q1_25") %>% pull(!!sym(var_name))
  Q3 <- metriche %>% filter(metrica == "Q3_75") %>% pull(!!sym(var_name))
  
  
  # Calcolo IQR e i limiti perdefinire gli outlier (k=1.5 tutti, k=3 solo i grandi)
  IQR_value <- Q3 - Q1
  lower_limit_standard <- Q1 - 1.5 * IQR_value
  upper_limit_standard <- Q3 + 1.5 * IQR_value
  lower_limit_large <- Q1 - 3 * IQR_value
  upper_limit_large <- Q3 + 3 * IQR_value
  
  # creiamo i vari dataset (con outliers, senza i grandi, senza tutti)
  data_with_outliers <- var_numeriche %>%
    select(all_of(var_name)) %>%
    mutate(type = "Con Outliers")
  
  data_no_large_outliers <- var_numeriche %>%
    filter((!!sym(var_name)) >= lower_limit_large & (!!sym(var_name)) <= upper_limit_large) %>%
    select(all_of(var_name)) %>%
    mutate(type = "Senza Grandi Outliers")
  
  data_no_outliers <- var_numeriche %>%
    filter((!!sym(var_name)) >= lower_limit_standard & (!!sym(var_name)) <= upper_limit_standard) %>%
    select(all_of(var_name)) %>%
    mutate(type = "Senza Outliers")
  
  # definiamo i vari tipi di grafici
  if (plot_type == "boxplot") {
    plot_with_outliers <- ggplot(data_with_outliers, aes(y = .data[[var_name]], x = type)) +
      geom_boxplot(fill = "pink") + #definisci il boxplot
      geom_violin(fill="pink", alpha = .2)+ #aggiunto violin per far vedere la distribuzione
      labs(x = "", y = var_name) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous() #serve per avere scale diverse altrimenti non si legge nulla
    
    plot_no_large_outliers <- ggplot(data_no_large_outliers, aes(x = type, y = .data[[var_name]])) +
      geom_boxplot(fill = "skyblue") +
      geom_violin(fill="skyblue", alpha = .2)+
      labs(x = "", y = var_name) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous()
    
    plot_no_outliers <- ggplot(data_no_outliers, aes(x = type, y = .data[[var_name]])) +
      geom_boxplot(fill = "lightgreen") +
      geom_violin(fill="lightgreen", alpha = .2)+
      labs(x = "", y = var_name) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous()
    
    grid.arrange(plot_with_outliers, plot_no_large_outliers, plot_no_outliers, ncol = 3, 
                 top=paste("BOXPLOTS - ",var_name)) #si mettono insieme in un unico grafico
    
  } else if (plot_type == "histogram") {
    # Calcolo la media e deviazione standard per ciascun sottoinsieme, poiché per visualizzare poi
    # una distribuzione normale su questi dati ha bisogno di media e sd
    mean_with_outliers <- mean(data_with_outliers[[var_name]], na.rm = TRUE)
    sd_with_outliers <- sd(data_with_outliers[[var_name]], na.rm = TRUE)
    
    mean_no_large_outliers <- mean(data_no_large_outliers[[var_name]], na.rm = TRUE)
    sd_no_large_outliers <- sd(data_no_large_outliers[[var_name]], na.rm = TRUE)
    
    mean_no_outliers <- mean(data_no_outliers[[var_name]], na.rm = TRUE)
    sd_no_outliers <- sd(data_no_outliers[[var_name]], na.rm = TRUE)
    
    plot_with_outliers <- ggplot(data_with_outliers, aes(x = .data[[var_name]])) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "pink", color = "black", alpha = 0.7) + # definsico l'istogramma
      stat_function(fun = dnorm, args = list(mean = mean_with_outliers, sd = sd_with_outliers), color = "red", size = 1) + # aggiungo la normale per far vedere che i dati non si distribuiscono come una normale
      ggtitle(paste("Con Outliers")) +
      theme_minimal() +
      labs(x = var_name, y = "Densità")
    
    plot_no_large_outliers <- ggplot(data_no_large_outliers, aes(x = .data[[var_name]])) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
      stat_function(fun = dnorm, args = list(mean = mean_no_large_outliers, sd = sd_no_large_outliers), color = "red", size = 1) +
      ggtitle(paste("Senza Grandi Outliers")) +
      theme_minimal() +
      labs(x = var_name, y = "Densità")
    
    plot_no_outliers <- ggplot(data_no_outliers, aes(x = .data[[var_name]])) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
      stat_function(fun = dnorm, args = list(mean = mean_no_outliers, sd = sd_no_outliers), color = "red", size = 1) +
      ggtitle(paste("Senza Outliers")) +
      theme_minimal() +
      labs(x = var_name, y = "Densità")
    
    # Combino i grafici 
    combined_plot <- plot_grid(
      plot_with_outliers, plot_no_large_outliers, plot_no_outliers,
      ncol = 3
    )
    
    print(combined_plot)
    
  } else if (plot_type == "pareto") {
    # Funzione per creare il diagramma di Pareto per ciascun dataset
    create_pareto_plot <- function(data, title, interv, col) {
      data <- data %>%
        mutate(bin = cut(.data[[var_name]], breaks = seq(0, max(.data[[var_name]], na.rm = TRUE), by = interv), right = FALSE)) %>% 
        #i bin servono a contare quanti valori cadono in intervalli che definiamo noi, si definiscono nel momento che si chiama la funzione
        count(bin) %>%
        arrange(desc(n)) %>% #bisogna ordinarli in modo decrescente
        mutate(proporzione = n / sum(n),
               cumulativa = cumsum(proporzione)) #si calcola la frequenza comulativa 
      
      # Grafico di Pareto con barre e linea cumulativa
      ggplot(data, aes(x = reorder(as.factor(bin), -n), y = proporzione)) +
        geom_bar(stat = "identity", fill = col, color = "black", alpha = 0.7) +  # Barre per le frequenze
        geom_line(aes(x = bin, y = cumulativa), color = "red", size = 1, group = 1) +  # Linea cumulativa
        geom_point(aes(x = bin, y = cumulativa), color = "red", size = 2) +  # Punti sulla linea cumulativa
        labs(x = paste("Bin di", var_name), y = "Proporzione", title = title) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    # Creazione dei grafici di Pareto per i tre casi
    plot_with_outliers <- create_pareto_plot(data_with_outliers,
                                             paste("Con Outliers"),
                                             interv = bins[1], col = "pink")
    plot_no_large_outliers <- create_pareto_plot(data_no_large_outliers,
                                                 paste("Senza Grandi Outliers"),
                                                 interv = bins[2], col = "skyblue")
    plot_no_outliers <- create_pareto_plot(data_no_outliers,
                                           paste("Senza Outliers"),
                                           interv = bins[3], col = "lightgreen")
    
    # Combino i grafici in una visualizzazione a griglia
    combined_plot <- plot_grid(
      
      plot_with_outliers, plot_no_large_outliers, plot_no_outliers,
      ncol = 3
    )
    
    print(combined_plot)
    
  } else if (plot_type == "qqplot") { #serve ulteriormente per far vedere che non si distribuisce come una normale
    # Creazione dei Q-Q plot
    plot_with_outliers <- ggplot(data_with_outliers, aes(sample = .data[[var_name]])) +
      geom_qq() +
      geom_qq_line(color = "pink") +
      ggtitle(paste("Con Outliers")) +
      theme_minimal()
    
    plot_no_large_outliers <- ggplot(data_no_large_outliers, aes(sample = .data[[var_name]])) +
      geom_qq() +
      geom_qq_line(color = "skyblue") +
      ggtitle(paste("Senza Grandi Outliers")) +
      theme_minimal()
    
    plot_no_outliers <- ggplot(data_no_outliers, aes(sample = .data[[var_name]])) +
      geom_qq() +
      geom_qq_line(color = "lightgreen") +
      ggtitle(paste("Senza Outliers")) +
      theme_minimal()
    
    grid.arrange(plot_with_outliers, plot_no_large_outliers, plot_no_outliers, ncol = 3,
                 top = paste("QQPLOTS - ",var_name))
    
    } else {
    stop("Tipo di grafico non valido. Scegli tra 'boxplot', 'histogram', 'barplot', 'qqplot', 'boxstrip'.")
  }
}
#
##---------------------------------------------------------------------------------------------------------------------------------------------------------------------##

# Esempio di utilizzo: Variabile FOLLOWING
create_comparison_plot("following", "boxplot")
# Grafico 1: "Con Outliers"
# Cosa vediamo:
#   La variabile following mostra un numero elevato di outlier estremi, rappresentati dai punti oltre i baffi del boxplot.
# La maggior parte dei dati si trova in un range molto ristretto, vicino alla parte inferiore del grafico. La coda lunga superiore rende difficile distinguere i valori centrali (i.e., la scatola stessa del boxplot).
# Implicazioni:
#   I dati contengono molti valori estremi molto grandi, il che potrebbe indicare utenti che seguono un numero eccezionalmente elevato di altre persone. Questi valori estremi influenzano fortemente la visualizzazione e il calcolo di alcune statistiche (e.g., media, deviazione standard).
# 2. Grafico 2: "Senza Grandi Outliers"
# Cosa vediamo:
#   Qui sono stati rimossi gli outlier molto grandi (definiti con un criterio di soglia basato su 3 volte l'IQR - Interquartile Range).
# La scatola del boxplot è più visibile, e possiamo osservare meglio i valori centrali della distribuzione. Tuttavia, ci sono ancora alcuni outlier (più piccoli), che sono stati inclusi.
# Implicazioni:
# Rimuovendo solo gli outlier estremi, possiamo concentrarci sulla distribuzione centrale dei dati. Questo grafico fornisce una rappresentazione più equilibrata, che aiuta ad analizzare la variabilità dei valori che non sono fortemente influenzati dagli estremi.
# 3. Grafico 3: "Senza Outliers"
# Cosa vediamo:
# Qui sono stati rimossi tutti gli outlier (secondo il criterio standard di 1.5 volte l'IQR).
# La distribuzione risulta molto più compatta, e la variabilità centrale dei dati è più chiara.
# La maggior parte dei valori si trova in un range relativamente ristretto.
# Implicazioni:
#   Questo grafico aiuta a comprendere meglio la parte "normale" della distribuzione dei dati, senza l'influenza degli outlier. È utile per analisi statistiche che richiedono di escludere valori anomali, come il calcolo della media e deviazione standard.

create_comparison_plot("following", "histogram")
create_comparison_plot("following", "pareto", c(3000, 1800, 500)) # c(bin dataset con outliers, bin dataset senza i grandi, bin dataset senza tutti)
create_comparison_plot("following", "qqplot")
# Grafico 1: "Con Outliers"
# Osservazioni:
#   I punti si discostano significativamente dalla linea diagonale verde (che rappresenta la distribuzione normale).
# Gli outlier estremi sono evidenti nella coda superiore, mostrando deviazioni notevoli rispetto alla normalità.
# Interpretazione:
#   I dati non seguono una distribuzione normale a causa della presenza di valori estremi che distorcono la forma generale.
# 2. Grafico 2: "Senza Grandi Outliers"
# Osservazioni:
#   Dopo aver rimosso gli outlier estremi (ma non tutti), la forma della distribuzione è più vicina alla normalità rispetto al primo grafico.
# Tuttavia, le code (sia inferiore che superiore) mostrano ancora deviazioni dalla linea teorica.
# Interpretazione:
#   Anche senza gli outlier più grandi, i dati non sono perfettamente normali. Ciò potrebbe indicare una distribuzione asimmetrica o con code più lunghe del normale (distribuzione heavy-tailed).
# 3. Grafico 3: "Senza Outliers"
# Osservazioni:  Quando tutti gli outlier sono rimossi (criterio standard di 1.5 volte l'IQR), i dati si avvicinano di più alla linea teorica.
# C'è comunque una leggera deviazione nelle code, ma la distribuzione appare più normale rispetto agli altri casi.
# Interpretazione:
# Dopo aver escluso tutti gli outlier, possiamo dire che i dati centrali seguono meglio una distribuzione normale. Tuttavia, non è una perfetta distribuzione normale.
                                         
create_comparison_plot("score", "boxplot")


dataset %>%
  filter(is_retweet == 1) %>%
  summarise(
    n_retweet = n()
  )


## analizzare come sottoinsieme i tweet che sono retweet di altri tweet, 
## e vedere come questi si distribuiscono nel dataset ossia cercare delle tendenze
## come i retweet neg, pos, neutri come aumentano nel corso del tempo?
## Cercare di dminuire la grandezza del dataset aggregando i valori: Contare se ci sono userID
## uguali che hanno tweettato o retweettato più volte e contare il numero di tweet pos, neg e neu e identificare quindi 
## quel profilo come NEU, POS, NEG rispetto alla guerra--> fare questo per ogni utente presente
## ci permette di categorizzare gli utenti e fare analisi su un dataset con questi nuovi oggetti.

