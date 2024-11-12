library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(moments)
library(purrr)
library(gridExtra)


#carichiamo il dataset in R
Sentiment_fr_tweet_2023 <- read_csv2("C:/Users/rosar/Desktop/SAD/Sentiment_fr_tweet_2023.csv")

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
create_comparison_plot("following", "histogram")
create_comparison_plot("following", "pareto", c(3000, 1800, 500)) # c(bin dataset con outliers, bin dataset senza i grandi, bin dataset senza tutti)
create_comparison_plot("following", "qqplot")



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

