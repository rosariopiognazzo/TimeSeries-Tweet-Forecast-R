library(readr)
library(httr)
library(jsonlite)
library(dplyr)

twitter_bearer <- "****RpggOR"
botometer_rapidapi_key <- "61d0af68b5msh1728bc86f51c723p1810f9jsn6ec4d80841bc"

dataset <- read_csv2("C:/Users/Utente/Documenti/Dataset.csv")


check_bot <- function(user_id) {
  url <- "https://botometer-pro.p.rapidapi.com/botometer-x/get_botscores_in_batch"
  
  response <- POST(url,
                   add_headers(
                     "X-RapidAPI-Key" = botometer_rapidapi_key,
                     "Authorization" = paste("Bearer", twitter_bearer),
                     "Content-Type" = "application/json"
                   ),
                   body = toJSON(list(user_id = user_id), auto_unbox = TRUE)
  )
  
  # Parse JSON response
  response_content <- fromJSON(content(response, as = "text"))
  return(response_content$scores$overall) # Ritorna il punteggio
}

# Applica la funzione a ogni user_id e aggiungi una nuova colonna con i punteggi
dataset <- dataset[1:5,] %>%
  mutate(bot_score = sapply(userid, check_bot))
