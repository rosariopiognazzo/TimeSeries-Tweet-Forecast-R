# library(readr)
# library(httr)
# library(jsonlite)
# library(dplyr)

# twitter_bearer <- "****RpggOR"
# botometer_rapidapi_key <- "61d0af68b5msh1728bc86f51c723p1810f9jsn6ec4d80841bc"
# 
# dataset <- read_csv2("C:/Users/Utente/Documenti/Dataset.csv")
# 
# 
# check_bot <- function(user_id) {
#   url <- "https://botometer-pro.p.rapidapi.com/botometer-x/get_botscores_in_batch"
#   
#   response <- POST(url,
#                    add_headers(
#                      "X-RapidAPI-Key" = botometer_rapidapi_key,
#                      "Authorization" = paste("Bearer", twitter_bearer),
#                      "Content-Type" = "application/json"
#                    ),
#                    body = toJSON(list(user_id = user_id), auto_unbox = TRUE)
#   )
#   
#   # Parse JSON response
#   response_content <- fromJSON(content(response, as = "text"))
#   return(response_content$scores$overall) # Ritorna il punteggio
# }
# 
# # Applica la funzione a ogni user_id e aggiungi una nuova colonna con i punteggi
# dataset <- dataset[1:5,] %>%
#   mutate(bot_score = sapply(userid, check_bot))  


install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)



# 1. Creare un Account su RapidAPI e Registrarsi al Servizio Botometer:
#   - Vai su RapidAPI e crea un account gratuito.
#   - Cerca il servizio Botometer Pro e abbonati al piano che desideri (il piano Basic è gratuito per iniziare).
#   - Copia la tua chiave API (API Key) dalla dashboard di RapidAPI.
#
# 2. Ottenere Credenziali Twitter
#   - Botometer richiede di accedere ai dati di un utente Twitter. Per farlo, devi creare un'app Twitter:
#   - Vai su Twitter Developer Portal e crea un'app.
#   - Ottieni le seguenti credenziali:
#     - API Key
#     - API Secret Key
#     - Access Token
#     - Access Token Secret


# Credenziali RapidAPI e Twitter
rapidapi_key <- "LA_TUA_RAPIDAPI_KEY"
twitter_api_key <- "LA_TUA_TWITTER_API_KEY"
twitter_api_secret <- "LA_TUA_TWITTER_API_SECRET"
twitter_access_token <- "IL_TUO_ACCESS_TOKEN"
twitter_access_secret <- "IL_TUO_ACCESS_SECRET"

# URL dell'endpoint Botometer
botometer_url <- "https://botometer-pro.p.rapidapi.com/4/check_account"

# Funzione per verificare un account Twitter
check_account <- function(screen_name) {
  # Configura l'autenticazione e i parametri della richiesta
  response <- POST(
    url = botometer_url,
    add_headers(
      "X-RapidAPI-Key" = rapidapi_key,
      "Content-Type" = "application/json"
    ),
    body = toJSON(list(
      user = list(
        screen_name = screen_name
      ),
      twitter_auth = list(
        consumer_key = twitter_api_key,
        consumer_secret = twitter_api_secret,
        access_token = twitter_access_token,
        access_token_secret = twitter_access_secret
      )
    )),
    encode = "json"
  )
  
  # Verifica lo stato della risposta
  if (status_code(response) == 200) {
    return(fromJSON(content(response, "text")))
  } else {
    print(content(response, "text"))
    stop("Errore nella richiesta API.")
  }
}

# Esempio di utilizzo
risultato <- check_account("TwitterUserName")
print(risultato)


