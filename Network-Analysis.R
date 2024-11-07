library(readr)
library(igraph)
library(tidygraph)
library(ggraph)

dataset <- read_csv("dataset.csv")

# Costruzione della rete
# Considerando `userid` e `original_tweet_userid` come esempio
edges <- dataset %>% 
  filter(original_tweet_userid != "0.0") %>% # Rimuove valori non validi
  select(userid, original_tweet_userid) %>% 
  rename(from = userid, to = original_tweet_userid)

# Crea il grafo
network <- graph_from_data_frame(d = edges, directed = TRUE)

# Calcolo delle metriche
degree <- degree(network, mode = "all")
betweenness <- betweenness(network, directed = TRUE)
closeness <- closeness(network, mode = "all")

# Rilevazione di comunità
community <- cluster_louvain(network)

# Aggiungi attributi al grafo
V(network)$degree <- degree
V(network)$betweenness <- betweenness
V(network)$closeness <- closeness
V(network)$community <- membership(community)

# Visualizzazione della rete
ggraph(network, layout = "fr") +
  geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) +
  geom_node_point(aes(size = degree, color = as.factor(community)), show.legend = TRUE) +
  theme_void()
