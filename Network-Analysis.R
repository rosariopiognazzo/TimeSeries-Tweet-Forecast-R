library(readr)
library(igraph)
library(tidygraph)
library(ggraph)

dataset <- read_csv("dataset.csv")

# dataframe con le interazioni tra utenti (retweet o risposte)
network_data <- dataset %>%
  filter(original_tweet_userid != "0.0") %>%
  select(userid, original_tweet_userid, retweetcount, favorite_count) %>%
  rename(source = userid, target = original_tweet_userid)

head(network_data)

# grafo diretto 
network_graph <- graph_from_data_frame(d=network_data, directed=TRUE)

# Aggiungo attributo dei retweet ricevuti ai nodi
V(network_graph)$retweetcount <- sapply(V(network_graph)$name, function(x) {
  sum(network_data$retweetcount[network_data$target == x])
})

# Aggiungo attributo dei like ricevuti
V(network_graph)$favorite_count <- sapply(V(network_graph)$name, function(x) {
  sum(network_data$favorite_count[network_data$target == x])
})


## -------- Metriche di centralità -------------------##
# centralità di grado
V(network_graph)$degree <- degree(network_graph, mode="in")

# centralità di betweenness
V(network_graph)$betweenness <- betweenness(network_graph, directed=TRUE)

# centralità di PageRank
V(network_graph)$pagerank <- page_rank(network_graph, directed=TRUE)$vector


# Ordino gli utenti per metrica diPageRank
top_influencers <- data.frame(
  username = V(network_graph)$name,
  degree = V(network_graph)$degree,
  retweetcount = V(network_graph)$retweetcount,
  favorite_count = V(network_graph)$favorite_count,
  pagerank = V(network_graph)$pagerank
) %>%
  arrange(desc(pagerank))

# top 10 influencer
head(top_influencers, 10)

high_pagerank_nodes <- V(network_graph)[V(network_graph)$pagerank > quantile(V(network_graph)$pagerank, 0.95)]
reduced_graph <- induced_subgraph(network_graph, high_pagerank_nodes)

# Visualizza il grafo ridotto
ggraph(reduced_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha=0.2), show.legend=FALSE) + 
  geom_node_point(aes(size = pagerank, color = degree)) +
  scale_size(range = c(2, 10)) +
  labs(title = "Grafo Ridotto agli Utenti con Alta Centralità") +
  theme_void()


