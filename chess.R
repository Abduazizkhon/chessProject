#How does the choice of opening affect the outcome of a chess game?


#libraries


library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)



#READING THE DATASET
setwd('/Users/user/Desktop/dm_project')
ds_chess <- read.csv('games.csv')
View(ds_chess)
num_rows <- nrow(ds_chess)


#--------------------------------------------------
#I replaced all ids with simple integers
ds_chess$id <- seq(0, num_rows - 1)
ds_chess$id 


# Set seed for reproducibility
# Set seed for reproducibility
#set.seed(41)

# Load the dataset
ds_chess <- read.csv('games.csv')

# Sample 100 nodes with replacement
sample_ds <- sample_n(ds_chess, size = 100, replace = TRUE)
sample_rows <- nrow(sample_ds)

# Create unique vertex names to avoid duplicates
sample_ds$unique_id <- paste0(sample_ds$id, "_", seq_len(nrow(sample_ds)))

# Create an edge list based on shared opening_eco and include the 'winner' attribute
edges <- data.frame()
for (eco in unique(sample_ds$opening_eco)) {
  games_with_eco <- sample_ds[sample_ds$opening_eco == eco, , drop = FALSE]
  if (nrow(games_with_eco) > 1) {
    comb <- t(combn(games_with_eco$unique_id, 2))
    winners <- apply(comb, 1, function(x) paste(games_with_eco[games_with_eco$unique_id == x[1], "winner"],
                                                games_with_eco[games_with_eco$unique_id == x[2], "winner"]))
    edges <- rbind(edges, data.frame(from=comb[,1], to=comb[,2], opening_eco=eco, winner_combo=winners))
  }
}

# Assign colors to edges based on the combination of game outcomes
edges$color <- sapply(edges$winner_combo, function(combo) {
  winners <- unlist(strsplit(combo, " "))
  if (winners[1] == winners[2]) {
    if (winners[1] == "white") {
      return("green")
    } else if (winners[1] == "black") {
      return("red")
    } else if (winners[1] == "draw") {
      return("yellow")
    }
  } else {
    return("gray")
  }
})

# Create the graph from the edge list using unique IDs
g <- graph_from_data_frame(d=edges, vertices=sample_ds[, c("unique_id", "opening_eco", "id", "winner")], directed=FALSE)

# Identify the openings with more than 3 games
openings_more_than_3 <- unique(sample_ds$opening_eco[ave(sample_ds$opening_eco, sample_ds$opening_eco, FUN=length) > 3])

# Assign colors to these openings
opening_colors <- rainbow(length(openings_more_than_3))
names(opening_colors) <- openings_more_than_3

# Assign colors to vertices based on their opening_eco
V(g)$color <- ifelse(V(g)$opening_eco %in% openings_more_than_3, opening_colors[V(g)$opening_eco], "skyblue")

# Assign edge colors
E(g)$color <- edges$color

# Add edge labels for the openings with more than 3 games
#E(g)$label <- ifelse(E(g)$opening_eco %in% openings_more_than_3, E(g)$opening_eco, NA)

# Calculate vertex degrees for sizing
vertex_degrees <- degree(g) * 0.6

# Plot the graph with the specified edge labels and vertex colors
plot(g, vertex.size = vertex_degrees, vertex.color = V(g)$color, vertex.label=NA,
     edge.label = E(g)$label, edge.label.cex = 0.8, edge.label.color = "blue",
     main = "Game Network Based on Shared Openings (Sampled Nodes)",
     edge.color = E(g)$color)

# Optionally, add legends or further customize the plot
legend("bottomleft", legend=names(opening_colors), fill=opening_colors, border=NA, bty="n", cex=0.8, title="Openings")
legend("topright", legend=c("White Wins", "Black Wins", "Draw", "Different Outcomes"), fill=c("green", "red", "yellow", "gray"), border=NA, bty="n", cex=0.8, title="Game Outcomes")

# node_attributes <- vertex_attr_names(g)
# node_attributes

#let's calculate corelation of chosing openning an who will win
# Create a contingency table of opening_eco and winner
# Identify the openings with more than 3 games
popular_openings <- ds_chess %>%
  group_by(opening_eco) %>%
  filter(n() > 100) %>%
  ungroup()

# Ensure only popular openings are included
sample_ds_popular <- ds_chess %>%
  filter(opening_eco %in% popular_openings$opening_eco)

# Aggregate data to count the number of white wins, black wins, and draws for each popular opening
agg_data <- sample_ds_popular %>%
  group_by(opening_eco, winner) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = winner, values_from = count, values_fill = list(count = 0))

# Melt the aggregated data for ggplot2
agg_data_melted <- agg_data %>%
  gather(key = "outcome", value = "count", -opening_eco)

# Plot the distribution of game outcomes for each opening_eco
ggplot(agg_data_melted, aes(x = opening_eco, y = count, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Game Outcomes by Popular Openings",
       x = "Opening ECO", y = "Count", fill = "Outcome") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white")
        )
#--------------------------------------------------------------------
chess <- ds_chess %>%
  mutate(time_increment = as.numeric(sub("\\+.*", "", increment_code)),
         increment_class = case_when(
           time_increment <= 5 ~ "blitz",
           time_increment > 30 ~ "classic",
           TRUE ~ "rapid"
         ))
View(chess)

data <- chess %>%
  mutate(move_pairs = strsplit(as.character(moves), " ")) %>%
  rowwise() %>%
  mutate(move_pairs = list(paste0(move_pairs[seq(1, length(move_pairs)-1, 2)], 
                                  "-", 
                                  move_pairs[seq(2, length(move_pairs), 2)])))

# Unnest the move pairs into separate rows
edges <- data %>%
  select(opening_eco, move_pairs) %>%
  unnest(move_pairs) %>%
  select(opening_eco, move_pairs)

# Create the bipartite graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Add types to the vertices
V(g)$type <- V(g)$name %in% unique(data$opening_eco)

# Plot the bipartite graph
plot(g, vertex.label = NA,
     vertex.color = ifelse(V(g)$type, "red", "blue"),
     vertex.size = 5,
     edge.color = "grey",
     main = "Bipartite Network of Chess Move Pairs and Openings")
















# g <- make_graph(edges = c(1, 2, 3, 5), n = 10, directed = FALSE)
# g
# plot(g)
# g <- make_graph(edges = c(1, 2, 3, 5), n = 10, directed = FALSE)
# 
# g <- delete_vertices(g, 10) # adds specified number of verticies to already existing number of verticies
# plot(g)
# g <- add_edges(g, edges = c(6,4,6,7))
# plot(g)
# g <- add_edges(g, edges = c(5,6,7,9)) #in edges you specify which nodes u want to connect
# plot(g)
# 
# g <- g %>%
#   add_edges(edges = c(1, 10)) %>%
#   add_vertices(3)
# plot(g)
# 
# g <- make_ring(10)
# g <- delete_edges(g, get.edge.ids(g, c(5, 4, 5,6)))
# 
# plot(g)
# g1 <- graph_from_literal(
#   A - B:C:I, B - A:C:D, 
#   C - A:B:E:H, 
#   D - B:E:F,
#   E - C:D:F:H, 
#   F - D:E:G, 
#   G - F:H, 
#   H - C:E:G:I,
#   I - A:H
# )
# plot(g1)
# 
# #constructing graphs
# graph1 <- make_tree(13, 2, mode = "undirected")
# plot(graph1)
# 
# graph1 <- sample_grg(10, 0.2)# 0.2 is the radius graphs will be connec ted if their euc radius is arounfd this va
# 
# plot(graph1)
# 
# g <- make_graph(
#   ~ Alice - Boris:Himari:Moshe, Himari - Alice:Nang:Moshe:Samira,
#   Ibrahim - Nang:Moshe, Nang - Samira
# )
# plot(g)
# V(g)$age <- c(25, 31, 18, 23, 47, 22, 50)
# V(g)$gender <- c("f", "m", "f", "m", "m", "f", "m")
# E(g)$is_formal <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
# plot(g)
# g <- make_graph(
#   ~ Alice - Boris:Himari:Moshe, Himari - Alice:Nang:Moshe:Samira,
#   Ibrahim - Nang:Moshe, Nang - Samira
# ) %>%
#   set_vertex_attr("age", value = c(25, 31, 18, 23, 47, 22, 50)) %>%
#   set_vertex_attr("gender", value = c("f", "m", "f", "m", "m", "f", "m")) %>%
#   set_edge_attr("is_formal", value = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))
# plot(g)
# E(g)$is_formal
# V(g)$gender
# g$date <- c("2022-02-11")
# graph_attr(g, 'date')
# match(c("Ibrahim"), V(g)$name)
# V(g)$name[1:3] <- c("Alejandra", "Bruno", "Carmina")
# V(g)
# plot(g)
# as_adjacency_matrix(g)
# layout <- layout_in_circle
# plot(g, layout = layout, main = "Social network with the Kamada-Kawai layout algorithm")
# V(g)$color <- ifelse(V(g)$gender == "m", "yellow", "red")
# plot(
#   g,
#   layout = layout, vertex.label.dist = 3.5,
#   main = "Social network - with genders as colors"
# )
# vertex_degrees <- degree(g)*10
# vertex_degrees
# plot(g,
#      layout = layout, vertex.label.dist = 3.5,
#      vertex.color = ifelse(V(g)$gender == "m", "yellow", "red"),
#      edge.width = ifelse(E(g)$is_formal, 5, 1),
#      vertex.size = vertex_degrees
# )
# #layout <- layout_with_fr(g)  # Example layout, change as needed
# 
# # Calculate the degree of each vertex and scale it
# vertex_degrees <- degree(g) * 10
# 
# # Print vertex degrees to verify
# print(vertex_degrees)
# 
# # Plot the graph with the corrected vertex sizes
# plot(g,
#      layout = layout, vertex.label.dist = 3.5,
#      vertex.color = ifelse(V(g)$gender == "m", "yellow", "red"),
#      edge.width = ifelse(E(g)$is_formal, 5, 1),
#      vertex.size = vertex_degrees
# )