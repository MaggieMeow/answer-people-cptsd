# Maggie Yundi Li
# SOCY2169 A3

# load libraries
library(ggplot2)
library(igraph)
library(vosonSML)
library(dplyr)

# read in data
g <- read.graph("reddit-cptsd-actor.graphml",format="graphml")

vcount(g)

g <- delete.vertices(g, V(g)[user == "[deleted]"])

# find potential answer people
outdegrees <- degree(g, mode = "out")
table(outdegrees)

high_outdegree_nodes <- which(outdegrees > 10)
high_outdegree_node_names <- V(g)$name[high_outdegree_nodes]

# create a 1.5-degree ego network based on node attribute name and value
create_one_and_half_degree_ego <- function(graph, attribute_value) {
  # find the ego node based on its attribute value
  ego_node <- which(V(graph)$name == attribute_value)
  
  # create 1-degree ego network
  ego_network_1 <- make_ego_graph(graph, order = 1, nodes = ego_node, mode = "all")[[1]]
  
  # get 1-degree neighbors
  neighbors_1 <- unlist(neighbors(graph, ego_node, mode = "all"))
  
  # make 1.5-degree ego network
  nodes_1_5 <- unique(c(ego_node, neighbors_1))
  ego_network_1_5 <- induced_subgraph(graph, nodes_1_5)
  V(ego_network_1_5)$size <- sqrt(degree(ego_network_1_5))
  V(ego_network_1_5)$color <- ifelse(V(ego_network_1_5)$name==attribute_value, "lightblue", "yellow")
  
  # save plot to file
  png(paste0("figures/cptsd_answer_people_1.5ego_", attribute_value, ".png"), width=2000, height=2000)
  plot(ego_network_1_5, edge.width=E(ego_network_1_5)$weight, 
       vertex.label=V(ego_network_1_5)$user, 
       vertex.label.cex=2, edge.arrow.size=0.5)
  dev.off()
}

E(g)$weight <- 1

graph <- simplify(g, remove.loops=TRUE, edge.attr.comb=list(weight="sum"))

# plot ego networks for potential answer people
for (node_name in high_outdegree_node_names) {
  create_one_and_half_degree_ego(graph, node_name)
  print(node_name)
}

# DISTRIBUTION OF NEIGHBOURS' DEGREE
# degree_table <- data.frame(node = character(), outdegree = numeric(), indegree = numeric(), stringsAsFactors = FALSE)
# 
# # loop through node names to get indegree and outdegree
# for (node_name in high_outdegree_node_names) {
#   node_index <- which(V(g)$name == node_name)
#   node_out_degree <- degree(g, v = node_index, mode = "out")
#   node_in_degree <- degree(g, v = node_index, mode = "in")
# 
#   degree_table <- rbind(degree_table, data.frame(node = node_name, outdegree = node_out_degree, indegree = node_in_degree))
# }

# Function to plot log-transformed degree distribution of a node's neighbors
plot_neighbor_degree_distribution <- function(graph, node_name, mode = "out") {
  # get node index
  node_index <- which(V(graph)$name == node_name)
  
  # get neighbors of the node
  neighbors <- neighbors(graph, node_index, mode = mode)
  
  # get the degree of each neighbor
  neighbors_degree <- degree(graph, v = neighbors, mode = mode)
  
  # bin the data
  bin_counts <- cut(as.numeric(neighbors_degree), breaks = c(0,1,2*2^(0:8)), include.lowest = FALSE, right = TRUE)
  #print(table(bin_counts))
  bin_counts <- table(bin_counts)
  
  # log-transform counts with ln(count+1)
  log_counts <- log(bin_counts + 1)
  
  # plot
  degree_distribution <- data.frame(degree = names(bin_counts), count = as.numeric(log_counts))
  
  p <- ggplot(degree_distribution, aes(x = degree, y = count)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    theme_minimal() +
    #theme(panel.background = element_rect(fill = "white", colour = "white")) +
    scale_x_discrete(limits = degree_distribution$degree)+
    labs(title = paste("Distribution of Neighbors Degree, Node ", node_name),
         x = "Degree Bin", y = "Log-transformed Count")
  ggsave(paste0("figures/dist_neighbor_degree_", node_name, ".jpg"), plot = p, width = 10, height = 8, dpi = 300)
  print(node_name)
}


for (node_name in high_outdegree_node_names) {
  plot_neighbor_degree_distribution(graph_no_loop, node_name)
}

# Calculate metrics
calculate_neighbor_outdegree_log_ratio <- function(graph, node_name) {
  node_index <- which(V(graph)$name == node_name)
  # Get the ids of the out-neighbors of the target node
  neighbors <- neighbors(graph, node_index, mode = "out")
  
  # Get the degree of each neighbor
  neighbors_degree <- degree(graph, v = neighbors, mode = "out")
  
  # count small and big degrees
  S <- sum(neighbors_degree <= 3)
  print(S)
  B <- sum(neighbors_degree >= 4)
  print(B)
  
  # Calculate ln((S+1)/(B+1))
  result <- log((S + 1) / (B + 1))
  
  # Return the result
  return(result)
}

log_ratios <- numeric(length(high_outdegree_node_names))

i <- 1
for (node_name in high_outdegree_node_names) {
  log_ratios[i] <- calculate_neighbor_outdegree_log_ratio(graph, node_name)
  i <- i+1
}

results_df <- data.frame(
  node_name = high_outdegree_node_names,
  neighbor_outdegree_metric = log_ratios
)

# proportion of ties to low-degree neighbors
calculate_outward_ties_ratio <- function(graph, node_name) {
  node_index <- which(V(graph)$name == node_name)
  # Get the node's out-degree
  node_out_degree <- degree(graph, v = node_index, mode = "out")
  
  # Get the node's out-neighbors
  out_neighbors <- neighbors(graph, node_index, mode = "out")
  
  # Calculate the number of those neighbors with out-degree 1 and degree of 2
  # different from Welser et al.'s neighbors with out-degree 0 and degree of 1 because all replies in reddit has an outdegree of at least one
  num_relevant_neighbors <- sum(degree(graph, v = out_neighbors, mode = "out") <= 1 &
                                  degree(graph, v = out_neighbors, mode = "all") == 2)
  
  # Calculate the ratio
  ratio <- num_relevant_neighbors / node_out_degree
  
  return(ratio)
}
ratio_low_degree_neighbors <- numeric(length(high_outdegree_node_names))

i <- 1
for (node_name in high_outdegree_node_names) {
  ratio_low_degree_neighbors[i] <- calculate_outward_ties_ratio(graph, node_name)
  i <- i+1
}
results_df$ratio_lo_deg_neighb <- ratio_low_degree_neighbors

