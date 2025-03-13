library(igraph)
library(dplyr)

fehlerhafte_Komponisten <- c("Mozart*", "Bach*", "Beethoven*", "Tchaikovsky*", "Vienna*", "Mendelssohn*", "Handel*", "Brahms*", "Haydn*", "Schubert*", "Schumann*", "Telemann*", "Boccherini*")


## nodes
nodes <- read.csv("/Users/jakob/Desktop/Rudy-Van-Gelder-R/nodesRVG.csv",header=TRUE, encoding = "UTF-8")
nodes <- nodes[!duplicated(nodes$artist), ]
new_node <- data.frame(artist = "Rudy Van Gelder", artist_id = 2283)
nodes <- rbind(nodes, new_node)
node_names <- nodes$artist
nodes$artist <- trimws(nodes$artist)
node_names <- trimws(node_names)
##
duplicated_nodes <- nodes_filtered[duplicated(nodes_filtered$artist), ]
print(duplicated_nodes)
nodes_filtered <- nodes_filtered[!duplicated(nodes_filtered$artist), ]
## edges
edges <- read.csv("/Users/jakob/Desktop/Rudy-Van-Gelder-R/edgesRVG.csv",header=TRUE, encoding = "UTF-8")
edges$source <- trimws(edges$source)
edges$target <- trimws(edges$target)
edge_nodes <- unique(c(edges$source, edges$target))
missing_from_nodes <- setdiff(edge_nodes, node_names)
invalid_edges <- edges$source %in% missing_from_nodes | edges$target %in% missing_from_nodes
edges_cleaned <- edges[!invalid_edges, ]
##

edges_filtered <- edges_cleaned[!(edges_cleaned$source %in% fehlerhafte_Komponisten | edges_cleaned$target %in% fehlerhafte_Komponisten), ]
nodes_filtered <- nodes[!nodes$artist %in% fehlerhafte_Komponisten, ]


G = graph.data.frame(edges_filtered, directed = F, vertices = nodes_filtered)

density <- graph.density(G)
print(paste("RVG Network density:", format(density,digits = 8)))

diameter <- diameter(G)
print(paste("RVG Network Diameter:", diameter))

components <- components(G)
print(paste("RVG Network components:", components$no))

avrg_path_length <- mean_distance(G, directed = F, unconnected = T)
print(paste("Average Path Length in RVG network:", avrg_path_length))

transitivity <- transitivity(G)
print(paste("Transitivity of RVG Graph:", transitivity))

####----------------------------------------------------------------------------

degree.G <- degree(G)
head(degree.G)
G <- set_vertex_attr(G, "degree", index = V(G), degree.G)
print(paste("Largest degree:", sort(degree.G,decreasing = T)[1]))
sort(degree.G,decreasing = T)[1:20]

#### Table:
top_10_degree <- sort(degree.G, decreasing = TRUE)[1:10]
top_10_degree_tabelle <- data.frame(
  Node = names(top_10_degree),
  Degree = top_10_degree
)
rownames(top_10_degree_tabelle) <- 1:10

####----------------------------------------------------------------------------

cc.G <- closeness(G, mode = "all", normalized = T)
G <- set_vertex_attr(G, "closeness", index = V(G), cc.G)

#### Table:
top_10_closeness <- sort(cc.G, decreasing = T)[1:10]
top_10_closeness_tabelle <- data.frame(
  Node = names(top_10_closeness),
  Closeness = top_10_closeness
)
rownames(top_10_closeness_tabelle) <- 1:10

####----------------------------------------------------------------------------

evc.G <- evcent(G)$vector
G <- set_vertex_attr(G, "eigenvector_centrality", index = V(G), evc.G)
print(paste("Largest eigenvector centrality:", sort(evc.G, decreasing = TRUE)[1]))
sort(evc.G, decreasing = TRUE)[1:20]

#### Table:
top_10_eigenvector <- sort(evc.G, decreasing = T)[1:10]
top_10_eigenvector_tabelle <- data.frame(
  Node = names(top_10_eigenvector),
  Eigenvector = top_10_eigenvector
)
rownames(top_10_eigenvector_tabelle) <- 1:10

####----------------------------------------------------------------------------

bc.G <- betweenness(G, normalized = T)
G <- set_vertex_attr(G, "betweenness_centrality", index = V(G), bc.G)
sort(bc.G,decreasing = T)[1:20]

#### Table:
top_10_betweenness <- sort(bc.G, decreasing = T)[1:10]
top_10_betweenness_tabelle <- data.frame(
  Node = names(top_10_betweenness),
  Betweenness = top_10_betweenness
)
rownames(top_10_betweenness_tabelle) <- 1:10
options(scipen = 999)
print(names(bc.G))
####----------------------------------------------------------------------------

#### erstellen eines zweiten Graphen, welcher keine multi-edges mehr hat, damit cluster gefunden werden können.
G2 <- simplify(G)
c1 = cluster_fast_greedy(G2)
G <- set_vertex_attr(G2, "modularity", index = V(G2), membership(c1))
modularity(c1)
head(membership(c1))
length(c1)
sizes(c1)
par(mai=c(0,0,1,0))
library(dplyr)
vertex_name_proxy <- tibble(Label = V(G2)$name, Degree = V(G2)$degree) %>%
  mutate(vertex_name = ifelse(Degree < 10, NA, Label))
plot(c1, G2,
     vertex.size = degree(G2),
     vertex.label = vertex_name_proxy$vertex_name,
     edge.arrow.size = .25,
     vertex.label.font = 2,
     edge.curved=.1,
     vertex.label.dist=1,
     vertex.label.color="purple",
     vertex.label.cex = 1)
#### experiment geendet. Graph sehr unübersichtlich

#### bc.G[bc.G == 0] <- 0.0 (wurde bei 0 immer falsch gecastet, hat aber hier nicht funktioniert das richtig umzuwandeln)

G <- delete_vertex_attr(G, "betweenness_centrality")
G <- delete_vertex_attr(G, "eigenvector_centrality")

write_graph(G, "attr_graph_out.gml", format = "gml")

G <- delete_vertex_attr(G, "modularity")

#### Netzwerk nach Jahrzenten filtern
edges_1930s <- subset(edges_filtered, year >= 1930 & year < 1940)
edges_1940s <- subset(edges_filtered, year >= 1940 & year < 1950)
edges_1950s <- subset(edges_filtered, year >= 1950 & year < 1960)
edges_1960s <- subset(edges_filtered, year >= 1960 & year < 1970)
edges_1970s <- subset(edges_filtered, year >= 1970 & year < 1980)
edges_1980s <- subset(edges_filtered, year >= 1980 & year < 1990)
edges_1990s <- subset(edges_filtered, year >= 1990 & year < 2000)
edges_2000s <- subset(edges_filtered, year >= 2000 & year < 2010)
edges_2010s <- subset(edges_filtered, year >= 2010 & year < 2017)
edges_posthum <- subset(edges_filtered, year >= 2017)
edges_closeToPosthum <- subset(edges_filtered, year >= 2017 & year < 2021)
edges_notCloseToPosthum <- subset(edges_filtered, year >= 2021)

print(nrow(edges_1930s))
print(nrow(edges_1940s))
print(nrow(edges_1950s))
print(nrow(edges_1960s))
print(nrow(edges_1970s))
print(nrow(edges_1980s))
print(nrow(edges_1990s))
print(nrow(edges_2000s))
print(nrow(edges_2010s))
print(nrow(edges_posthum))

ReleaseProJahrzent <- data.frame(
  Jahrzent = c("1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "posthum"),
  Releases = c(nrow(edges_1930s), nrow(edges_1940s), nrow(edges_1950s), nrow(edges_1960s), nrow(edges_1970s), 
            nrow(edges_1980s), nrow(edges_1990s), nrow(edges_2000s), nrow(edges_2010s), nrow(edges_posthum))
) %>%
  arrange(desc(Releases))

