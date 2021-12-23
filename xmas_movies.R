rm(list=ls())
library(backbone)
library(igraph)

#### Visualization function ####
draw.xmas.network <- function(net) {
  set.seed(1225)
  net <- delete.vertices(net, which(degree(net)==0))  #Remove isolates
  
  #Make the labels look nice
  V(net)$name <- stringr::str_replace_all(V(net)$name, "\\ ", "\n")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "\\.", "\n")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "A\n", "A ")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "An\n", "An ")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "\na\n", "\na ")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "The\n", " The ")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "\nthe\n", " the\n")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "\nand\n", " &\n")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "\nin\n", " in\n")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "\nof\n", " of\n")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "\nit", " it")
  V(net)$name <- stringr::str_replace_all(V(net)$name, "is\n", " is ")
  
  #Color by community membership
  community <- cluster_louvain(net)
  V(net)$color <- community$membership              
  V(net)[V(net)$color == 1]$color <- rgb(.77,.26,.27,.5)
  V(net)[V(net)$color == 2]$color <- rgb(0,.53,.24,.5)
  V(net)[V(net)$color == 3]$color <- rgb(.83,.68,.21,.5)
  V(net)[V(net)$color == 4]$color <- rgb(.75,.75,.75,.5)
  V(net)[V(net)$color == 5]$color <- "#235E6F80"
  V(net)[V(net)$color == 6]$color <- "#8B008B80"
  
  #Adjust layout to highlight communities
  weights <- ifelse(crossing(community, net), .45, .2)
  layout <- layout_with_kk(net, weights = weights)
  
  plot(net, layout = layout, vertex.label.cex = .5, vertex.label.color = "black",
       vertex.label.font = 2, vertex.label.family = "sans", vertex.frame.color = NA, vertex.size = 20)
}

#### Import Data ####
B <- read.csv("xmas_movies.csv", row.names = 1, header = TRUE)

#### The movie network ####
movie <- fdsm(B, trials = 50000, alpha = 0.15, class = "igraph", narrative = TRUE)
pdf("movie.pdf")
draw.xmas.network(movie)
dev.off()

#### The trope network ####
trope <- fdsm(t(B), trials = 50000, alpha = 0.15, class = "igraph", narrative = TRUE)
pdf("trope.pdf")
draw.xmas.network(trope)
dev.off()

