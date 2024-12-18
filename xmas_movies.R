#Load packages
devtools::install_github("zpneal/backbone", ref = "v3", build_vignettes = TRUE)  #Install latest version of backbone
library(backbone)
library(igraph)

#Import data as an incidence/biadjacency matrix
B <- as.matrix(read.csv("xmas_movies.csv", row.names = 1, header = TRUE))
B <- t(B)  #Transpose to focus on tropes (omit this to focus on movies, although the network doesn't look as nice)

#Extract backbone
trope <- backbone_from_bipartite(B, alpha = 0.1, model = "fdsm")
trope <- graph_from_adjacency_matrix(trope, mode = "undirected")
trope <- delete_vertices(trope, which(degree(trope)==0))  #Remove isolates

#Make the labels look nice
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\\ ", "\n")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\\.", "\n")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "A\n", "A ")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "An\n", "An ")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\na\n", "\na ")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "The\n", " The ")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\nthe\n", " the\n")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\nand\n", " &\n")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\nin\n", " in\n")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\nto\n", "  to\n")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\nof\n", " of\n")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\nit", " it")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "\nbe", " be")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "or\n", "or ")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, "is\n", " is ")
V(trope)$name <- stringr::str_replace_all(V(trope)$name, " belief", "\nbelief")

#Plot
par(mar = c(0, 0, 0, 0))
pdf("trope.pdf", height = 20, width = 20)
plot(trope, layout = layout_with_kk(trope),
     vertex.label.cex = 1.6, vertex.label.color = "black", vertex.label.font = 2, vertex.label.family = "sans", 
     vertex.frame.color = NA, vertex.size = 20, vertex.color = rgb(.84,0,.11,.5),
     edge.color = rgb(0,.53,.24,.75), edge.width = 10)
dev.off()
