#Load packages
library(backbone)
library(igraph)

#Import data
B <- as.matrix(read.csv("xmas_movies.csv", row.names = 1, header = TRUE))

#Extract signed backbone
trope <- fdsm(t(B), trials = 50000, alpha = 0.2, class = "igraph", narrative = TRUE, signed = TRUE)
trope <- delete.vertices(trope, which(degree(trope)==0))  #Remove isolates

#Layout
positive <- delete_edges(trope, which(E(trope)$weight==-1))
layout <- layout_with_kk(positive)

#Color
E(trope)$edgecolor <- rgb(0,.53,.24,.75)
E(trope)$edgecolor[which(E(trope)$weight==-1)] <- rgb(.77,.26,.27,.25)
E(trope)$edgewidth <- 3
E(trope)$edgewidth[which(E(trope)$weight==-1)] <- 1

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
pdf("trope.pdf")
plot(trope, layout = layout, 
     vertex.label.cex = .5, vertex.label.color = "black", vertex.label.font = 2, vertex.label.family = "sans", 
     vertex.frame.color = NA, vertex.size = 20, vertex.color = rgb(.83,.68,.21,.5),
     edge.color = E(trope)$edgecolor, edge.width = E(trope)$edgewidth)
dev.off()