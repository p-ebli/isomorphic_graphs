#  This code generates isomorphic graphs based on number of nodes
#  
#  saves the graphs in a pdf file
#  
#  for 2 nodes, 45 graphs are generated, structurally unique (total number 81)
#  
#  for 3 nodes, 3411 graphs are generated (total number 19683)
#  


library(igraph)
library(digest)
library(grDevices)  
library(combinat)

generate_signed_graphs_hashed <- function(n) {
  total_edges <- n * n
  total_graphs <- 3^total_edges
  cat("Total graphs to generate:", total_graphs, "\n")
  
  
  base3_to_signed <- function(vec) {
    sapply(vec, function(x) if (x == 0) 0 else if (x == 1) -1 else 1)
  }
  
  
  int_to_base3 <- function(num, len) {
    base3 <- integer(len)
    for (i in 1:len) {
      base3[i] <- num %% 3
      num <- num %/% 3
    }
    return(rev(base3))
  }
  
  
  perms <- combinat::permn(n)
  
  
  canonical_signature <- function(mat, perms) {
    sigs <- sapply(perms, function(p) {
      permuted_mat <- mat[p, p, drop = FALSE]
      paste(as.vector(permuted_mat), collapse = ",")
    })
    return(min(sigs))
  }
  
  unique_graphs <- list()
  graph_hashes <- character()
  
  
  pdf("signed_graphs.pdf", width = 8, height = 8)
  
  for (i in 0:(total_graphs - 1)) {
    vec <- int_to_base3(i, total_edges)
    adj_vals <- base3_to_signed(vec)
    mat <- matrix(adj_vals, nrow = n, byrow = TRUE)
    
    #
    sig <- canonical_signature(mat, perms)
    h <- digest(sig)
    
    if (!(h %in% graph_hashes)) {
      
      g <- graph_from_adjacency_matrix(mat != 0, mode = "directed", diag = TRUE)
      E(g)$sign <- mat[mat != 0]
      
      unique_graphs[[length(unique_graphs) + 1]] <- g
      graph_hashes <- c(graph_hashes, h)
      
      
      edge_colors <- ifelse(E(g)$sign == -1, "red", "black")
      
      plot(
        g, 
        edge.color = edge_colors,  
        edge.width = 3,  
        vertex.size = 25,  
        vertex.label.cex = 1.2,  
        vertex.label.color = "black",  
        layout = layout_nicely(g)  
      )
    }
  }
  
  
  dev.off()
  
  cat("Unique signed graphs found:", length(unique_graphs), "\n")
  
  return(unique_graphs)
}

# Provide number of nodes
graphs_hashed <- generate_signed_graphs_hashed(2)
