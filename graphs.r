#  This code generates isomorphic graphs based on number of nodes
#  
#  saves the graphs in a pdf file
# 
#  the corresponding matrix is written on the left side of each graph
# 
#  
#  for 2 nodes, 45 graphs are generated, structurally unique (total number 81)
#  
#  for 3 nodes, 3411 graphs are generated (total number 19683)
#  


library(igraph)
library(digest)
library(grDevices)
library(combinat)


plot_matrix_as_text <- function(mat) {
  text_lines <- capture.output(print(mat))
  plot.new()
  text(x = 0, y = 1, labels = "Adjacency Matrix", adj = c(0, 1), cex = 1.2)
  for (i in seq_along(text_lines)) {
    text(x = 0, y = 0.9 - 0.1 * i, labels = text_lines[i], adj = c(0, 1), cex = 0.9, family = "mono")
  }
}

generate_signed_graphs_hashed <- function(n) {
  total_edges <- n * n
  total_graphs <- 3 ^ total_edges
  cat("Total graphs to generate:", total_graphs, "\n")
  
  base3_to_signed <- function(vec) {
    sapply(vec, function(x) if (x == 0) 0 else if (x == 1) 1 else -1)
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
  
  pdf("signed_graphs.pdf", width = 10, height = 5)
  
  graph_counter <- 0
  for (i in 0:(total_graphs - 1)) {
    vec <- int_to_base3(i, total_edges)
    adj_vals <- base3_to_signed(vec)
    mat <- matrix(adj_vals, nrow = n, byrow = TRUE)
    
    sig <- canonical_signature(mat, perms)
    h <- digest(sig)
    
    if (!(h %in% graph_hashes)) {
      M <- t(mat)                                 # Transposed
      g <- graph_from_adjacency_matrix(M != 0, mode = "directed", diag = TRUE)
      
      edge_coords <- which(M != 0, arr.ind = TRUE)
      edge_coords <- edge_coords[order(edge_coords[, "row"], edge_coords[, "col"]), , drop = FALSE]
      edge_signs <- mapply(function(i, j) M[i, j], edge_coords[, "row"], edge_coords[, "col"])
      E(g)$sign <- edge_signs
      
      graph_counter <- graph_counter + 1
      unique_graphs[[graph_counter]] <- g
      graph_hashes <- c(graph_hashes, h)
      
      edge_colors <- ifelse(E(g)$sign == -1, "red", "black")
      
      layout(matrix(c(1, 2), nrow = 1), widths = c(1, 2))
      
      # Left panel: show matrix as text
      par(mar = c(1, 1, 1, 1))
      plot_matrix_as_text(mat)
      
      # Right panel: plot the graph
      par(mar = c(5, 5, 5, 5))
      plot(
        g,
        edge.color = edge_colors,
        edge.width = 3,
        vertex.size = 25,
        vertex.label.cex = 1.2,
        vertex.label.color = "black",
        layout = layout_nicely(g),
        edge.curved = 0.5,
        edge.arrow.size = 1.5,
        main = paste("Graph", graph_counter)
      )
    }
  }
  
  dev.off()
  cat("Unique signed graphs found:", length(unique_graphs), "\n")
  return(unique_graphs)
}

# provide number of nodes
graphs_hashed <- generate_signed_graphs_hashed(2)
