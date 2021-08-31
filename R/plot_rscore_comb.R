library(igraph)

plot_rscore_combination <- function(combinations, scores) {
   g1 <- graph(combinations, directed = FALSE)
   plot_title <- paste0("Total score: ", round(prod(scores), 4))
   
   plot(
      g1,
      main = plot_title,
      vertex.size = 100,
      edge.label = scores,
      edge.label.color = "black"
   )
}

save_and_plot_rscore_combination <- function(combinations, scores,
                                             file_path) {
   png(file_path)
   plot_rscore_combination(combinations, scores)
   dev.off()
}