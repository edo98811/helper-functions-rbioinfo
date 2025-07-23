plot_n_reads <- function(dds) {
  
  myd <- data.frame(
    counts = colSums(counts(dds)),
    sample = colnames(dds)
  )
  ggplot(myd, aes(x = sample, weight = counts, fill = sample)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_bw() +
    coord_flip() +
    theme(legend.position = "none")
}
