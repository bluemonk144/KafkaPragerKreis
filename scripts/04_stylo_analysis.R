library(stylo)

setwd("~/kafka")

results_pca <- stylo(
  gui = FALSE,
  corpus.dir = "stylo_corpus",
  analysis.type = "PCR",
  mfw.min = 1000,
  mfw.max = 1000,
  culling.min = 0,
  culling.max = 0,
  distance.measure = "delta"
)



