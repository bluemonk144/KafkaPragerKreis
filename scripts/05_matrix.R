library(readr)
library(stringr)
library(dplyr)
library(stylo)

setwd("~/kafka")

clean_dir <- "clean_texts"
author_dir <- "author_corpus"

dir.create(author_dir, showWarnings = FALSE)

# optional: remove old author corpus files
old_files <- list.files(author_dir, full.names = TRUE)
if (length(old_files) > 0) file.remove(old_files)

# -----------------------------
# 1. Build one corpus file per author
# -----------------------------
files <- list.files(
  path = clean_dir,
  pattern = "\\.txt$",
  full.names = TRUE
)

meta_files <- data.frame(
  path = files,
  file = basename(files),
  stringsAsFactors = FALSE
)

meta_files$author <- sub("_.*", "", tools::file_path_sans_ext(meta_files$file))
author_list <- split(meta_files$path, meta_files$author)

for (a in names(author_list)) {
  texts <- lapply(author_list[[a]], read_file)
  combined <- paste(texts, collapse = "\n\n")
  combined <- str_replace_all(combined, "[ \t]+", " ")
  combined <- str_replace_all(combined, "\n{3,}", "\n\n")
  combined <- str_trim(combined)
  
  write_file(
    paste0(combined, "\n"),
    file.path(author_dir, paste0(a, ".txt"))
  )
}

# -----------------------------
# 2. Correspondence analysis with random sampling
# -----------------------------
set.seed(123)

res_random_ca <- stylo(
  gui = FALSE,
  corpus.dir = author_dir,
  analysis.type = "CA",
  mfw.min = 1000,
  mfw.max = 1000,
  culling.min = 0,
  culling.max = 0,
  sampling = "random.sampling",
  sample.size = 5000,
  number.of.samples = 5,
  distance.measure = "delta"
)

dist_mat <- as.matrix(res_random_ca$distance.table)
write.csv(dist_mat, "distance_matrix_random_authors.csv", row.names = TRUE)

# -----------------------------
# 3. Metadata for samples
# -----------------------------
labs <- rownames(dist_mat)

meta <- data.frame(
  sample = labs,
  author = sub("_\\d+$", "", labs),
  stringsAsFactors = FALSE
)

# -----------------------------
# 4. Mean distance of each sample to all samples of other authors
# -----------------------------
mean_to_others <- sapply(seq_len(nrow(dist_mat)), function(i) {
  other_idx <- which(meta$author != meta$author[i])
  mean(dist_mat[i, other_idx])
})

sample_summary <- data.frame(
  sample = meta$sample,
  author = meta$author,
  mean_dist_to_others = mean_to_others,
  stringsAsFactors = FALSE
) %>%
  arrange(desc(mean_dist_to_others))

print(sample_summary)
write.csv(sample_summary, "sample_mean_distances.csv", row.names = FALSE)

# -----------------------------
# 5. Mean distance of each author to all other authors
# -----------------------------
author_mean_dist <- sapply(unique(meta$author), function(a) {
  idx_a <- which(meta$author == a)
  idx_other <- which(meta$author != a)
  mean(dist_mat[idx_a, idx_other])
})

author_mean_dist <- sort(author_mean_dist, decreasing = TRUE)
print(author_mean_dist)

author_mean_dist_df <- data.frame(
  author = names(author_mean_dist),
  mean_distance_to_others = as.numeric(author_mean_dist),
  stringsAsFactors = FALSE
)

write.csv(author_mean_dist_df, "author_mean_distances.csv", row.names = FALSE)

# -----------------------------
# 6. Within-author mean distances
# -----------------------------
author_within <- sapply(unique(meta$author), function(a) {
  idx <- which(meta$author == a)
  submat <- dist_mat[idx, idx, drop = FALSE]
  mean(submat[upper.tri(submat)])
})

author_within <- sort(author_within, decreasing = TRUE)
print(author_within)

author_within_df <- data.frame(
  author = names(author_within),
  mean_within_author_distance = as.numeric(author_within),
  stringsAsFactors = FALSE
)

write.csv(author_within_df, "author_within_distances.csv", row.names = FALSE)

# -----------------------------
# 7. Author-to-author mean distance matrix
# -----------------------------
authors <- unique(meta$author)

author_author <- matrix(
  NA,
  nrow = length(authors),
  ncol = length(authors),
  dimnames = list(authors, authors)
)

for (a in authors) {
  for (b in authors) {
    idx_a <- which(meta$author == a)
    idx_b <- which(meta$author == b)
    
    if (a != b) {
      author_author[a, b] <- mean(dist_mat[idx_a, idx_b])
    }
  }
}

print(round(author_author, 3))
write.csv(author_author, "author_to_author_distances.csv", row.names = TRUE)

# -----------------------------
# 8. Overall outlier / centrality score
# -----------------------------
author_outlier_score <- sort(rowMeans(author_author, na.rm = TRUE), decreasing = TRUE)
print(author_outlier_score)

author_outlier_df <- data.frame(
  author = names(author_outlier_score),
  mean_author_to_author_distance = as.numeric(author_outlier_score),
  stringsAsFactors = FALSE
)

write.csv(author_outlier_df, "author_outlier_scores.csv", row.names = FALSE)

# -----------------------------
# 9. Mean distance to Kafka
# -----------------------------
kafka_idx <- which(meta$author == "kafka")

kafka_distances <- sapply(setdiff(authors, "kafka"), function(a) {
  idx_a <- which(meta$author == a)
  mean(dist_mat[kafka_idx, idx_a])
})

kafka_distances <- sort(kafka_distances)
print(kafka_distances)

kafka_distances_df <- data.frame(
  author = names(kafka_distances),
  mean_distance_to_kafka = as.numeric(kafka_distances),
  stringsAsFactors = FALSE
)

write.csv(kafka_distances_df, "distance_to_kafka.csv", row.names = FALSE)

# -----------------------------
# 10. Prague vs non-Prague
# -----------------------------
prague <- c("brod", "kafka", "leppin", "meyrink", "rilke", "werfel")
nonprague <- c("hesse", "mann", "musil", "schnitzler", "zweig")

prague_idx <- which(meta$author %in% setdiff(prague, "kafka"))
nonprague_idx <- which(meta$author %in% nonprague)

obs_prague <- mean(dist_mat[kafka_idx, prague_idx])
obs_nonprague <- mean(dist_mat[kafka_idx, nonprague_idx])
obs_diff <- obs_nonprague - obs_prague

cat("\nKafka -> Prague mean distance:", obs_prague, "\n")
cat("Kafka -> Non-Prague mean distance:", obs_nonprague, "\n")
cat("Difference (Non-Prague - Prague):", obs_diff, "\n")

# -----------------------------
# 11. Permutation test
# -----------------------------
set.seed(123)

other_authors <- setdiff(authors, "kafka")
n_prague_others <- length(setdiff(prague, "kafka"))

perm_diffs <- replicate(10000, {
  perm_prague <- sample(other_authors, n_prague_others, replace = FALSE)
  perm_nonprague <- setdiff(other_authors, perm_prague)
  
  perm_prague_idx <- which(meta$author %in% perm_prague)
  perm_nonprague_idx <- which(meta$author %in% perm_nonprague)
  
  mean(dist_mat[kafka_idx, perm_nonprague_idx]) -
    mean(dist_mat[kafka_idx, perm_prague_idx])
})

p_value <- mean(perm_diffs >= obs_diff)

cat("Permutation test p-value:", p_value, "\n")

perm_results <- data.frame(
  obs_prague = obs_prague,
  obs_nonprague = obs_nonprague,
  obs_diff = obs_diff,
  p_value = p_value
)

write.csv(perm_results, "prague_vs_nonprague_permutation.csv", row.names = FALSE)