library(stringr)
library(readr)

raw_dir <- "raw_texts"
clean_dir <- "clean_texts"
segments_dir <- "segments"
corpus_dir <- "stylo_corpus"

dir.create(clean_dir, showWarnings = FALSE)
dir.create(segments_dir, showWarnings = FALSE)
dir.create(corpus_dir, showWarnings = FALSE)

files <- list.files(
  path = raw_dir,
  pattern = "\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

files <- files[!dir.exists(files)]

parts <- str_split(files, "/", simplify = TRUE)

meta <- data.frame(
  path = files,
  group = parts[,2],
  author = parts[,3],
  text = tools::file_path_sans_ext(parts[,4]),
  stringsAsFactors = FALSE
)

clean_text <- function(path) {
  lines <- read_lines(path)
  
  start_idx <- grep("^\\*\\*\\* START OF THE PROJECT GUTENBERG EBOOK", lines)
  end_idx   <- grep("^\\*\\*\\* END OF THE PROJECT GUTENBERG EBOOK", lines)
  
  if (length(start_idx) > 0) {
    lines <- lines[(start_idx[1] + 1):length(lines)]
  }
  
  if (length(end_idx) > 0) {
    lines <- read_lines(path)
    start_idx <- grep("^\\*\\*\\* START OF THE PROJECT GUTENBERG EBOOK", lines)
    end_idx   <- grep("^\\*\\*\\* END OF THE PROJECT GUTENBERG EBOOK", lines)
    
    start_pos <- if (length(start_idx) > 0) start_idx[1] + 1 else 1
    end_pos   <- if (length(end_idx) > 0) end_idx[1] - 1 else length(lines)
    
    lines <- lines[start_pos:end_pos]
  }
  
  txt <- paste(lines, collapse = " ")
  txt <- str_replace_all(txt, "\r|\n", " ")
  txt <- str_replace_all(txt, "\\s+", " ")
  txt <- str_trim(txt)
  
  txt
}

segment_text <- function(text, segment_size = 10000) {
  words <- str_split(text, "\\s+")[[1]]
  n_segments <- floor(length(words) / segment_size)
  
  segments <- vector("list", n_segments)
  
  for (i in seq_len(n_segments)) {
    start <- (i - 1) * segment_size + 1
    end <- i * segment_size
    segments[[i]] <- paste(words[start:end], collapse = " ")
  }
  
  segments
}

for (i in seq_len(nrow(meta))) {
  
  txt <- clean_text(meta$path[i])
  
  clean_file <- file.path(
    clean_dir,
    paste0(meta$author[i], "_", meta$text[i], ".txt")
  )
  
  write_file(txt, clean_file)
  
  segments <- segment_text(txt)
  
  if (length(segments) == 0) next
  
  for (j in seq_along(segments)) {
    seg_name <- paste0(
      meta$author[i], "_",
      meta$text[i], "_",
      sprintf("%02d", j),
      ".txt"
    )
    
    write_file(segments[[j]], file.path(segments_dir, seg_name))
    write_file(segments[[j]], file.path(corpus_dir, seg_name))
  }
}