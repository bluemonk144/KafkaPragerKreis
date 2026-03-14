library(stringr)
library(readr)
library(dplyr)

raw_dir <- "raw_texts"
clean_dir <- "clean_texts"
segments_dir <- "segments"
corpus_dir <- "stylo_corpus"

dir.create(clean_dir, showWarnings = FALSE)
dir.create(segments_dir, showWarnings = FALSE)
dir.create(corpus_dir, showWarnings = FALSE)

files <- list.files(
  path = raw_dir,
  recursive = TRUE,
  full.names = TRUE
)

files <- files[!dir.exists(files)]
files <- files[str_detect(files, "\\.txt$")]

meta <- data.frame(
  path = files,
  stringsAsFactors = FALSE
)

meta$text <- tools::file_path_sans_ext(basename(meta$path))
meta$author <- basename(dirname(meta$path))
meta$group <- basename(dirname(dirname(meta$path)))

meta$author <- str_replace_all(meta$author, " ", "_")

trim_table <- tribble(
  ~author,         ~text,                   ~trim_start, ~trim_end,
  "kafka",         "prozess",               6,           227,
  "kafka",         "der_bau",               2,           0,
  "kafka",         "landartzt",             24,           3,
  "kafka",         "strafkolonie",          7,           4,
  "kafka",         "verwandlung",           12,           1,
  "brod",          "Arnold_Beer",           24,           227,
  "brod",          "gespenstergeschichte",  13,           4,
  "brod",          "gottin",                9,           5,
  "leppin",        "Severin",               11,           99,
  "leppin",        "blaugast",               0,           0,
  "meyrink",       "der_golem",             38,           72,
  "rilke",         "aufzeichnungen",        2,           2,
  "werfel",        "ermoredete",            30,           23,
  "hesse",         "siddharta",             8,           0,
  "hesse",         "steppenwolf",           12,           10,
  "mann",          "venedig",               5,           0,
  "mann",          "zauberberg",            14,           82,
  "musil",         "verwirrungen",          16,           44,
  "schnitzler",     "casanova",              14,          89, 
  "zweig",         "liebe",                 51,           110
  
  
)

clean_text <- function(path, trim_start = 0, trim_end = 0) {
  lines <- read_lines(path)
  
  start_idx <- grep("^\\*\\*\\* START OF .*PROJECT GUTENBERG EBOOK", lines)
  end_idx   <- grep("^\\*\\*\\* END OF .*PROJECT GUTENBERG EBOOK", lines)
  
  start_pos <- if (length(start_idx) > 0) start_idx[1] + 1 else 1
  end_pos   <- if (length(end_idx) > 0) end_idx[1] - 1 else length(lines)
  
  lines <- lines[start_pos:end_pos]
  lines <- lines[str_trim(lines) != ""]
  
  if (trim_start > 0 && length(lines) > trim_start) {
    lines <- lines[(trim_start + 1):length(lines)]
  }
  
  if (trim_end > 0 && length(lines) > trim_end) {
    lines <- lines[1:(length(lines) - trim_end)]
  }
  
  lines
}

segment_text <- function(lines, segment_size = 10000) {
  text <- paste(lines, collapse = " ")
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)
  
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

meta <- left_join(meta, trim_table, by = c("author", "text"))

meta$trim_start[is.na(meta$trim_start)] <- 0
meta$trim_end[is.na(meta$trim_end)] <- 0

for (i in seq_len(nrow(meta))) {
  txt <- clean_text(
    path = meta$path[i],
    trim_start = meta$trim_start[i],
    trim_end = meta$trim_end[i]
  )
  
  clean_file <- file.path(
    clean_dir,
    paste0(meta$author[i], "_", meta$text[i], ".txt")
  )
  
  write_lines(txt, clean_file)
  
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

