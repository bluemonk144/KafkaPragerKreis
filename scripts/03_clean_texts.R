# trim to limit tokens


library(stringr)
library(readr)
library(dplyr)

clean_dir <- "clean_texts"
segments_dir <- "segments"
corpus_dir <- "stylo_corpus"

dir.create(segments_dir, showWarnings = FALSE)
dir.create(corpus_dir, showWarnings = FALSE)

files <- list.files(
  path = clean_dir,
  recursive = FALSE,
  full.names = TRUE
)

files <- files[!dir.exists(files)]
files <- files[str_detect(files, "\\.txt$")]

meta <- data.frame(
  path = files,
  stringsAsFactors = FALSE
)

meta$file <- basename(meta$path)
meta$text <- tools::file_path_sans_ext(meta$file)
meta$author <- sub("_.*", "", meta$text)

# manual corpus settings
corpus_table <- tribble(
  ~author,       ~text,                       ~include, ~start_word, ~max_words,
  "brod",        "brod_Arnold_Beer",          TRUE,     1,           21000,
  "brod",        "brod_gespenstergeschichte", TRUE,     1,           11000,
  "brod",        "brod_gottin",               TRUE,     1,           21000,
  "hesse",       "hesse_siddharta",           TRUE,     1,           21000,
  "hesse",       "hesse_steppenwolf",         TRUE,     1,           31000,
  "kafka",       "kafka_das_urteil",          FALSE,    1,           50000,
  "kafka",       "kafka_der_bau",             TRUE,     1,           50000,
  "kafka",       "kafka_landartzt",           TRUE,     1,           50000,
  "kafka",       "kafka_prozess",             TRUE,     1,           60000,
  "kafka",       "kafka_strafkolonie",        TRUE,     1,           50000,
  "kafka",       "kafka_verwandlung",         TRUE,     1,           50000,
  "leppin",      "leppin_Severin",            TRUE,     1,           50000,
  "leppin",      "leppin_blaugast",            TRUE,     1,           50000,
  "mann",        "mann_venedig",              TRUE,     1,           35000,
  "mann",        "mann_zauberberg",           TRUE,     1,           31000,
  "meyrink",     "meyrink_der_golem",         TRUE,     1,           51000,
  "musil",       "musil_verwirrungen",        TRUE,     1,           51000,
  "rilke",       "rilke_aufzeichnungen",      TRUE,     1,           51000,
  "schnitzler",  "schnitzler_casanova",       TRUE,     1,           51000,
  "werfel",      "werfel_ermoredete",         TRUE,     1,           51000,
  "zweig",       "zweig_liebe",               TRUE,     1,           51000
)

meta <- left_join(meta, corpus_table, by = c("author", "text"))

meta$include[is.na(meta$include)] <- TRUE
meta$start_word[is.na(meta$start_word)] <- 1
meta$max_words[is.na(meta$max_words)] <- 50000

prepare_text <- function(path, start_word = 1, max_words = 50000) {
  txt <- read_file(path)
  txt <- str_replace_all(txt, "\\s+", " ")
  txt <- str_trim(txt)
  
  words <- str_split(txt, "\\s+")[[1]]
  
  if (start_word > length(words)) {
    return(character(0))
  }
  
  end_word <- min(length(words), start_word + max_words - 1)
  words <- words[start_word:end_word]
  
  words
}

segment_words <- function(words, segment_size = 10000) {
  n_segments <- floor(length(words) / segment_size)
  
  segments <- vector("list", n_segments)
  
  for (i in seq_len(n_segments)) {
    start <- (i - 1) * segment_size + 1
    end <- i * segment_size
    segments[[i]] <- paste(words[start:end], collapse = " ")
  }
  
  segments
}

# optional: clear old outputs
unlink(list.files(segments_dir, full.names = TRUE), recursive = TRUE)
unlink(list.files(corpus_dir, full.names = TRUE), recursive = TRUE)

for (i in seq_len(nrow(meta))) {
  
  if (!meta$include[i]) next
  
  words <- prepare_text(
    path = meta$path[i],
    start_word = meta$start_word[i],
    max_words = meta$max_words[i]
  )
  
  if (length(words) == 0) next
  
  segments <- segment_words(words, segment_size = 10000)
  
  if (length(segments) == 0) next
  
  for (j in seq_along(segments)) {
    seg_name <- paste0(
      meta$text[i], "_",
      sprintf("%02d", j),
      ".txt"
    )
    
    write_file(segments[[j]], file.path(segments_dir, seg_name))
    write_file(segments[[j]], file.path(corpus_dir, seg_name))
  }
}