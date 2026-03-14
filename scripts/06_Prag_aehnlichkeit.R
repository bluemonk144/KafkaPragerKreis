# ------------------------------------------------
# load full corpus from stylo_corpus
# ------------------------------------------------

library(quanteda)
library(quanteda.textstats)

files <- list.files("stylo_corpus", full.names = TRUE, pattern = "\\.txt$")

texts <- sapply(files, function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  paste(x, collapse = " ")
}, USE.NAMES = TRUE)

corp <- corpus(texts, docnames = basename(files))

# ------------------------------------------------
# create Prague / non-Prague grouping variable
# ------------------------------------------------

docvars(corp, "group") <- ifelse(
  grepl("^(kafka|brod|meyrink|werfel|rilke|leppin)_", basename(files)),
  "prague",
  "nonprague"
)

# ------------------------------------------------
# tokenization
# ------------------------------------------------

toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks <- tokens_tolower(toks)

# remove obvious artifacts
toks <- tokens_remove(
  toks,
  pattern = c("^.$", "^<+$", "^>+$", "^~+$"),
  valuetype = "regex"
)

# ------------------------------------------------
# keep only function words / stopwords
# ------------------------------------------------

toks_fw <- tokens_select(
  toks,
  pattern = stopwords("de"),
  selection = "keep"
)

dfm_fw <- dfm(toks_fw)

# optional: remove very rare features
dfm_fw <- dfm_trim(dfm_fw, min_termfreq = 5)

# ------------------------------------------------
# keyness analysis: Prague vs non-Prague
# ------------------------------------------------

keyness_fw <- textstat_keyness(
  dfm_fw,
  target = docvars(corp, "group") == "prague"
)

head(keyness_fw, 40)