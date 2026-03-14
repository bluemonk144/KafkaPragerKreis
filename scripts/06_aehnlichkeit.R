# ------------------------------------------------
# load full corpus from stylo_corpus
# ------------------------------------------------

library(quanteda)
library(quanteda.textstats)

files <- list.files("stylo_corpus", full.names = TRUE, pattern = "\\.txt$")
bn <- basename(files)

# ------------------------------------------------
# balance Kafka: keep only one segment per work
# ------------------------------------------------

# Prague authors
prague_files <- files[grepl("^(kafka|brod|meyrink|werfel|rilke|leppin)_", bn)]
prague_bn <- basename(prague_files)

# Kafka files only
kafka_files <- prague_files[grepl("^kafka_", prague_bn)]
kafka_bn <- basename(kafka_files)

# remove final segment number, e.g. kafka_prozess_03.txt -> kafka_prozess
kafka_work <- sub("_[0-9]+\\.txt$", "", kafka_bn)

# keep only first segment of each Kafka work
kafka_keep_idx <- !duplicated(kafka_work)
kafka_selected <- kafka_files[kafka_keep_idx]

# all other Prague authors unchanged
other_prague <- prague_files[!grepl("^kafka_", prague_bn)]

# non-Prague authors unchanged
nonprague_files <- files[grepl("^(mann|hesse|musil|schnitzler|zweig)_", bn)]

# final selection
selected_files <- c(kafka_selected, other_prague, nonprague_files)
selected_bn <- basename(selected_files)

# ------------------------------------------------
# read selected files
# ------------------------------------------------

texts <- sapply(selected_files, function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  paste(x, collapse = " ")
}, USE.NAMES = TRUE)

corp <- corpus(texts, docnames = basename(selected_files))

# ------------------------------------------------
# create Prague / non-Prague grouping variable
# ------------------------------------------------

docvars(corp, "group") <- ifelse(
  grepl("^(kafka|brod|meyrink|werfel|rilke|leppin)_", basename(selected_files)),
  "prague",
  "nonprague"
)

# optional: author variable
docvars(corp, "author") <- sub("_.*$", "", basename(selected_files))

# ------------------------------------------------
# check resulting corpus composition
# ------------------------------------------------

table(docvars(corp, "group"))
table(docvars(corp, "author"))

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






keyness_prague <- textstat_keyness(
  dfm_fw,
  target = docvars(corp, "group") == "prague"
)

keyness_nonprague <- textstat_keyness(
  dfm_fw,
  target = docvars(corp, "group") == "nonprague"
)

head(keyness_prague, 20)
head(keyness_nonprague, 20)




# ------------------------------------------------
# sentence segmentation
# ------------------------------------------------

sentences <- tokens(corp, what = "sentence")

# calculate sentence lengths (number of tokens)
sent_lengths <- lapply(sentences, function(x) {
  lengths(tokens(x))
})

# mean sentence length per document
mean_sent_length <- sapply(sent_lengths, mean)

# add variable to corpus metadata
docvars(corp, "mean_sentence_length") <- mean_sent_length

# compare groups
aggregate(mean_sentence_length ~ group,
          data = docvars(corp),
          FUN = mean)


# calculate sentence length variance per document
var_sent_length <- sapply(sent_lengths, var)

# store in corpus metadata
docvars(corp, "var_sentence_length") <- var_sent_length

# compare groups
aggregate(var_sentence_length ~ group,
          data = docvars(corp),
          FUN = mean)




# ------------------------------------------------
# punctuation profile analysis
# ------------------------------------------------

# tokenize texts while keeping punctuation
toks_punct <- tokens(
  corp,
  remove_punct = FALSE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

# build document-feature matrix
dfm_punct <- dfm(toks_punct)

# select punctuation marks of interest
dfm_punct_subset <- dfm_select(
  dfm_punct,
  pattern = c(",", ".", ";", ":", "!", "?", "-", "—", "–")
)

# aggregate punctuation by group
dfm_punct_group <- dfm_group(
  dfm_punct_subset,
  groups = docvars(corp, "group")
)

# compute relative frequencies
dfm_punct_prop <- dfm_weight(
  dfm_punct_group,
  scheme = "prop"
)

# inspect punctuation profile
convert(dfm_punct_prop, to = "data.frame")
topfeatures(dfm_punct_prop)