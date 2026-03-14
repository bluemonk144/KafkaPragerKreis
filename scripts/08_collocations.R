# ------------------------------------------------
# Prague corpus
# ------------------------------------------------

prague_corp <- corpus_subset(corp, group == "prague")

toks_prague <- tokens(
  prague_corp,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks_prague <- tokens_tolower(toks_prague)

# detect collocations
coll_prague <- textstat_collocations(
  toks_prague,
  size = 2:3,
  min_count = 5
)

# spatial filter
spatial_pattern <- paste0("\\b(", paste(spatial_words, collapse="|"), ")\\b")

coll_prague_spatial <- coll_prague[
  grepl(spatial_pattern, coll_prague$collocation),
]

# sort
coll_prague_spatial <- coll_prague_spatial[
  order(-coll_prague_spatial$lambda),
]

head(coll_prague_spatial, 30)


# ------------------------------------------------
# non-Prague corpus
# ------------------------------------------------

nonprague_corp <- corpus_subset(corp, group == "nonprague")

toks_np <- tokens(
  nonprague_corp,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks_np <- tokens_tolower(toks_np)

# detect collocations
coll_np <- textstat_collocations(
  toks_np,
  size = 2:3,
  min_count = 5
)

# spatial filter
coll_np_spatial <- coll_np[
  grepl(spatial_pattern, coll_np$collocation),
]

# sort
coll_np_spatial <- coll_np_spatial[
  order(-coll_np_spatial$lambda),
]

head(coll_np_spatial, 30)