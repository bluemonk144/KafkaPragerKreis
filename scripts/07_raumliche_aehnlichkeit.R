# ------------------------------------------------
# define spatial lexicon
# ------------------------------------------------

interior_words <- c(
  "zimmer","raum","stube","saal","gang","flur",
  "tür","bett","fenster","treppe","korridor"
)

exterior_words <- c(
  "straße","strasse","gasse","platz","hof",
  "haus","tor","brücke","bruecke","ufer",
  "weg","stadt","markt"
)

spatial_words <- c(interior_words, exterior_words)


# ------------------------------------------------
# tokenization
# ------------------------------------------------

toks_words <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks_words <- tokens_tolower(toks_words)

# ------------------------------------------------
# build dfm
# ------------------------------------------------

dfm_words <- dfm(toks_words)

# ------------------------------------------------
# select spatial lexicon
# ------------------------------------------------

dfm_spatial <- dfm_select(
  dfm_words,
  pattern = spatial_words
)

# ------------------------------------------------
# aggregate by group
# ------------------------------------------------

dfm_spatial_group <- dfm_group(
  dfm_spatial,
  groups = docvars(corp, "group")
)

convert(dfm_spatial_group, to = "data.frame")

# ------------------------------------------------
# relative frequencies
# ------------------------------------------------

dfm_spatial_prop <- dfm_weight(
  dfm_spatial_group,
  scheme = "prop"
)

convert(dfm_spatial_prop, to = "data.frame")






# ------------------------------------------------
# create Kafka vs other Prague grouping
# ------------------------------------------------

prague_corp <- corpus_subset(corp, group == "prague")

docvars(prague_corp, "subgroup") <- ifelse(
  docvars(prague_corp, "author") == "kafka",
  "kafka",
  "other_prague"
)

table(docvars(prague_corp, "subgroup"))
table(docvars(prague_corp, "author"))

# ------------------------------------------------
# tokenization
# ------------------------------------------------

toks_words_prague <- tokens(
  prague_corp,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks_words_prague <- tokens_tolower(toks_words_prague)

# ------------------------------------------------
# build dfm
# ------------------------------------------------

dfm_words_prague <- dfm(toks_words_prague)

# ------------------------------------------------
# select spatial lexicon
# ------------------------------------------------

dfm_spatial_prague <- dfm_select(
  dfm_words_prague,
  pattern = spatial_words
)

# ------------------------------------------------
# aggregate by Kafka vs other Prague
# ------------------------------------------------

dfm_spatial_subgroup <- dfm_group(
  dfm_spatial_prague,
  groups = docvars(prague_corp, "subgroup")
)

# absolute frequencies
convert(dfm_spatial_subgroup, to = "data.frame")

# relative frequencies
dfm_spatial_subgroup_prop <- dfm_weight(
  dfm_spatial_subgroup,
  scheme = "prop"
)

convert(dfm_spatial_subgroup_prop, to = "data.frame")

# ------------------------------------------------
# interior vs exterior
# ------------------------------------------------

dfm_interior_prague <- dfm_select(dfm_words_prague, pattern = interior_words)
dfm_exterior_prague <- dfm_select(dfm_words_prague, pattern = exterior_words)

interior_subgroup <- dfm_group(
  dfm_interior_prague,
  groups = docvars(prague_corp, "subgroup")
)

exterior_subgroup <- dfm_group(
  dfm_exterior_prague,
  groups = docvars(prague_corp, "subgroup")
)

# relative frequencies within each lexicon
interior_subgroup_prop <- dfm_weight(interior_subgroup, scheme = "prop")
exterior_subgroup_prop <- dfm_weight(exterior_subgroup, scheme = "prop")

convert(interior_subgroup_prop, to = "data.frame")
convert(exterior_subgroup_prop, to = "data.frame")