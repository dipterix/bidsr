register_file_schema_key <- function(bids_version) {

  # DIPSAUS DEBUG START
  # bids_version <- current_bids_version()
  schema_raw <- load_schema_file(bids_version = bids_version)

  # schema_raw$rules$files$raw$anat$nonparametric

  # expand all names
  schema_keys <- names(unlist(schema_raw))
  schema_keys <- schema_keys[startsWith(schema_keys, "rules.files")]
  schema_keys <- unique(schema_keys)
  schema_keys <- schema_keys[grepl("\\.(suffixes|extensions|datatypes|entities)", schema_keys)]


  schema_key_prefix <- lapply(strsplit(schema_keys, ".", fixed = TRUE), function(x) {
    if(length(x) < 5) { return(NULL) }
    desc_idx <- grepl("^(suffixes|extensions|datatypes|entities)", x)
    if(!any(desc_idx)) { return(NULL) }
    desc_idx <- which(desc_idx)
    desc_idx <- desc_idx[desc_idx >= 4]
    if(!length(desc_idx)) { return(NULL) }
    paste(x[seq_len(desc_idx[[1]] - 1)], collapse = ".")
  })

  schema_key_prefix <- unique(unlist(schema_key_prefix))

  length(schema_key_prefix)

  # now get the rules
  has_rules <- vapply(schema_key_prefix, function(key) {
    length(get_schema_file_entity_rules(key)) > 0
  }, FALSE)

  mean(has_rules)

  schema_key_prefix <- schema_key_prefix[has_rules]

  rules <- list()
  for(key in schema_key_prefix) {
    rule <- get_schema_file_entity_rules(key)
    tbl <- expand.grid(rule$datatypes, rule$suffixes)
    id <- sprintf("%s/%s", tbl[[1]], tbl[[2]])
    dups <- id[id %in% names(rules)]
    rule$key <- key
    if(length(dups)) {
      existing <- rules[dups[[1]]]
      testthat::expect_equal(rule$entities, existing$entities)
    }
    for(ii in id[!id %in% dups]) {
      rules[[ii]] <- rule
    }
  }
  rules[[dups]]$key
  rule$key


  m <- stringr::str_split(schema_keys, pattern = "\\.", simplify = TRUE, n = 7)
  m <- m[grepl("(suffixes|extensions|datatypes|entities)", m[,6]), ]

  all_keys <- unique(apply(m[, 1:5], 1, paste, sep = ".", collapse = "."))


  vapply(all_keys, function(key){
    get_schema_entity_rule(key)
  }, F)
}
