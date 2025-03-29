
# DIPSAUS DEBUG START
# schema <- load_schema_file()
# glossary_list = compile_schema_glossary(schema)
# entity_identifiers = compile_file_entity_identifier(schema)


#' @export
print.bids_schema_raw <- function(x, ...) {
  cat(sprintf("<BIDS specification> `%s` (schema version: `%s`)\n", x$bids_version, x$schema_version))
}

#' @export
`[.bids_schema_raw` <- function(x, i, ..., drop = TRUE) {
  if(missing(i)) { return(x) }
  keys <- c(i, ...)

  re <- structure(
    names = keys,
    lapply(strsplit(keys, ".", fixed = TRUE), function(key_i) {
      if(anyNA(key_i)) { return(NULL) }
      re <- x
      for(k in key_i) {
        re <- re[[k]]
      }
      re
    })
  )

  if(length(keys) == 1 && drop) {
    re <- re[[1]]
  }

  re
}

# ---- transform and compile the schema ----------------------------------------

get_schema_entity_discription <- function(entity_schema_name, schema) {
  entity_schema <- schema$objects$entities[[entity_schema_name]]
  if(!length(entity_schema) || length(entity_schema$name) != 1) { return(NULL) }
  entity_schema
}

compile_schema_glossary <- function(schema) {

  # compile these tables
  # data_type_table <- utils::read.csv(system.file("definitions", "glossary", "DataType.csv", package = "bidsr"))
  # suffix_table <- utils::read.csv(system.file("definitions", "glossary", "Suffix.csv", package = "bidsr"))

  data_type_table <- data.table::rbindlist(lapply(schema["objects.datatypes"], function(item) {
    value <- item$value

    nvals <- length(value)
    if (!nvals) {
      return()
    }

    display_name <- trimws(paste(item$display_name, collapse = ""))
    description <- trimws(paste(item$description, collapse = ""))

    data.table::data.table(Name = value,
                           FullName = display_name,
                           Description = description)
  }))

  suffix_table <- data.table::rbindlist(lapply(schema["objects.suffixes"], function(item) {
    value <- item$value

    nvals <- length(value)
    if (!nvals) {
      return()
    }

    display_name <- trimws(paste(item$display_name, collapse = ""))
    description <- trimws(paste(item$description, collapse = ""))

    data.table::data.table(Name = value,
                           FullName = display_name,
                           Description = description)
  }))

  entity_names <- schema$rules$entities

  entity_table <- data.table::rbindlist(lapply(seq_along(entity_names), function(ii) {
    name <- entity_names[[ii]]
    entity_schema <- as.list(get_schema_entity_discription(name, schema = schema))

    key <- entity_schema$name
    if(!length(key)) { key <- name }

    type <- entity_schema$type
    format <- entity_schema$format

    display_name <- trimws(paste(entity_schema$display_name, collapse = ""))
    description <- trimws(paste(entity_schema$description, collapse = ""))
    list(
      Order = ii,
      Key = key,
      Type = type,
      Format = format,
      Name = name,
      FullName = display_name,
      Description = I(list(description))
    )
  }))

  list(
    data_type_table = data_type_table,
    suffix_table = suffix_table,
    entity_table = entity_table
  )
}

compile_file_entity_identifier <- function(schema) {
  schema_keys <- names(unlist(schema))
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

  # calculate identifier
  file_rules <- new.env(parent = emptyenv())
  identifier_map <- data.table::rbindlist(lapply(schema_key_prefix, function(key) {
    # key <- sample(schema_key_prefix, 1)
    rule <- schema[key]

    if(!length(rule)) { return() }
    if(!length(rule$suffixes) || !length(rule$datatypes)) { return(NULL) }

    tbl <- expand.grid(rule$datatypes, rule$suffixes, stringsAsFactors = FALSE)
    id <- tolower(sprintf("%s/%s", tbl[[1]], tbl[[2]]))

    data.table::data.table(
      schema_prefix = key,
      identifier = id,
      datatype = tbl[[1]],
      suffix = tbl[[2]]
    )
  }))
  identifier_map
}

compile_related_entity_requirement_list <- function(
    schema, glossary_list = compile_schema_glossary(schema),
    entity_identifiers = compile_file_entity_identifier(schema)) {

  schema_key_prefix <- entity_identifiers$schema_prefix
  identifiers <- sort(unique(entity_identifiers$identifier))

  # ---- 2. for each schema key, compile entity to get rules
  #         if there are multiple rules applied to the same identifier, get a common rule that fit all
  entity_params <- structure(
    names = identifiers,
    lapply(identifiers, function(identifier) {
      # identifier <- identifiers[[1]]
      schema_keys <- entity_identifiers$schema_prefix[entity_identifiers$identifier == identifier]
      schema_keys <- unique(schema_keys)

      n_keys <- length(schema_keys)

      rules <- schema[schema_keys, drop = FALSE]

      common_entities <- new.env(parent = emptyenv())

      lapply(rules, function(rule) {
        entities <- rule$entities
        lapply(names(entities), function(nm) {
          common_entities[[nm]] <- c(common_entities[[nm]], entities[[nm]])
          NULL
        })
        NULL
      })

      common_entities <- as.list(common_entities)
      entity_names <- names(common_entities)

      # order entity_names
      reserved_entity_names <- glossary_list$entity_table$Name[glossary_list$entity_table$Name %in% entity_names]
      custom_entity_names <- sort(entity_names[!entity_names %in% reserved_entity_names])

      entity_names <- c(reserved_entity_names, custom_entity_names)

      entities <- structure(
        names = entity_names,
        lapply(entity_names, function(entity_name) {
          req <- common_entities[[entity_name]]
          n <- length(req)

          # BIDS_ENTITY_REQUIREMENT_OPTIONS <- c("optional", "required", "prohibited")
          # get requirement option
          req <- unique(req)
          if(length(req) != 1 || n < n_keys) {
            req <- "optional"
          }

          req
        })
      )

      list(
        identifier = identifier,
        schema_keys = schema_keys,
        common_entities = entities,
        description = "The common entity rules are relaxed version if there are multiple `schema_keys`"
      )
    })
  )

  entity_params
}


compile_schema <- function(bids_version) {
  # bids_version <- "1.10.1"
  # load schema, set class
  schema_file <- system.file("bids-schema", sprintf("schema-%s.json", bids_version), package = "bidsr")
  schema_raw <- structure(from_json(file = schema_file), class = c("bids_schema_raw"))

  # compile schema
  glossary_list <- compile_schema_glossary(schema_raw)
  entity_identifiers <- compile_file_entity_identifier(schema_raw)
  entity_requirement_list <- compile_related_entity_requirement_list(
    schema = schema_raw,
    glossary_list = glossary_list,
    entity_identifiers = entity_identifiers
  )

  compiled <- structure(
    class = "bids_schema_compiled",
    list(
      schema_version = schema_raw$schema_version,
      bids_version = schema_raw$bids_version,
      glossary = glossary_list,
      entity = list(
        identifiers = entity_identifiers,
        requirements = entity_requirement_list
      )
    )
  )

  list(
    original = schema_raw,
    compiled = compiled
  )
}


