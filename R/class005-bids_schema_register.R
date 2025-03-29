# DIPSAUS DEBUG START
# schema <- bids_schema()
# schema_compiled <- schema$compiled

get_schema_entity_rule <- function(identifier = NULL, schema_key = NA, bids_version = current_bids_version()) {
  schema_combo <- bids_schema(bids_version = bids_version)
  entity_table <- schema_combo$compiled$glossary$entity_table

  # query using `identifier` (datatype/suffix)
  if( length(identifier) ) {
    rules <- schema_combo$compiled$entity$requirements[[tolower(identifier)]]
    rules <- as.list(rules)
  } else {
    rules <- list()
  }

  # query using schema_key
  # usually you don't need to have both identifier and schema_key
  # but schema_key is fine grained approach
  if(!is.na(schema_key)) {
    # make sure rules and schema key are compatible
    if(length(rules) > 0 && !isTRUE(schema_key %in% rules$schema_keys)) {
      stop(
        "Cannot create BIDS entity rules property for `", paste(identifier, collapse = ""),
        "`: Invalid `schema_key`. The `schema_key` needs to be one of the followings:\n",
        paste("  - ", rules$schema_keys, collapse = "\n")
      )
    }
    rules <- as.list(schema_combo$original[schema_key])
    identifier <- as.vector(outer(rules$datatypes, rules$suffixes, function(a, b) { sprintf("%s/%s", a, b) }))
    identifier <- tolower(identifier)

    entities <- rules$entities
    entity_names <- names(entities)
    reserved <- entity_table$Name[entity_table$Name %in% entity_names]
    custom <- entity_names[!entity_names %in% reserved]
    entity_names <- c(reserved, custom)

    rules <- list(
      identifier = identifier,
      schema_keys = schema_key,
      common_entities = entities[entity_names]
    )
  }

  if(length(rules) && length(rules$common_entities)) {
    # rules or schema_key is working
    entity_names <- names(rules$common_entities)
    entity_keys <- structure(names = entity_table$Name, entity_table$Key)[entity_names]
    entity_format <- structure(names = entity_table$Name, entity_table$Format)[entity_names]
    entity_required <- structure(
      names = entity_names,
      vapply(rules$common_entities, function(x) { if(length(x)) { x[[1]] } else { "optional" }}, "")
    )

    combined_rules <- structure(
      names = entity_keys,
      as.list(as.data.frame(rbind(entity_required, entity_format)))
    )
  } else {
    # not found, using default
    combined_rules <- structure(
      names = entity_table$Key,
      as.list(entity_table$Format)
    )
  }

  list(
    identifiers = identifier,
    schema_keys = rules$schema_key,
    entities = combined_rules
  )

}
