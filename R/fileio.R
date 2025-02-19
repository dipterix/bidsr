read_tsv <- function(file, header = TRUE, sep = "\t", na.strings = "n/a", stringsAsFactors = FALSE, ...) {
  data.table::fread(input = file, header = header, sep = sep, na.strings = na.strings, stringsAsFactors = stringsAsFactors, ...)
}

read_csv <- function(file, header = TRUE, sep = ",", na.strings = "n/a", stringsAsFactors = FALSE, ...) {
  data.table::fread(input = file, header = header, sep = sep, na.strings = na.strings, stringsAsFactors = stringsAsFactors, ...)
}

write_tsv <- function(x, file, sep = '\t', dec = ".", na = "n/a", row.names = FALSE, col.names = TRUE, ...) {
  data.table::fwrite(x = x, file = file, sep = sep, dec = dec, na = na, row.names = row.names, col.names = col.names, ...)
}

write_csv <- function(x, file, sep = ',', dec = ".", na = "n/a", row.names = FALSE, col.names = TRUE, ...) {
  data.table::fwrite(x = x, file = file, sep = sep, dec = dec, na = na, row.names = row.names, col.names = col.names, ...)
}


from_json <- function(json_str, file, ...) {
  if(missing(json_str)) {
    suppressWarnings({
      json_str <- readLines(file)
    })
  }
  jsonlite::fromJSON(txt = json_str, simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE, ...)
}

to_json <- function(x, indent = 0, dataframe = "columns", null = "null", na = "null",
                    auto_unbox = TRUE, digits = getOption("shiny.json.digits", I(16)),
                    use_signif = inherits(digits, "AsIs"), force = TRUE,
                    POSIXt = "ISO8601", UTC = TRUE, rownames = FALSE,
                    keep_vec_names = TRUE, strict_atomic = TRUE) {
  jsonlite::toJSON(x = x, dataframe = dataframe, null = null, na = na, auto_unbox = auto_unbox,
                   digits = digits, force = force, POSIXt = POSIXt,
                   UTC = UTC, rownames = rownames, keep_vec_names = keep_vec_names, pretty = indent)
}
