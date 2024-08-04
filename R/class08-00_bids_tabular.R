bids_tabular_column_descriptor <- S7::new_class(
  name = "bids_tabular_column_descriptor",
  parent = bids_object,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    LongName = bids_optional(class = S7::class_character),
    Description = make_collapsed_string_property(name = "Description"),
    Levels = property_named_list,
    Units = bids_optional(class = S7::class_character),
    Delimiter = bids_optional(class = S7::class_character),
    TermURL = bids_optional(class = S7::class_character),
    HED = S7::new_property(class = S7::class_character)
  )
)

#' @examples
#' #'
#' test_dscr <- bids_tabular_colname(
#'   name = "test",
#'   descriptors = list(
#'     LongName = "Education level",
#'     Description = "Self-rated by participant",
#'     Levels = list(
#'       `1` = "Finished primary school",
#'       `2` = "Finished secondary school",
#'       `3` = "Student at university",
#'       `4` = "Has degree from university"
#'     )
#'   )
#' )
#'
#' bmi_dscr <- bids_tabular_colname(
#'   name = "bmi",
#'   descriptors = list(
#'     LongName = "Body mass index",
#'     Units = "kg/m^2",
#'     TermURL = "https://..."
#'   )
#' )
#'
#' header <- bids_tabular_header(columns = list(test_dscr, bmi_dscr))
#'
#' # access the header components
#' header$columns$test$descriptors$LongName
#' header$columns$bmi$descriptors$Units
#'
#' # Check JSON format
#' cat(as.character(header))
#'
#' @export
bids_tabular_colname <- S7::new_class(
  name = "bids_tabular_colname",
  parent = bids_object,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    name = property_nonempty_string,
    descriptors = S7::new_property(
      bids_tabular_column_descriptor,
      setter = function(self, value) {
        if(!inherits(value, "bidsr::bids_tabular_column_descriptor")) {
          value <- do.call(bids_tabular_column_descriptor, value)
        }
        self@descriptors <- value
        self
      }
    )
  )
)

#' @export
bids_tabular_header <- S7::new_class(
  name = "bids_tabular_header",
  parent = bids_object,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    columns = S7::new_property(
      class = S7::class_list,
      setter = function(self, value) {
        value <- lapply(value, function(item) {
          S7::check_is_S7(item, bids_tabular_colname,
                          arg = "@columns items")
          item
        })
        nms <- vapply(value, S7::prop, "", name = "name")
        self@columns <- structure(names = nms, value)
        self
      }
    ),
    format = property_format(function(self) {
        nms <- names(self@columns)
        li <- structure(
          names = nms,
          lapply(self@columns, function(col) {
            as.list(col@descriptors)
          })
        )
        rjson::toJSON(li, indent = json_indent())
      }
    )
  )
)

#' @export
bids_tabular_data <- S7::new_class(
  name = "bids_tabular_data",
  parent = bids_object,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    header = S7::new_property(class = bids_tabular_header),
    data = S7::new_property(class = S7::class_data.frame),
    format = property_format(function(self) {
      s <- utils::capture.output({
        if(nrow(self@data) >= 10) {
          print(utils::head(self@data, n = 3L))
          cat("...\n")
          print(utils::tail(self@data, n = 3L))
        } else {
          print(self@data)
        }
      })
      paste(s, collapse = "\n")
    }),
    print = property_print_format
  )
)

#' @export
`as.data.frame.bidsr::bids_tabular_data` <- function(x, ...) {
  x@data
}

#' @export
bids_read_tabular <- function(file, ..., header_only = FALSE, as_class = bids_tabular_data) {
  if(length(file) != 1) {
    stop(sprintf("Function `bids_read_tabular` must have one `file` path"))
  }
  prefix <- gsub("\\.(tsv|json)$", "", file, ignore.case = TRUE)
  json_path <- correct_filepath(sprintf("%s.json", prefix))
  tsv_path <- correct_filepath(sprintf("%s.tsv", prefix))

  stopifnot(file_exists(tsv_path))

  header <- bids_tabular_header()
  if(!is.na(json_path)) {
    json <- rjson::fromJSON(file = json_path)
    header@columns <- lapply(names(json), function(nm) {
      bids_tabular_colname(name = nm, descriptors = json[[nm]])
    })
  }
  if( header_only ) { return(header) }

  suppressWarnings({
    tbl <- utils::read.csv(file = tsv_path, header = TRUE, sep = "\t", na.strings = "n/a")
  })

  as_class(header = header, data = tbl)
}

#' @export
bids_write_tabular <- S7::new_generic(
  name = "bids_write_tabular",
  dispatch_args = c("x", "file")
)


S7::method(
  bids_write_tabular,
  list(
    x = S7::new_S3_class("data.frame"),
    file = S7::class_any
  )
) <- function(x, file, ..., quote = FALSE) {
  if( inherits(file, "connection") ) {
    conn <- file
    prefix <- file
  } else {
    prefix <- gsub("\\.(tsv|json)$", "", file)
    conn <- sprintf("%s.tsv", prefix)
  }
  utils::write.table(
    x = x,
    file = conn,
    sep = "\t",
    na = "n/a",
    row.names = FALSE,
    quote = quote,
    col.names = TRUE,
    ...
  )
  invisible(prefix)
}

S7::method(
  bids_write_tabular,
  list(
    x = bids_tabular_data,
    file = S7::new_S3_class("character")
  )
) <- function(x, file, ..., quote = FALSE) {
  prefix <- gsub("\\.(tsv|json)$", "", file)

  if(length(x@header@columns) > 0) {
    writeLines(
      format(x@header),
      con = sprintf("%s.json", prefix)
    )
  }

  utils::write.table(
    x = as.data.frame(x),
    file = sprintf("%s.tsv", prefix),
    sep = "\t",
    na = "n/a",
    row.names = FALSE,
    quote = quote,
    col.names = TRUE,
    ...
  )

  invisible(prefix)
}


new_bids_builtin_tabular <- function(x, header, as_class, default_descriptors = list()) {
  # DIPSAUS DEBUG START
  # x <- data.frame(
  #   participant_id = "sub-01",
  #   age = 66,
  #   species = "homo sapiens"
  # )

  if( S7::S7_inherits(x, as_class) ) {
    impl <- x
  } else if ( S7::S7_inherits(x, bids_tabular_data) ) {
    impl <- as_class(header = x@header, data = x@data)
  } else if( is.character(x) ) {
    # x is path to the file
    impl <- bids_read_tabular(x, as_class = as_class)
  } else {
    impl <- as_class(data = x)
  }

  header_names <- names(impl@data)

  if(length(header)) {
    S7::check_is_S7(header, bids_tabular_header)
  } else {
    header <- bids_tabular_header()
  }

  header@columns <- drop_nulls(lapply(header_names, function(nm) {
    descr <- header@columns[[nm]]
    if (!is.null(descr)) {
      return(descr)
    }
    descr <- default_descriptors[[nm]]
    if (!is.null(descr)) {
      return(descr)
    }
    bids_tabular_colname(name = nm,
                         descriptors = list(Description = "no description"))
  }))
  impl@header <- header

  # There might be some active setter (e.g. participants), trygger setter modification
  impl@data <- impl@data

  impl
}
