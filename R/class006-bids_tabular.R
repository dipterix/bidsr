BIDS_TABULAR_COLUME_DESCRIPTOR_FIELDS <- c("LongName", "Description", "Levels", "Units", "Delimiter", "TermURL", "HED")

bids_tabular_constuctor <- function(content, meta = NULL) {
  S7::new_object(
    S7::S7_object(),
    content = data.table::as.data.table(content),
    meta = as_bids_tabular_meta(meta)
  )
}

as_bids_tabular_meta <- function(meta = NULL, ...) {
  more <- list(...)
  if( !S7::S7_inherits(meta, bids_tabular_meta_sidecar) ) {
    if(isTRUE(is.character(meta))) {
      meta <- paste(meta, collapse = "\n")
      # path or json
      if(endsWith(tolower(meta), ".json")) {
        meta <- from_json(file = meta)
      } else if (startsWith(trimws(meta), "{")){
        meta <- from_json(json_str = meta)
      } else {
        stop("Unable to parse meta: ", meta)
      }
    }
    meta <- c(as.list(meta), more)
    meta <- bids_tabular_meta_sidecar(columns = meta)
  } else {
    if(length(more)) {
      meta$columns[names(more)] <- more
    }
  }
  meta
}

as_bids_tabular_table <- function(x, meta, ..., cls = NULL) {
  x <- data.table::as.data.table(x)
  if(missing(meta)) {
    meta <- NULL
  }
  meta <- as_bids_tabular_meta(meta = meta, ...)
  if(all(inherits(cls, c("bids_tabular_definition", "S7_class"), which = TRUE) > 0)) {
    re <- cls(content = x, meta = meta)
  } else {
    re <- bids_tabular(content = x, meta = meta)
  }
  re
}


#' @name bids_tabular
#' @title Class definitions and utilities for 'BIDS' tabular
#' @description
#' Official specification link:
#' \url{https://bids-specification.readthedocs.io/en/stable/common-principles.html#tabular-files}
#' @param content a data frame or table with column names non-blanks and
#' possibly all in snake-cases (see specification); \code{bidsr} does not check
#' on the column names for compatibility concerns. However users should respect
#' the specification and use the recommended conventions
#' @param meta instance of \code{bids_tabular_meta_sidecar}, a class containing
#' a list of descriptors for each column (see argument \code{columns})
#' @param columns a named list, where each key correspond to a table column
#' name, and each item is a named list of descriptors, or a
#' \code{bids_tabular_column_descriptor} instance
#' @param ...,.list for \code{bids_tabular_column_descriptor}, this is
#' a list of key-value properties; for \code{as_bids_tabular}, this is passed
#' to \code{bids_tabular_meta_sidecar}
#' @param x R object that can be converted (e.g. list, table), or a path
#' to a tabular file.
#' @param table_name name of the table, used to generate a new class;
#' the class name will be \code{bids_tabular_<table_name>}
#' @param parent parent class of the new class; default is \code{bids_tabular}
#' @param content_setter a \code{setter} function to set the content; see
#' \code{\link{bids_property}}
#' @param meta_preset a \code{preset} function to set the meta; see
#' \code{bids_tabular_meta_sidecar}
#' @param prepare_save a function to prepare the content before saving; should
#' take the \code{bids_tabular} object as the first argument, and return the
#' content to be saved
#' @param lower_case_column_names if \code{TRUE}, the column names will be
#' converted to lower case; default is \code{TRUE}
#' @returns A component in \code{bids_tabular}.
#'
#' @examples
#'
#'
#'
#' # convert a data table into BIDS tabular
#' table <- data.frame(
#'   a = c(1, 2, 3, NA, NA, 6, 7, 8, 9, 10),
#'   b = sample(c('a', 'b'), size = 10, replace = TRUE)
#' )
#'
#' # basic
#' as_bids_tabular(table)
#'
#' # add descriptors
#' tabular <- as_bids_tabular(
#'   table,
#'   a = list(LongName = "An integer"),
#'   b = list("Levels" = list('a' = "Abnormal", 'b' = "Bipolar"))
#' )
#' tabular
#'
#'
#' # query data
#' is.data.frame(tabular$content)
#' tabular$content$a
#'
#' # query meta
#' tabular$meta$columns$a
#'
#' # save to tsv
#' tsv <- tempfile(fileext = ".tsv")
#' paths <- tabular$save(tsv)
#' print(paths)
#'
#' # use base R to read
#' read.table(tsv, header = TRUE, na.strings = "n/a")
#'
#' # get sidecar
#' cat(readLines(paths$sidecar_path), sep = "\n")
#'
#' unlink(tsv)
#' unlink(paths$sidecar_path)
#'
#'
#'
NULL

#' @rdname bids_tabular
#' @export
bids_tabular_column_descriptor <- new_bids_class(
  name = "bids_tabular_column_descriptor",
  hidden_names = ".more",
  properties = list(
    LongName = bids_property_collapsed_character(name = "LongName", type = "optional"),
    Description = bids_property_collapsed_character(name = "Description", type = "optional"),
    Levels = bids_property_named_list(name = "Levels"),
    Units = bids_property_character(name = "Unit", type = "optional"),
    Delimiter = bids_property_character(name = "Delimiter", type = "optional"),
    TermURL = bids_property_character(name = "TermURL", type = "optional"),
    HED = bids_property_collapsed_character(name = "HED", type = "optional"),
    .more = bids_property_named_list(name = ".more")
  ),
  methods = list(
    format = function(self, ..., indent = json_indent()) {
      to_json(as.list(self, recursive = TRUE), indent = indent)
    }
  ),
  constructor = function(..., .list = list()) {
    props <- c(list(...), .list)
    object <- S7::new_object(
      .parent = S7::S7_object(),
      LongName = character(0L),
      Description = character(0L),
      Levels = list(),
      Units = character(0L),
      Delimiter = character(0L),
      TermURL = character(0L),
      HED = character(0L),
      .more = list()
    )
    S7::valid_eventually(object = object, function(object) {
      nms <- BIDS_TABULAR_COLUME_DESCRIPTOR_FIELDS[BIDS_TABULAR_COLUME_DESCRIPTOR_FIELDS %in% names(props)]
      if(length(nms)) {
        S7::props(object) <- props[nms]
      }
      if(length(props) > length(nms)) {
        object@.more <- props[!names(props) %in% nms]
      }
      object
    })
  }
)

S7::method(names.generic, bids_tabular_column_descriptor) <- function(x) {
  nms <- c(S7::prop_names(x), names(x@.more))
  nms <- nms[!startsWith(".")]
  nms
}

S7::method(as.list.generic, bids_tabular_column_descriptor) <- function(x, all.names = FALSE, sorted = FALSE, ...) {
  nms <- S7::prop_names(x)
  nms <- nms[!startsWith(nms, ".")]

  re <- c(S7::props(x, nms), x@.more)

  re <- re[vapply(re, function(el) { length(el) > 0 }, FALSE)]

  if(length(re)) {
    nms <- names(re)
    if(!all.names) {
      nms <- nms[!startsWith(nms, ".")]
    }
    if(sorted) {
      nms <- sort(nms)
    }
    re <- re[nms]
  }
  re
}


#' @rdname bids_tabular
#' @export
bids_tabular_meta_sidecar <- new_bids_class(
  name = "bids_tabular_meta_sidecar",
  properties = list(
    columns = bids_property_tabular_column_descriptor_list(name = "columns")
  ),
  methods = list(
    format = function(self, name_list = key_missing, compact = TRUE, ..., indent = json_indent()) {
      li <- as.list(self, recursive = TRUE)$columns

      if(!identical(name_list, key_missing)) {
        li <- li[unlist(name_list)]
      }
      if(compact) {
        li <- li[vapply(li, length, 0L) > 0]
      }

      if(length(li)) {
        return(to_json(li, indent = indent))
      } else {
        return("{}")
      }
    }
  )
)




#' @rdname bids_tabular
#' @export
bids_tabular <- new_bids_class(
  name = "bids_tabular",
  # function .prepare_save is called before saving
  #   to allow for any data manipulation
  hidden_names = c(".prepare_save"),
  properties = list(
    content = bids_property_tabular_content(name = "content"),
    meta = bids_property_tabular_meta(name = "meta")
  ),
  constructor = bids_tabular_constuctor,
  methods = list(
    print = function(self, ...) {
      class_name <- attr(S7::S7_class(self), "name")
      if(length(class_name)) {
        class_name <- sprintf("[%s]", class_name[[1]])
      } else {
        class_name <- "bids_tabular"
      }
      cat(sprintf("<BIDS Tabular>%s\n$meta:\n", class_name))
      print(self@meta)
      cat("\n$content:\n")
      print(self@content)
    },
    save = function(self, path, meta = TRUE, compact_meta = TRUE, ...) {
      if(!grepl("\\.(tsv|tsv\\.gz)", tolower(path))) {
        path <- paste0(path, ".tsv")
      }

      if(is.function(self$.prepare_save)) {
        content <- self$.prepare_save(...)
      } else {
        content <- self$content
      }

      write_tsv(x = content, file = path)
      path <- path_abs(path)
      sidecar_path <- NA
      if(meta) {
        sidecar_path <- gsub("\\.[ct](sv|sv\\.gz)", ".json", x = path, ignore.case = TRUE)

        writeLines(text = self$meta$format(compact = TRUE, name_list = names(content)),
                   con = sidecar_path)
        sidecar_path <- path_abs(sidecar_path)
      }
      invisible(list(
        table_path = path,
        sidecar_path = sidecar_path
      ))
    }
  )
)

#' @rdname bids_tabular
#' @export
as_bids_tabular <- S7::new_generic("as_bids_tabular", "x")

S7::method(as_bids_tabular, bids_tabular) <- function(x, ..., cls = NULL) {
  if(
    all(inherits(cls, c("bids_tabular_definition", "S7_class"), which = TRUE) > 0) &&
    !identical(cls, S7::S7_class(x))
  ) {
    x <- cls(content = x$content, meta = x$meta)
  }
  x
}


S7::method(
  as_bids_tabular,
  S7::new_union(
    S7::class_data.frame,
    S7::class_list,
    S7::new_S3_class("fst_table")
  )
) <- function(x, meta = NULL, ..., cls = NULL) {
  as_bids_tabular_table(x = x, meta = meta, ..., cls = cls)
}


S7::method(as_bids_tabular, S7::class_character) <- function(x, meta = NULL, ..., cls = NULL) {
  # csv, tsv
  x_ <- gsub(".gz$", "", tolower(x))
  if(endsWith(x_, "csv")) {
    reader <- read_csv
  } else {
    reader <- read_tsv
  }
  tbl <- reader(x_)
  as_bids_tabular_table(x = tbl, meta = meta, ..., cls = cls)
}




# generator


#' @rdname bids_tabular
new_bids_tabular_class <- function(
    table_name, parent = bids_tabular,
    content_setter = NULL, meta_preset = NULL, prepare_save = NULL,
    lower_case_column_names = FALSE) {

  class_name <- sprintf("bids_tabular_%s", table_name)

  if(is.function(prepare_save)) {
    methods <- list(
      .prepare_save = prepare_save
    )
  } else {
    methods <- NULL
  }

  new_bids_class(
    name = class_name,
    parent = parent,
    properties = list(
      content = bids_property_tabular_content(
        name = "content",
        name_meta = "meta",
        setter = content_setter,
        lower_case_column_names = lower_case_column_names
      ),
      meta = bids_property_tabular_meta(
        name = "meta",
        name_content = "content",
        preset = meta_preset
      )
    ),
    methods = methods,
    constructor = bids_tabular_constuctor
  )
}




