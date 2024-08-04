
# Correct path if prefix has incorrect cases (works on case-sensitive os)
correct_filepath <- function(path) {
  stopifnot(length(path) == 1)
  if(file_exists(path)) { return(path) }
  # path_prefix = "bidsr.rproj"
  dir <- dirname(path)
  prefix <- basename(path)
  files <- list.files(
    path = dir,
    full.names = FALSE,
    all.files = TRUE,
    recursive = FALSE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  sel <- toupper(files) == toupper(prefix)
  if(!any(sel)) { return(NA) }

  return(files[sel][[1]])
}

path_to_nearest_file <- function(filename, start, root = NA, ignore_cases = FALSE) {

  # filename <- "rave"
  # start = "."

  if( ignore_cases ) {
    filename <- tolower(filename)
    start <- tolower(start)
  }

  if(file_exists(start)) {
    if(fs::is_file(start)) {
      if( basename(start) == filename ) {
        # start is a file and is the filename
        return(start)
      }
      start <- fs::path_dir(start)
    }
    # now start is a folder
    tmp <- fs::path(start, filename)
    if( file_exists(tmp) ) {
      return(tmp)
    }
    # folder does not have this file
  }
  if(!fs::is_absolute_path(start)) {
    start <- fs::path_abs(start)
  }
  start_ <- fs::path_dir(start)

  if(!is.na(root)) {
    if(!fs::is_absolute_path(root)) {
      root <- fs::path_abs(root)
    }
    if(!fs::path_has_parent(start_, root)) {
      return(NA_character_)
    }
  } else {
    if(start_ == start) {
      return(NA_character_)
    }
  }
  Recall(filename = filename, start = start_, root = root)
}


# Do not use fs::dir_create as the `fs` file ops do not respect system umask
# and is default to 0755
dir_create <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    stop('Cannot create directory at ', shQuote(x))
  }
  invisible(normalizePath(x))
}

path_abs <- function(path) {
  fs::path_abs(path)
}

path_expand <- function(path) {
  fs::path_norm(fs::path_expand(path))
}

path_norm <- function(path) {
  fs::path_abs(path)
}

path_rel <- function(path, start = ".") {
  fs::path_rel(path, start = start)
}

file_path <- function(..., ext = "") {
  fs::path(..., ext = ext)
}

file_exists <- function(path) {
  fs::file_exists(path)
}

dir_exists <- function(path) {
  fs::dir_exists(path)
}
