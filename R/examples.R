#' @title Download 'BIDS' example data-sets
#' @description
#' See \url{https://github.com/bids-standard/bids-examples} for the full
#' repository.
#' @returns A local path to the example repository
#' @examples
#'
#' if(interactive()) {
#'
#'   download_bids_examples()
#'
#' }
#'
#' @export
download_bids_examples <- function() {
  url = "https://github.com/bids-standard/bids-examples/archive/refs/heads/master.zip"
  cache_root <- tools::R_user_dir(package = "bidsr", which = "cache")
  example_root <- file_path(cache_root, "bids-examples")

  # check if ...../bids-examples-master/docs exists
  docs_path <- file_path(example_root, c("bids-examples-master/docs", "docs"))
  sel <- file_exists(docs_path)

  if(any(sel)) {
    example_path <- dirname(docs_path[sel][[1]])
  } else {
    dir_create(example_root)

    f <- tempfile(fileext = ".zip")
    old_opt <- options(timeout = 3600)
    on.exit({
      options(old_opt)
      unlink(f)
    })
    utils::download.file(url, destfile = f)
    utils::unzip(f, exdir = example_root)
    sel <- file_exists(docs_path)
    example_path <- dirname(docs_path[sel][[1]])
  }
  example_path
}
