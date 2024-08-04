bids_scans_impl <- S7::new_class(
  name = "bids_scans",
  parent = bids_tabular_data,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    data = S7::new_property(
      S7::class_data.frame,
      validator = function(value) {
        nms <- names(value)
        if(!length(nms) || nms[[1]] != "filename") {
          return("BIDS scans table must contain column `filename` and it must be the first column in the table")
        }
        if(!nrow(value)) { return() }
        if( anyNA(value$filename) ) {
          return("BIDS scans `filename` must NOT be NA")
        }
        dup <- value$filename[duplicated(value$filename)]
        dup <- dup[!is.na(dup)]
        if(length(dup)) {
          return("BIDS scans `filename` must be unique. Remove the following duplicated filenames: ", dup[[1]], ", ...")
        }
        return()
      }
    )
  )
)



#' @export
bids_scans <- function(x, header = NULL) {
  # DIPSAUS DEBUG START
  # x <- data.frame(
  #   filename = "sub-01",
  #   age = 66,
  #   species = "homo sapiens"
  # )
  new_bids_builtin_tabular(
    x = x,
    header = header,
    as_class = bids_scans_impl,
    default_descriptors = list()
  )
}



