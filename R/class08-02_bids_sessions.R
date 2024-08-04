bids_sessions_impl <- S7::new_class(
  name = "bids_sessions",
  parent = bids_tabular_data,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    data = S7::new_property(
      S7::class_data.frame,
      validator = function(value) {
        nms <- names(value)
        if(!length(nms) || nms[[1]] != "session_id") {
          return("BIDS sessions table must contain column `session_id` and it must be the first column in the table")
        }
        if(!nrow(value)) { return() }
        if( anyNA(value$session_id) ) {
          return("BIDS sessions `session_id` must NOT be NA")
        }
        dup <- value$session_id[duplicated(value$session_id)]
        dup <- dup[!is.na(dup)]
        if(length(dup)) {
          return("BIDS sessions `session_id` must be unique. Remove the following duplicated sessions: ", dup[[1]], ", ...")
        }
        if(!all(startsWith(value$session_id, "ses-"))) {
          return("BIDS sessions `session_id` must have format `ses-<label>`")
        }
        return()
      }
    )
  )
)



#' @export
bids_sessions <- function(x, header = NULL) {
  # DIPSAUS DEBUG START
  # x <- data.frame(
  #   session_id = "ses-01"
  # )
  new_bids_builtin_tabular(
    x = x,
    header = header,
    as_class = bids_sessions_impl,
    default_descriptors = list()
  )
}



