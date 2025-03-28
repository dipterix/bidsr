
# ---- query_bids -------------------------------------------------------
#' @title Query 'BIDS'
#' @description
#' Query 'BIDS' project and analyze the files
#' @param x 'BIDS' objects such as subject
#' @param search_params searching parameters, leave it blank to see help
#' documentations
#' @param ... passed to down-stream methods
#' @returns A data table of query results
#'
#' @export
query_bids <- S7::new_generic("query_bids", c("x"), function(x, search_params, ...) {
  S7::S7_dispatch()
})
