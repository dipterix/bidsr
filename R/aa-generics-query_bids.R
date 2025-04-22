
# ---- query_bids -------------------------------------------------------
#' @title Query 'BIDS'
#' @author Zhengjia Wang
#' @description
#' Query 'BIDS' project and analyze the files
#' @param x 'BIDS' objects such as subject
#' @param search_params searching parameters, leave it blank to see help
#' documentations
#' @param env where to resolve selectors
#' @param ... passed to down-stream methods
#' @returns A data table of query results
#'
#' @export
query_bids <- S7::new_generic("query_bids", c("x"), function(x, search_params, env = parent.frame(), ...) {
  S7::S7_dispatch()
})
