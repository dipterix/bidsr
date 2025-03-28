
# ---- Sidecars -------------------------------------------------------
#' @rdname bids_tabular
#' @export
as_bids_tabular <- S7::new_generic("as_bids_tabular", "x")

#' @rdname bids_tabular
#' @export
save_bids_tabular <- S7::new_generic("save_bids_tabular", "x", function(x, path, meta = TRUE, ...) {
  S7::S7_dispatch()
})

#' @rdname bids_dataset_description
#' @export
get_bids_dataset_description <- S7::new_generic(
  name = "get_bids_dataset_description",
  dispatch_args = "x",
  fun = function(x, parent_directory, ...) {
    S7::S7_dispatch()
  }
)

#' @rdname bids_tabular_participants
#' @export
get_bids_participants <- S7::new_generic(
  name = "get_bids_participants",
  dispatch_args = "x",
  fun = function(x, ...) {
    S7::S7_dispatch()
  }
)

#' @rdname bids_tabular_samples
#' @export
get_bids_samples <- S7::new_generic(
  name = "get_bids_samples",
  dispatch_args = "x",
  fun = function(x, ...) {
    S7::S7_dispatch()
  }
)

#' @rdname bids_tabular_phenotype
#' @export
get_bids_phenotype_data <- S7::new_generic(
  name = "get_bids_phenotype_data",
  dispatch_args = "x",
  fun = function(x, ...) {
    S7::S7_dispatch()
  }
)



















