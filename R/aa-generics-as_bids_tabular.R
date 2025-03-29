
# ---- Sidecars -------------------------------------------------------
#' @rdname BIDSTabular
#' @export
as_bids_tabular <- S7::new_generic("as_bids_tabular", "x")

#' @rdname BIDSTabular
#' @export
save_bids_tabular <- S7::new_generic("save_bids_tabular", "x", function(x, path, meta = TRUE, ...) {
  S7::S7_dispatch()
})

#' @rdname BIDSDatasetDescription
#' @export
get_bids_dataset_description <- S7::new_generic(
  name = "get_bids_dataset_description",
  dispatch_args = "x",
  fun = function(x, parent_directory, ...) {
    S7::S7_dispatch()
  }
)

#' @rdname BIDSTabularParticipants
#' @export
get_bids_participants <- S7::new_generic(
  name = "get_bids_participants",
  dispatch_args = "x",
  fun = function(x, ...) {
    S7::S7_dispatch()
  }
)

#' @rdname BIDSTabularSamples
#' @export
get_bids_samples <- S7::new_generic(
  name = "get_bids_samples",
  dispatch_args = "x",
  fun = function(x, ...) {
    S7::S7_dispatch()
  }
)

#' @rdname BIDSTabularPhenotype
#' @export
get_bids_phenotype_data <- S7::new_generic(
  name = "get_bids_phenotype_data",
  dispatch_args = "x",
  fun = function(x, ...) {
    S7::S7_dispatch()
  }
)



















