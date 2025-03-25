#' @name bids_project
#' @export
bids_project <- new_bids_class(
  name = "bids_project",
  properties = list(
    name = bids_property_character(
      name = "name",
      getter = function(self) {
        return(basename(self@path))
      }
    ),
    path = bids_property_character(
      name = "path",
      type = "required",
      default = ".",
      validator = function(value) {
        if (length(value) != 1) {
          return("BIDS project path must be a length of 1")
        }
        if (is.na(value) || !is.character(value)) {
          return("BIDS project path must be string and cannot be NA")
        }
        if (trimws(value) %in% c("", "/")) {
          return("BIDS project path must be a valid path and cannot be blank nor `/`")
        }
        if(is_file(value)) {
          return("Path exists but appear to be a file. BIDS project path must be a directory.")
        }
        return(NULL)
      },
      setter = function(self, value) {
        value_ <- tolower(value)
        using_protocol <- grepl("^[a-z]+://", value_) || startsWith(value_, "doi:")
        if(!using_protocol) {
          value <- path_abs(value)
        }
        self@path <- value
        self
      }
    )
  )
)
