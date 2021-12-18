new_cube <- function(data, dims) {
  dm <- purrr::map_int(dims, length)
  for (i in seq_along(data)) {
    data[[i]] <- array(data[[i]],
                       dim = dm)
  }
  structure(data,
            class = c("cube", "array"),
            dims = dims)
}

cube <- function(...) {

}

#' @export
as_cube <- function(x, ...) {
  UseMethod("as_cube")
}

#' @export
as_cube.array <- function(x, dims, ...) {
  ellipsis::check_dots_empty()


}



# Printing ----------------------------------------------------------------

#' @export
print.cube <- function(x, n = NULL, ...) {
  print_cubes(x, n)
}

#' @importFrom pillar obj_sum
#' @export
obj_sum

#' @export
obj_sum.cube <- function(x) {
  dm <- paste(dim(x),
              collapse = " x ")
  paste("[cube:", dm, "]")
}

#' @importFrom pillar tbl_format_setup
#' @export
tbl_format_setup

#' @export
tbl_format_setup.tbl_cube <- function(x, width, ..., n, max_extra_cols, max_footer_lines) {
  setup <- NextMethod()

  # setup$tbl_sum <- c(`A house` = attr(x, "tbl_sum"))

  rows_total_old <- setup$rows_total
  rows_total_new <- attr(x, "rows_total")
  setup$rows_total <- rows_total_new
  setup$rows_missing <- rows_total_new - (rows_total_old - setup$rows_missing)
  setup
}
