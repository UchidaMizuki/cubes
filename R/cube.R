new_cube <- function(data, dims) {
  data <- array(data,
                dim = size_dims(dims))
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

#' @export
is_cube <- function(x) {
  inherits(x, "cube")
}

#' @export
as.array.cube <- function(x, ...) {
  out <- unclass(x)
  attr(out, "dims") <- NULL
  out
}

#' @export
dimnames.cube <- function(x) {
  attr(x, "dims")
}

#' @export
`dimnames<-.cube` <- function(x) {
  attr(x, "dims") <- x
}

#' @export
as_tibble.cube <- function(x, ...,
                           .rows = NULL,
                           .name_repair = c("check_unique", "unique", "universal", "minimal"),
                           rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
  as_tibble_cubes(x,
                  .rows = .rows,
                  .name_repair = .name_repair,
                  rownames = rownames)
}

#' @export
aperm.cube <- function(a, perm, ...) {
  aperm_cubes(a, perm)
}



# Verbs -------------------------------------------------------------------

#' @export
slice.cube <- function(.data, ...) {
  slice_cubes(.data, ...)
}

#' @export
select.cube <- function(.data, ...) {
  select_cubes(.data, ...)
}

#' @export
relocate.cube <- function(.data, ...) {
  relocate_cubes(.data, ...)
}


# Printing ----------------------------------------------------------------

#' @export
print.cube <- function(x, n = NULL, ...) {
  print_cubes(x, n)
}

#' @importFrom pillar obj_sum
#' @export
pillar::obj_sum
#' @export
obj_sum.cube <- function(x) {
  paste0("[cube: ", dim_sum(x), "]")
}
