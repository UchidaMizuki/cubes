
new_cubes <- function(data, dims) {
  dm <- purrr::map_int(dims, length)
  for (i in seq_along(data)) {
    data[[i]] <- array(data[[i]],
                       dim = dm)
  }
  structure(data,
            class = "cubes",
            dims = dims)
}

#' @export
cubes <- function(..., .dims = NULL) {

}

#' @export
as_cubes <- function(x, ...) {
  UseMethod("as_cubes")
}

#' @export
as_cubes.cube <- function(x, dims, values = "value", ...) {
  ellipsis::check_dots_empty()

  dims <- as_list_dims(dims)
}

#' @export
as_cubes.array <- function(x, dims, values = "value", ...) {
  ellipsis::check_dots_empty()

  dims <- as_list_dims(dims)
}

#' @export
as_cubes.data.frame <- function(x, dims, values = NULL, ...) {
  ellipsis::check_dots_empty()

  dims <- as_list_dims(dims)

  nms_x <- names(x)
  nms_dims <- names(dims)
  stopifnot(nms_dims %in% nms_x,
            !vctrs::vec_duplicate_any(x[nms_dims]))

  values <- values %||% setdiff(nms_x, nms_dims)

  for (i in seq_along(dims)) {
    nm <- nms_dims[[i]]
    x_nm <- x[[nm]]
    dims_i <- dims[[i]] %||% unique(x_nm)
    dims[[i]] <- dims_i
    x[[nm]] <- vctrs::vec_match(x_nm, dims_i)
  }

  ids <- purrr::modify(dims, seq_along)
  ids <- rlang::exec(expand.grid, !!!ids,
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  x <- dplyr::left_join(ids, x,
                        by = nms_dims)
  new_cubes(as.list(x[values]),
            dims)
}

#' @export
dimnames.cubes <- function(x) {
  attr(x, "dims")
}

#' @export
as_tibble.cubes <- function(x, ...,
                            .rows = NULL,
                            .name_repair = c("check_unique", "unique", "universal", "minimal"),
                            rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
  dims <- tibble::as_tibble(rlang::exec(expand.grid, !!!dimnames(x),
                                        KEEP.OUT.ATTRS = FALSE,
                                        stringsAsFactors = FALSE))
  values <- purrr::map_dfc(x, as.vector)

  tibble::tibble(dims = dims,
                 values = values)
}



# Subsetting --------------------------------------------------------------

#' @export
`[.cubes` <- function(x, i) {
  out <- NextMethod()
  attrs <- attributes(x)
  attributes(out) <- c(attributes(out),
                       attrs[names(attrs) != "names"])
  out
}

#' @export
`[[.cubes` <- function(x, i) {
  out <- NextMethod()
}


# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice

#' @export
slice.cubes <- function(.data, ...) {
  dots <- rlang::list2(...)
  nms_dots <- rlang::names2(dots)

  dims <- dimnames(.data)
  nms_dims <- names(dims)
  stopifnot(length(dots) == length(dims),
            nms_dots == "" | nms_dots %in% nms_dims)

  names(dots)[nms_dots == ""] <- nms_dims[!nms_dims %in% nms_dots]
  dots <- dots[nms_dims]

  data <- purrr::modify(.data, function(x) rlang::exec(`[`, x, !!!dots))
  dims <- purrr::map2(dims, dots, `[`)
  new_cubes(data, dims)
}

# Printing ----------------------------------------------------------------

#' @export
print.cubes <- function(x, n = NULL, ...) {
  print_cubes(x, n)
}

print_cubes <- function(x, n) {
  out <- head_cube(x, n)
  out <- vctrs::new_data_frame(as_tibble.house(out),
                               class = c("tbl_cube", "tbl"))
  # attr(out, "tbl_sum") <- paste(pillar::size_sum(x[[1]]), length(x),
  #                               sep = " x ")
  attr(out, "rows_total") <- length(x[[1]])
  print(out)
  invisible(x)
}
