new_cubes <- function(data, dims) {
  for (i in seq_along(data)) {
    data[[i]] <- array(data[[i]],
                       dim = size_dims(dims))
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
  axes <- names(dims)
  stopifnot(axes %in% nms_x,
            !vctrs::vec_duplicate_any(x[axes]))

  values <- values %||% setdiff(nms_x, axes)

  for (i in seq_along(dims)) {
    axis <- axes[[i]]
    x_axis <- x[[axis]]
    dims_i <- dims[[i]] %||% unique(x_axis)
    dims[[i]] <- dims_i
    x[[axis]] <- vctrs::vec_match(x_axis, dims_i)
  }

  ids <- lapply(dims, seq_along)
  # ids <- purrr::modify(dims, seq_along)
  ids <- rlang::exec(expand.grid, !!!ids,
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  x <- dplyr::left_join(ids, x,
                        by = axes)
  new_cubes(as.list(x[values]),
            dims = dims)
}

#' @export
is_cubes <- function(x) {
  inherits(x, "cubes")
}

#' @export
dimnames.cubes <- function(x) {
  attr(x, "dims")
}

#' @export
`dimnames<-.cubes` <- function(x) {
  attr(x, "dims") <- x
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble
#' @export
as_tibble.cubes <- function(x, ...,
                            .rows = NULL,
                            .name_repair = c("check_unique", "unique", "universal", "minimal"),
                            rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
  as_tibble_cubes(x,
                  .rows = .rows,
                  .name_repair = .name_repair,
                  rownames = rownames)
}

#' @export
as_tibble_cubes <-  function(x, .rows, .name_repair, rownames) {
  dims <- tibble::as_tibble(rlang::exec(expand.grid, !!!dimnames(x),
                                        KEEP.OUT.ATTRS = FALSE,
                                        stringsAsFactors = FALSE),
                            .rows = .rows,
                            .name_repair = .name_repair,
                            rownames = rownames)
  if (is_cubes(x)) {
    tibble::tibble(dims = dims,
                   values = as_tibble(lapply(x, as.vector)))
    # purrr::map_dfc(x, as.vector))
  } else if (is_cube(x)) {
    tibble::tibble(dims = dims,
                   value = as.vector(x))
  }
}

#' @export
aperm.cubes <- function(a, perm, ...) {
  aperm_cubes(a, perm)
}

#' @export
aperm_cubes <- function(a, perm) {
  dims <- dimnames(a)

  perm <- vctrs::vec_match(perm, names(dims))
  aperm_cube <- function(x) {
    aperm(as.array.cube(x), perm = perm)
  }
  dims <- dims[perm]

  if (is_cubes(a)) {
    new_cubes(lapply(a, aperm_cube),
              dims = dims)
  } else if (is_cube(a)) {
    new_cube(aperm_cube(a),
             dims = dims)
  }
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
  new_cube(NextMethod(),
           dims = dimnames(x))
}

#' @export
`$.cubes` <- function(x, i) {
  new_cube(NextMethod(),
           dims = dimnames(x))
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
dplyr::slice
#' @export
slice.cubes <- function(.data, ...) {
  slice_cubes(.data, ...)
}

slice_cubes <- function(.data, ...) {
  dots <- rlang::list2(...)
  nms_dots <- rlang::names2(dots)

  dims <- dimnames(.data)
  axes <- names(dims)
  stopifnot(length(dots) == length(dims),
            nms_dots == "" | nms_dots %in% axes)

  names(dots)[nms_dots == ""] <- axes[!axes %in% nms_dots]
  dots <- dots[axes]
  dims <- mapply(`[`, dims, dots,
                 SIMPLIFY = FALSE)
  # dims <- purrr::map2(dims, dots, `[`)

  if (is_cubes(.data)) {
    data <- lapply(.data, function(x) rlang::exec(`[`, x, !!!dots))
    new_cubes(data,
              dims = dims)
  } else if (is_cube(.data)) {
    data <- rlang::exec(`[`, .data, !!!dots)
    new_cube(data, dims)
  }
}

#' @importFrom dplyr select
#' @export
dplyr::select
#' @export
select.cubes <- function(.data, ...) {
  select_cubes(.data, ...)
}

#' @export
select_cubes <- function(.data, ...) {
  dims <- dimnames(.data)
  data <- c(dims, .data)
  vars <- names(tidyselect::eval_select(rlang::expr(c(...)), data))

  axes <- names(dims)
  vars_rem <- axes[!axes %in% vars]
  vars <- c(vars, vars_rem)

  .data <- aperm(.data,
                 perm = vars[vars %in% axes])
  .data[vars[!vars %in% axes]]
}

#' @importFrom dplyr relocate
#' @export
dplyr::relocate
#' @export
relocate.cubes <- function(.data, ...) {
  relocate_cubes(.data, ...)
}

#' @export
relocate_cubes <- function(.data, ...) {
  dims <- dimnames(.data)
  data <- c(dims, .data)
  vars <- names(tidyselect::eval_select(rlang::expr(c(...)), data))

  vars_rem <- names(data)
  vars_rem <- vars_rem[!vars_rem %in% vars]
  vars <- c(vars, vars_rem)

  axes <- names(dims)
  .data <- aperm(.data,
                 perm = vars[vars %in% axes])
  .data[vars[!vars %in% axes]]
}



# Printing ----------------------------------------------------------------

#' @export
print.cubes <- function(x, n = NULL, ...) {
  print_cubes(x, n)
}

print_cubes <- function(x, n) {
  out <- head_cubes(x, n)
  out <- vctrs::new_data_frame(as_tibble(out),
                               class = c("tbl_cubes", "tbl"))
  if (is_cubes(x)) {
    attr(out, "tbl_sum") <- c(Cubes = paste(obj_sum.cube(x[[1]]), length(x),
                                            sep = " x "))
    attr(out, "rows_total") <- length(x[[1]])
  } else if (is_cube(x)) {
    attr(out, "tbl_sum") <- c(`A cube` = dim_sum(x))
    attr(out, "rows_total") <- length(x)
  }
  print(out)
  invisible(x)
}

#' @importFrom pillar tbl_format_setup
#' @export
pillar::tbl_format_setup
#' @export
tbl_format_setup.tbl_cubes <- function(x, width, ..., n, max_extra_cols, max_footer_lines) {
  setup <- NextMethod()

  setup$tbl_sum <-  attr(x, "tbl_sum")

  rows_total_old <- setup$rows_total
  rows_total_new <- attr(x, "rows_total")
  setup$rows_total <- rows_total_new
  setup$rows_missing <- rows_total_new - (rows_total_old - setup$rows_missing)
  setup
}
