new_cubes <- function(data, dims) {
  dm <- unname(size_dims(dims))
  prod_dm <- prod(dm)

  for (i in seq_along(data)) {
    data_i <- data[[i]]
    stopifnot(length(data_i) == prod_dm)

    data[[i]] <- array(data_i, dm)
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
  ids <- rlang::exec(expand.grid, !!!ids,
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  x <- dplyr::left_join(ids, x,
                        by = axes)
  new_cubes(as.list(x[values]), dims)
}

#' @export
is_cubes <- function(x) {
  inherits(x, "cubes") && is.list(x)
}

#' @export
dim.cubes <- function(x) {
  size_dims(dimnames(x))
}

#' @export
`dim<-.cubes` <- function(x, value) {
  stop()
}

#' @export
dimnames.cubes <- function(x) {
  attr(x, "dims")
}

#' @export
`dimnames<-.cubes` <- function(x, value) {
  attr(x, "dims") <- value
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.cubes <- function(x, ...) {
  dims <- tibble::as_tibble(rlang::exec(expand.grid, !!!dimnames(x),
                                        KEEP.OUT.ATTRS = FALSE,
                                        stringsAsFactors = FALSE),
                            ...)
  if (is_cubes(x)) {
    tibble::tibble(dims = dims,
                   cube = as_tibble(lapply(x, as.vector)))
  } else if (is_cube(x)) {
    value <- as.vector(x)
    tibble::tibble(dims = dims,
                   value = as.vector(x))
  }
}

#' @export
aperm.cubes <- function(a, perm, ...) {
  dims <- dimnames(a)

  perm <- vctrs::vec_match(perm, names(dims))
  aperm_cube <- function(x) {
    aperm(as.array.cube(x), perm)
  }
  dims <- dims[perm]

  if (is_cubes(a)) {
    new_cubes(lapply(a, aperm_cube), dims)
  } else if (is_cube(a)) {
    new_cube(aperm_cube(a), dims)
  }
}



# Subsetting --------------------------------------------------------------

#' @export
`[.cubes` <- function(x, ...) {
  out <- NextMethod()
  if (is_cubes(x)) {
    attrs <- attributes(x)
    attributes(out) <- c(attributes(out),
                         attrs[names(attrs) != "names"])
  }
  out
}

#' @export
`[[.cubes` <- function(x, i) {
  out <- NextMethod()
  if (is_cubes(x)) {
    out <- new_cube(out, dimnames(x))
  }
  out
}

#' @export
`$.cubes` <- function(x, i) {
  out <- NextMethod()
  if (is_cubes(x)) {
    out <- new_cube(out, dimnames(x))
  }
  out
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.cubes <- function(.data, ...) {
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

  if (is_cubes(.data)) {
    data <- lapply(.data, function(x) rlang::exec(`[`, x, !!!dots))
    new_cubes(data, dims)
  } else if (is_cube(.data)) {
    data <- rlang::exec(`[`, .data, !!!dots)
    new_cube(data, dims)
  }
}



#' @importFrom dplyr select
#' @export
select.cubes <- function(.data, ...) {
  dims <- dimnames(.data)
  data <- c(dims, .data)
  vars <- names(tidyselect::eval_select(rlang::expr(c(...)), data))

  axes <- names(dims)
  vars_rem <- axes[!axes %in% vars]
  vars <- c(vars, vars_rem)

  .data <- aperm(.data,
                 perm = vars[vars %in% axes])
  if (is_cubes(.data)) {
    .data[vars[!vars %in% axes]]
  } else if (is_cube(.data)) {
    .data
  }
}

#' @importFrom dplyr relocate
#' @export
relocate.cubes <- function(.data, ...) {
  dims <- dimnames(.data)
  data <- c(dims, .data)
  vars <- names(tidyselect::eval_select(rlang::expr(c(...)), data))

  vars_rem <- names(data)
  vars_rem <- vars_rem[!vars_rem %in% vars]
  vars <- c(vars, vars_rem)

  axes <- names(dims)
  .data <- aperm(.data, vars[vars %in% axes])
  if (is_cubes(.data)) {
    .data[vars[!vars %in% axes]]
  } else if (is_cube(.data)) {
    .data
  }
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.cubes <- function(x) {
  names(dimnames(x))
}

#' @importFrom dplyr group_vars
#' @export
group_vars.cubes <- function(x) {
  attr(x, "groups")
  # names(dimnames(x)[attr(x, "groups")])
}

#' @importFrom dplyr group_by
#' @export
group_by.cubes <- function(.data, ...) {
  groups <- dplyr::group_by_prepare(.data, ...)
  structure(groups$data,
            groups = groups$group_names)
}

# Printing ----------------------------------------------------------------

#' @export
print.cubes <- function(x, n = NULL, ...) {
  out <- head_cubes(x, n)
  out <- vctrs::new_data_frame(as_tibble(out),
                               class = c("tbl_cubes", "tbl"))
  if (is_cubes(x)) {
    tbl_sum <- paste(obj_sum.cubes(x[[1]]), length(x), sep = " x ")
    attr(out, "tbl_sum") <- c(Cubes = tbl_sum)
    attr(out, "rows_total") <- length(x[[1]])
  } else if (is_cube(x)) {
    attr(out, "tbl_sum") <- c(`A cube` = dim_sum(dim(x)))
    attr(out, "rows_total") <- length(x)
  }

  # Groups
  groups <- attr(x, "groups")
  if (!is.null(groups)) {
    attr(out, "tbl_sum") <- c(attr(out, "tbl_sum"),
                              Groups = paste0(commas(groups), " [", dim_sum(dim(x)[groups]), "]"))
  }

  print(out)
  invisible(x)
}

#' @importFrom pillar tbl_format_setup
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
