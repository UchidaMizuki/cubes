new_cubes <- function(data, dims,
                      groups = NULL) {
  dm <- unname(lengths(dims))
  prod_dm <- prod(dm)

  for (i in seq_along(data)) {
    data_i <- data[[i]]
    stopifnot(length(data_i) == prod_dm)

    data[[i]] <- array(data_i, dm)
  }
  structure(data,
            class = c("cubes", "list"),
            dims = dims,
            groups = groups)
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
  ids <- expand.grid(ids,
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  x <- dplyr::left_join(ids, x,
                        by = axes)
  new_cubes(as.list(x[values]),
            dims = dims)
}

#' @export
is_cubes <- function(x) {
  inherits(x, "cubes") && is.list(x)
}

#' @export
as.list.cubes <- function(x, ...) {
  structure(x,
            class = NULL,
            dims = NULL)
}

#' @export
dim.cubes <- function(x) {
  lengths(dimnames(x))
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
  dims <- expand.grid(dimnames(x),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  dims <- tibble::as_tibble(dims, ...)

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
  groups <- perm[perm %in% group_vars.cubes(a)]

  if (is_cubes(a)) {
    new_cubes(lapply(a, aperm_cube),
              dims = dims,
              groups = groups)
  } else if (is_cube(a)) {
    new_cube(aperm_cube(a),
             dims = dims,
             groups = groups)
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
    out <- new_cube(out,
                    dims = dimnames(x),
                    groups = group_vars.cubes(x))
  }
  out
}

#' @export
`$.cubes` <- function(x, i) {
  out <- NextMethod()
  if (is_cubes(x)) {
    out <- new_cube(out,
                    dims = dimnames(x),
                    groups = group_vars.cubes(x))
  }
  out
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.cubes <- function(.data, ...) {
  dots <- rlang::list2(...)
  dots <- lapply(dots, function(x) x %||% rlang::missing_arg())
  nms_dots <- rlang::names2(dots)

  dims <- dimnames(.data)
  axes <- names(dims)
  stopifnot(length(dots) == length(dims),
            nms_dots == "" | nms_dots %in% axes)

  names(dots)[nms_dots == ""] <- axes[!axes %in% nms_dots]
  dots <- dots[axes]
  dims <- mapply(`[`, dims, dots,
                 SIMPLIFY = FALSE)

  groups <- group_vars.cubes(.data)

  if (is_cubes(.data)) {
    new_cubes(lapply(.data, function(x) rlang::exec(`[`, x, !!!dots)),
              dims = dims,
              groups = groups)
  } else if (is_cube(.data)) {
    new_cube(rlang::exec(`[`, .data, !!!dots),
             dims = dims,
             groups = groups)
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

#' @importFrom dplyr mutate
#' @export
mutate.cubes <- function(.data, ...) {
  dots <- rlang::enquos(..., .named = TRUE)
  nms_dots <- names(dots)

  nms_data <- names(.data)
  dims <- dimnames(.data)

  data <- lapply(seq_along(.data), function(x) .data[[x]])
  names(data) <- nms_data
  data <- rlang::as_data_mask(data)

  size <- length(dots)
  out <- vector("list", size)
  for (i in seq_len(size)) {
    out[[i]] <- cube(rlang::eval_tidy(dots[[i]], data), dims)
  }
  names(out) <- names(dots)

  out <- c(.data[!nms_data %in% nms_dots], out)
  new_cubes(out, dims)
}

#' @importFrom dplyr summarise
#' @export
summarise.cubes <- function(.data, ...) {
  dots <- rlang::enquos(..., .named = TRUE)
  nms_dots <- names(dots)
  size <- length(dots)

  nms_data <- names(.data)
  dims <- dimnames(.data)

  groups <- group_vars.cubes(.data)
  ids_rem <- lapply(dims[!names(dims) %in% groups], seq_along)

  ids <- expand.grid(lapply(dims[groups], seq_along),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  size <- nrow(ids)
  out <- double(size)
  for (i in seq_len(size)) {
    data <- rlang::exec(slice, .data, !!!c(ids[i, ], ids_rem))
    data <- lapply(seq_along(data), function(x) data[[x]])
    names(data) <- nms_data
    data <- rlang::as_data_mask(data)

    # cube(rlang::eval_tidy(dots[[1]], data), dims) %>%
    #   print()

    # out <- vector("list", size)
    # for (i in seq_len(size)) {
    #   out[[i]] <- cube(rlang::eval_tidy(dots[[i]], data), dims)
    # }
    # names(out) <- names(dots)
  }


  #
  # out <- c(.data[!nms_data %in% nms_dots],
  #          out)
  # new_cubes(out, dims)
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
