new_cube <- function(data, dims,
                     groups = NULL) {
  dm <- unname(lengths(dims))
  stopifnot(length(data) == prod(dm))

  data <- array(data,
                dim = dm)
  structure(data,
            class = c("cubes", "array"),
            dims = dims,
            groups = groups)
}

cube <- function(data, dims) {
  withCallingHandlers({
    if (rlang::is_scalar_atomic(data)) {
      new_cube(rep(data, prod(lengths(dims))),
               dims = dims)
    } else {
      stopifnot(is_cube(data))

      dims_data <- dimnames(data)
      dims <- as_list_dims(dims)
      axes <- names(dims)

      stopifnot(names(dims_data) %in% axes)
      dims <- lapply(axes, function(x) dims[[x]] %||% dims_data[[x]])
      names(dims) <- axes

      stopifnot(!vapply(dims, is.null, FUN.VALUE = logical(1)))

      broadcast(data, dims)
    }
  },
  warning_broadcast = function(w) invokeRestart("restart_broadcast"))
}

#' @export
as_cube <- function(x, ...) {
  UseMethod("as_cube")
}

#' @export
as_cube.array <- function(x, dims, ...) {
  ellipsis::check_dots_empty()

  # TODO: named array
  out <- new_cube(x,
                  dims = dims[seq_along(dim(x))])
  broadcast(out, dims)
}

#' @export
is_cube <- function(x) {
  inherits(x, "cubes") && is.array(x)
}

#' @export
as.array.cubes <- function(x, ...) {
  stopifnot(is_cube(x))

  structure(x,
            # why?
            class = NULL,
            dims = NULL)
}

#' @export
as.table.cubes <- function(x, ...) {
  stopifnot(is_cube(x))

  structure(as.array(x),
            class = c("table", "array"),
            dimnames = dimnames(x))
}



# Ops ---------------------------------------------------------------------

broadcast <- function(x, dims) {
  old_dims <- dimnames(x)

  if (identical(old_dims, dims)) {
    x
  } else {
    # Broadcast coordinates
    old_axes <- names(old_dims)
    cmn_dims <- dims[old_axes]
    new_coords <- mapply(setdiff, cmn_dims, old_dims,
                         SIMPLIFY = FALSE)
    args <- mapply(vctrs::vec_match, cmn_dims, old_dims,
                   SIMPLIFY = FALSE)
    x <- slice(x, !!!args)
    attr(x, "dims") <- cmn_dims

    # Broadcast axes
    new_axes <- setdiff(names(dims), old_axes)
    new_dims <- dims[new_axes]
    dims <- dimnames(x)

    for (axis in new_axes) {
      along <- length(dim(x)) + 1

      x <- rep(list(as.array(x)), length(new_dims[[axis]]))
      x <- abind::abind(x, along = along)
    }
    x <- new_cube(x,
                  dims = c(dims, new_dims))

    withRestarts({
      if (vctrs::vec_is_empty(new_axes)) {
        new_axes <- NULL
      } else {
        new_axes <- commas(new_axes)
        new_axes <- paste("New axes:", new_axes)
      }

      size_new_coords <- vapply(new_coords, length,
                                FUN.VALUE = integer(1),
                                USE.NAMES = FALSE)
      new_coords <- new_coords[size_new_coords > 0]

      if (vctrs::vec_is_empty(new_coords)) {
        new_coords <- NULL
      } else {
        new_coords <- vapply(new_coords, commas,
                             FUN.VALUE = character(1))
        new_coords <- data.frame(axis = names(new_coords),
                                 coord = unname(new_coords))
        new_coords <- paste(c("New coords:",
                              format_data_frame(new_coords)),
                            collapse = "\n")
      }

      warning(warningCondition(paste(c("Broadcasting,", new_axes, new_coords, ""),
                                     collapse = "\n"),
                               class = "warning_broadcast"))

      x
    },
    restart_broadcast = function() x)
  }
}

#' @export
Ops.cubes <- function(e1, e2) {
  if (rlang::is_scalar_atomic(e1)) {
    stopifnot(is_cube(e2))

    NextMethod()
  } else if (rlang::is_scalar_atomic(e2)) {
    stopifnot(is_cube(e1))

    NextMethod()
  } else {
    stopifnot(is_cube(e1),
              is_cube(e2))

    dims_e1 <- dimnames(e1)
    dims_e2 <- dimnames(e2)

    axes <- unique(c(names(dims_e1), names(dims_e2)))
    dims <- lapply(axes, function(x) unique(c(dims_e1[[x]], dims_e2[[x]])))
    names(dims) <- axes

    e1 <- broadcast(e1, dims)
    e2 <- broadcast(e2, dims)

    NextMethod()
  }
}



# Printing ----------------------------------------------------------------

#' @importFrom pillar obj_sum
#' @export
pillar::obj_sum
obj_sum.cubes <- function(x) {
  paste0("[cube: ", dim_sum(dim(x)), "]")
}
