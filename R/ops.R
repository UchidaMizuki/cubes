
# broadcasting!!!

broadcast <- function(x, dims) {
  old_dims <- dimnames(x)

  if (identical(old_dims, dims)) {
    list(result = x,
         new_axes = NULL,
         new_coords = NULL)
  } else {
    # broadcast coords
    old_axes <- names(old_dims)
    cmn_dims <- dims[old_axes]
    new_coords <- mapply(setdiff, cmn_dims, old_dims,
                        SIMPLIFY = FALSE)
    args <- mapply(vctrs::vec_match, cmn_dims, old_dims,
                   SIMPLIFY = FALSE)
    x <- slice(x, !!!args)
    attr(x, "dims") <- cmn_dims

    # broadcast axes
    new_axes <- setdiff(names(dims), old_axes)

  }
}

#' @export
Ops.cube <- function(e1, e2) {
  stopifnot(is_cube(e2))

  dims_e1 <- dimnames(e1)
  dims_e2 <- dimnames(e2)

  axes <- unique(c(names(dims_e1), names(dims_e2)))
  dims <- lapply(axes,
                 function(x) unique(c(dims_e1[[x]], dims_e2[[x]])))
  names(dims) <- axes

  e1 <- broadcast(e1, dims)
  e2 <- broadcast(e2, dims)

  stopifnot(identical(dims_e1, dims_e2))

  NextMethod()
}
