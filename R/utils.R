as_list_dims <- function(x) {
  x <- as.list(x)
  nms <- rlang::names2(x)
  for (i in seq_along(x)) {
    if (nms[[i]] == "") {
      names(x)[[i]] <- x[[i]]
      x[i] <- list(NULL)
    } else {
      stopifnot(is.null(x[[i]]) || !vctrs::vec_duplicate_any(x[[i]]))
    }
  }
  x
}

head_dims <- function(dims, n = NULL) {
  n <- n %||% pillar:::get_pillar_option_print_max() + 1

  dm <- purrr::map_int(dims, length)
  i <- cumprod(dm) < n
  dm_head <- unname(dm[i])

  if (all(i)) {
    dm_tail <- NULL
  } else {
    dm_tail <- c(ceiling(n / prod(dm_head)),
                 rep(1, sum(!i) - 1))
  }
  lapply(c(dm_head, dm_tail), seq_len)
}

head_room <- function(x, n) {
  dims <- dimnames(x)
  dims_head <- head_dims(dims, n)
  slice(x, !!!dims_head)
}
