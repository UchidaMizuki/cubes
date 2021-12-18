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

size_dims <- function(x) {
  dm <- vapply(x, length,
               FUN.VALUE = integer(1),
               USE.NAMES = FALSE)
}

head_cubes <- function(x, n) {
  dims <- dimnames(x)
  n <- n %||% pillar:::get_pillar_option_print_max() + 1

  dm <- size_dims(dims)
  i <- cumprod(dm) < n
  dm_head <- unname(dm[i])

  if (all(i)) {
    dm_tail <- NULL
  } else {
    dm_tail <- c(ceiling(n / prod(dm_head)),
                 rep(1, sum(!i) - 1))
  }
  args <- lapply(c(dm_head, dm_tail), seq_len)

  slice(x, !!!args)
}

dim_sum <- function(x) {
  paste(dim(x),
        collapse = " x ")
}
