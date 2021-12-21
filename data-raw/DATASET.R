# library(tidyverse)
library(magrittr)

x <- tidyr::expand_grid(from = factor(letters[1:10]),
                 to = letters[1:10],
                 mode = letters[1:10]) %>%
  dplyr::mutate(value = dplyr::row_number(),
         value_2 = dplyr::row_number()) %>%
  as_cubes(dims = list("from", "to", "mode"))

x1 <- tidyr::expand_grid(from = factor(letters[3:12]),
                         to = letters[3:12],
                         mode2 = letters[3:12]) %>%
  dplyr::mutate(value = dplyr::row_number(),
                value_2 = dplyr::row_number()) %>%
  as_cubes(dims = list("from", "to", "mode2"))

x2 <- tidyr::expand_grid(from = factor(letters[1:12]),
                         to = letters[1:12]) %>%
  dplyr::mutate(value = dplyr::row_number(),
                value_2 = dplyr::row_number()) %>%
  as_cubes(dims = list("from", "to"))

(x$value * x1$value_2) %>%
  group_by(from, to)



m <- matrix(1:4, c(2, 2))
class(m) <- "matrix_2"

# sum.matrix_2 <- function(x, ..., na.rm = FALSE) {
#   apply(x, 2, function(x) sum(x, ..., na.rm = na.rm))
# }
Summary.matrix_2 <- function(x, ..., na.rm = FALSE) {
  print("OK!")
  NextMethod()
}
sum(m)
mean(m)

sloop::s3_dispatch(sum(1))
