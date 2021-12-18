# library(tidyverse)
library(magrittr)

f <- function(x, y) {
  out <- x * y
  withRestarts({
    warning(warningCondition("aaa",
                             class = "aaa"))
    # message(structure(list(message = "aaa\n"),
    #                   class = c("aaa", "condition")))
    out
  },
  g = function() out)
}

f(2, 3)

withCallingHandlers(f(2, 3),
                    aaa = function(w) {
                      invokeRestart("g")
                    })

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

x$value * x1$value_2
