library(tidyverse)

f <- function(x, y) {
  out <- x * y
  withRestarts({
    warning(warningCondition("aaa",
                             class = "aaa"))
    out
  },
  g = function() out)
}

f(2, 3)

withCallingHandlers(f(2, 3),
                    aaa = function(w) {
                      invokeRestart("g")
                    })

x <- expand_grid(from = factor(letters[1:10]),
                 to = letters[1:10],
                 mode = letters[1:10]) %>%
  mutate(value = row_number(),
         value_2 = row_number()) %>%
  as_house(dims = list("from", "to", "mode"))
# dims = list(from = letters[2:4], "to"))

n <- 1e1
x <- array(dim = c(n, n, n),
           dimnames = list(x = 1:n,
                           y = 1:n,
                           z = 1:n)) %>%
  as.table()
as_tibble(x)
