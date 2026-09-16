library(tidymodels)

set.seed(123)

dados =
  tibble(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    y = 2 + 3 * x1 + 2 * x3+ rnorm(100),
  )

mod = lm(y ~ 1, data = dados)

step = MASS::stepAIC(mod, direction = "forward", scope = ~x1+x2+x3, trace = T)
