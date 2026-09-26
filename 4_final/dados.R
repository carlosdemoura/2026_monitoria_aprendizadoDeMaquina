library(tidyverse)
library(tidymodels)

set.seed(12345)

n = 100

# Classe 1
x1 = matrix(
  rnorm(2 * n, mean = c(-2, -2), sd = 0.7),
  ncol = 2,
  byrow = TRUE
)

# Classe 2
x2 = matrix(
  rnorm(2 * n, mean = c(2, 2), sd = 0.7),
  ncol = 2,
  byrow = TRUE
)

df =
  rbind(x1, x2) |>
  `colnames<-`(c("x1", "x2")) |>
  as_tibble() |>
  mutate(
    classe = factor(rep(c("A", "B"), each = n))
  )

plot(df$x1, df$x2, col = df$classe, pch = 19)
legend("topleft", legend = levels(df$classe), col = 1:2, pch = 19)

set.seed(12345)
split = initial_split(df, prop = 0.8, strata = classe)
treino = training(split)
teste  = testing(split)

set.seed(12345)
folds = vfold_cv(treino, v = 10, strata = classe)


# treino =
#   treino |>
#   bind_rows(
#     tibble(x1 = -2, x2 = 2, classe = factor("A"))
#   )
# df =
#   df |>
#   bind_rows(
#     tibble(x1 = -2, x2 = 2, classe = factor("A"))
#   )
# 
# with(treino, plot(x1, x2, col = classe, pch = 19))
