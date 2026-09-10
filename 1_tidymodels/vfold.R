library(tidymodels)

set.seed(12345)
dados = tibble(
  x = runif(1000,0,10),
  y = 2 + 0.5 * x - 0.05 * x^2 + rnorm(length(x))
)


folds = vfold_cv(
  dados,
  v = 10
)

rec =
  recipe(y ~ x, data = dados) |>
  step_poly(x, degree = 3)

mod =
  linear_reg() |>
  set_engine("lm")

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

res =
  fit_resamples(
    wf,
    resamples = folds,
    metrics = metric_set(rmse, mae, rsq)
  )

collect_metrics(res)
