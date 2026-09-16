library(tidymodels)
# pak::pak("kknn")

data(Boston, package = "MASS")

rec =
  recipe(medv ~ ., data = Boston) |>
  step_normalize(all_predictors())

mod =
  nearest_neighbor(
    neighbors = 5,
    weight_func = "rectangular",
    dist_power = 2
  ) |>
  set_engine("kknn") |>
  set_mode("regression")

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

fit =
  wf |>
  fit(data = Boston)

