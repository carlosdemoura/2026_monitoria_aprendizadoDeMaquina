library(tidymodels)
# pak::pak("kknn")

data(Boston, package = "MASS")

folds = vfold_cv(Boston, v = 10)

grid =
  tibble(
    neighbors = seq(1, 10, by = 2)
  )

rec =
  recipe(medv ~ ., data = Boston) |>
  step_normalize(all_predictors())

mod =
  nearest_neighbor(
    neighbors = tune(),
    weight_func = "rectangular",
    dist_power = 2
  ) |>
  set_engine("kknn") |>
  set_mode("regression")

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

tunagem =
  tune_grid(
    wf,
    resamples = folds,
    grid = grid
  )

collect_metrics(tunagem)
