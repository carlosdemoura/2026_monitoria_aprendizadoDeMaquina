# pak::pak("xgboost")

library(tidymodels)

mod =
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    min_n = tune()
  ) |>
  set_engine("xgboost") |>
  set_mode("classification")

rec = recipe(classe ~ ., data = treino)

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

grid =
  grid_regular(
    trees(range = c(100, 500)),
    tree_depth(range = c(1, 5)),
    learn_rate(range = c(-3, -1)),
    loss_reduction(),
    min_n(range = c(2, 10)),
    levels = 2
  )

out =
  tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(roc_auc)
  )

show_best(out, metric = "roc_auc")
