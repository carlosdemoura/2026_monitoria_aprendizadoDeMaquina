set.seed(12345)
folds = vfold_cv(treino, v = 10, strata = sucesso)

mod =
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  ) |>
  set_engine("rpart") |>
  set_mode("classification")

rec = recipe(sucesso ~ ., data = treino)

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

grid_tree =
  grid_regular(
    cost_complexity(),
    tree_depth(),
    min_n(),
    levels = 5
  )

metricas = metric_set(
  roc_auc,
  accuracy,
  sens,
  spec
)

out = tune_grid(
  wf,
  resamples = folds,
  grid = grid_tree,
  metrics = metricas
)

show_best(out, metric = "roc_auc")