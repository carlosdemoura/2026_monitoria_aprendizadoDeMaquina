set.seed(12345)
folds = vfold_cv(treino, v = 10, strata = sucesso)

mod =
  logistic_reg(
    penalty = tune(),
    mixture = tune()
    ) |>
  set_engine("glmnet") |>
  set_mode("classification")

grid_reg = grid_regular(
  penalty(),
  mixture(),
  levels = 10
)

metricas = metric_set(
  roc_auc,
  accuracy,
  sens,
  spec
)

rec = 
  recipe(sucesso ~ ., data = treino) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)


set.seed(12345)

out = tune_grid(
  wf,
  resamples = folds,
  grid = grid_reg,
  metrics = metricas
)

collect_metrics(out)
