set.seed(12345)
folds = vfold_cv(treino, v = 10, strata = sucesso)

mod =
  logistic_reg( penalty = tune(), mixture = tune() ) |>
  set_engine("glmnet") |>
  set_mode("classification")

rec = 
  recipe(sucesso ~ ., data = treino) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)




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

out = tune_grid(
  wf,
  resamples = folds,
  grid = grid_reg,
  metrics = metricas
)

collect_metrics(out) |>
  select(-c(n,std_err,.config,.estimator)) |>
  pivot_wider(c(penalty, mixture), names_from = ".metric", values_from = "mean")
