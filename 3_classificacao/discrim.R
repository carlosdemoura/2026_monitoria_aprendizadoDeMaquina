library(tidymodels)
library(discrim)

set.seed(12345)
folds = vfold_cv(treino, v = 10, strata = sucesso)

mod =
  discrim_linear() |>
  # discrim_quad() |>
  set_engine("MASS") |>
  set_mode("classification")

rec = recipe(sucesso ~ ., data = treino) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

metricas = metric_set(
  roc_auc,
  accuracy,
  sens,
  spec
)

out = fit_resamples(
  wf,
  resamples = folds,
  metrics = metricas
)

collect_metrics(out)