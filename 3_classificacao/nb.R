# pak::pak("discrim")

mod_nb <- naive_Bayes() |>
  set_engine("naivebayes") |>
  set_mode("classification")

rec_nb <- recipe(
  sucesso ~ .,
  data = treino
) |>
  step_dummy(all_nominal_predictors())

wf_nb <- workflow() |>
  add_recipe(rec_nb) |>
  add_model(mod_nb)

set.seed(123)

res_nb <- fit_resamples(
  wf_nb,
  resamples = folds,
  metrics = metricas
)

collect_metrics(res_nb)