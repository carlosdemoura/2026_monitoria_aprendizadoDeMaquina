library(tidymodels)

folds = vfold_cv(
  dados,
  v = 3
)
metricas = vector("list", 30)

for (p in 1:9) {
  rec =
    recipe(y ~ x, data = dados) |>
    step_poly(x, degree = p)
  
  mod =
    linear_reg() |>
    set_engine("lm")
  
  wf =
    workflow() |>
    add_recipe(rec) |>
    add_model(mod)
  
  ajuste =
    fit_resamples(
      wf,
      resamples = folds,
      control = control_resamples(save_pred = TRUE)
    )
  
  pred =
    collect_predictions(ajuste)
  
  metricas[[p]] =
    tibble(
      p = p,
      risco = mean((pred$.pred - pred$y)^2)
    )
}

metricas =
  bind_rows(metricas)
