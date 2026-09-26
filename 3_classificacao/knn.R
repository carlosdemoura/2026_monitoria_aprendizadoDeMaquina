library(tidymodels)

treino =
  treino |>
  mutate(
    sucesso = as.numeric(sucesso) - 1
  )

mod =
  nearest_neighbor(
    neighbors = 5,
    weight_func = "rectangular",
    dist_power = 2
  ) |>
  set_engine("kknn") |>
  set_mode("regression")

rec = 
  recipe(sucesso ~ ., data = treino) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

fit =
  wf |>
  fit(data = treino)

predict(fit, teste) |>
  bind_cols(real = teste$sucesso) 
