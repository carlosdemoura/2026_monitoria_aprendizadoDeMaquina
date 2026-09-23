mod =
  logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

rec = 
  recipe(sucesso ~ ., data = treino) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

fit = fit(wf, data = treino)

resultado =
  predict(fit, teste, type = "class") |>
  bind_cols(teste["sucesso"])

conf_mat(resultado, truth = sucesso, estimate = .pred_class)
