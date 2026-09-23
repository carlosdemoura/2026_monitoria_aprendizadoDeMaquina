library(tidymodels)
# pak::pak("kknn")

data(Boston, package = "MASS")

set.seed(12345)
split = initial_split(Boston, prop = 0.8, strata = medv)
dados_treino = training(split)
dados_teste  = testing(split)



rec =
  recipe(medv ~ ., data = dados_treino) |>
  step_normalize(all_predictors())

mod =
  nearest_neighbor(
    neighbors = 5,
    weight_func = "rectangular",
    dist_power = 2
  ) |>
  set_engine("kknn") |>
  set_mode("regression")

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

fit =
  wf |>
  fit(data = dados_treino)


predict(fit, dados_teste) |>
  bind_cols(real = dados_teste$medv) 
