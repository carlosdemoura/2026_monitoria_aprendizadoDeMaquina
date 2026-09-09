library(tidymodels)

set.seed(12345)
dados = tibble(
  x = runif(1000,0,10),
  y = 2 + 0.5 * x - 0.05 * x^2 + rnorm(length(x))
)


set.seed(12345)
split = initial_split(dados, prop = 0.8, strata = y)
dados_treino = training(split)
dados_teste  = testing(split)

rec = 
  recipe(y ~ x, data = dados_treino) |>
  step_poly(x, degree = 2)

mod =
  linear_reg() |>
  set_engine("lm")

wf = workflow() |>
  add_recipe(rec) |>
  add_model(mod)

fit = fit(wf, data = dados_treino)


predict =
  predict(fit, dados_teste) |>
  bind_cols(dados_teste["y"])

metrics(predict, truth = y, estimate = .pred)
