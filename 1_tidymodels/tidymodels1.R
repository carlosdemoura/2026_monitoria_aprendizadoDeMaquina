library(tidymodels)

set.seed(12345)
dados = tibble(
  x = runif(1000,0,10),
  y = 2 + 0.5 * x - 0.05 * x^2 + rnorm(length(x))
)

rec = 
  recipe(y ~ x, data = dados) |>
  step_poly(x, degree = 2)

mod =
  linear_reg() |>
  set_engine("lm")

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

fit = fit(wf, data = dados)

fit

broom::tidy(fit)


predict =
  predict(fit, dados) |>
  bind_cols(dados["y"])

metrics(predict, truth = y, estimate = .pred)


metricas = list()
grid = tibble(
  x = seq(min(dados$x), max(dados$x), length.out = 100)
)
for (i in 1:9) {
  rec = 
    recipe(y ~ x, data = dados) |>
    step_poly(x, degree = i)
  mod =
    linear_reg() |>
    set_engine("lm")
  wf =
    workflow() |>
    add_recipe(rec) |>
    add_model(mod)
  fit = fit(wf, data = dados)
  
  predict =
    predict(fit, dados) |>
    bind_cols(dados["y"])

  grid =
    predict(fit, grid) |>
    `colnames<-`(paste0("grau=",i)) |>
    bind_cols(grid)
  
  metricas[[i]] =
    metrics(predict, truth = y, estimate = .pred) |>
    select(-.estimator) |>
    bind_cols(grau = i)
}

# metricas = do.call(rbind, metricas)  # ou
metricas = bind_rows(metricas)

metricas |>
  filter(.metric == "mae") |>
  # filter(.metric == "rmse") |>
  arrange(.estimate)

grid =
  grid |>
  pivot_longer(-x, values_to = "predito", names_to = "grau") |>
  mutate(
    grau = substr(grau,6,6) |> as.numeric()
  )

ggplot(grid, aes(x, predito, color = factor(grau))) +
  geom_line(linewidth = 1) +
  # geom_point(
  #   data = dados,
  #   aes(x, y),
  #   inherit.aes = FALSE,
  #   color = "black"
  # ) +
  labs(
    color = "Grau"
  )
