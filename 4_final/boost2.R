library(tidyverse)
library(tidymodels)

set.seed(123)

n = 300

f = function(xx) {
  g = function(x) {
    if (x < 1) {
      return(2 + 2 * x + 3 * x^2)
    } else {
      10
    }
  }
  lapply(xx, g) |> unlist()
}

df =
  tibble(
    x = runif(n, -2, 4),
    erro = rnorm(n, sd = 1)
  ) |>
  mutate(
    y = f(x) + erro
  )

ggplot(df, aes(x, y)) +
  geom_point() +
  theme_minimal()

mod =
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    min_n = tune()
  ) |>
  set_engine("xgboost") |>
  set_mode("regression")

rec =
  recipe(y ~ x, data = df)

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)





grid =
  grid_regular(
    trees(range = c(100, 500)),
    tree_depth(range = c(1, 5)),
    learn_rate(range = c(-3, -1)),
    loss_reduction(),
    min_n(range = c(2, 10)),
    levels = 2
  )

set.seed(123)

folds =
  vfold_cv(df, v = 5)

out =
  tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(rmse)
  )

show_best(out, metric = "rmse")

fit_boost =
  wf |>
  finalize_workflow(select_best(out, metric = "rmse")) |>
  fit(df)

df_plot =
  df |>
  mutate(
    y_pred = predict(fit_boost, new_data = df)$.pred
  )

ggplot(df_plot, aes(y, y_pred)) +
  geom_point() +
  geom_abline(linetype = 2) +
  labs(
    x = "Observado",
    y = "Predito"
  ) +
  theme_minimal()






x_grid = tibble(x = seq(min(df$x), max(df$x), length.out = 500))

pred = predict(fit_boost, new_data = x_grid)

df_curve = bind_cols(x_grid, pred)

ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_line(
    data = df_curve,
    aes(x, .pred),
    linewidth = 1
  ) +
  theme_minimal()
