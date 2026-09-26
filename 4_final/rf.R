library(tidymodels)

# pak::pak('ranger')

mod =
  rand_forest(
    trees = 500,
    mtry = tune(),
    min_n = tune()
  ) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rec =
  recipe(classe ~ ., data = treino)

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

grid =
  grid_regular(
    mtry(range = c(1, 2)),
    min_n(),
    levels = c(7, 5)
  )

set.seed(123)

res =
  tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(roc_auc)
  )

show_best(res,  metric = "roc_auc")

##  plot  ##

fit =
  wf |>
  finalize_workflow(select_best(res, metric = "roc_auc")) |>
  fit(data = treino) |>
  extract_fit_parsnip()

grid =
  expand.grid(
    x1 = seq(min(treino$x1), max(treino$x1), length.out = 200),
    x2 = seq(min(treino$x2), max(treino$x2), length.out = 200)
  )

grid$classe = predict(fit, new_data = grid)$.pred_class

ggplot() +
  geom_tile(
    data = grid,
    aes(x1, x2, fill = classe),
    alpha = 0.3
  ) +
  geom_point(
    data = treino,
    aes(x1, x2, color = classe),
    size = 2
  ) +
  theme_minimal()
