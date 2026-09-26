library(tidymodels)

mod =
  svm_rbf(
    cost = tune(),
    rbf_sigma = tune()
  ) |>
  set_engine("kernlab") |>
  set_mode("classification")

rec =
  recipe(classe ~ ., data = treino)

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

grid =
  grid_regular(
    cost(),
    rbf_sigma(),
    levels = 2
  )

out =
  tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(roc_auc)
  )

show_best(out, metric = "roc_auc")



best = select_best(out, metric = "roc_auc")

fit_svm =
  wf |>
  finalize_workflow(best) |>
  fit(data = treino)

grid =
  expand.grid(
    x1 = seq(min(treino$x1), max(treino$x1), length.out = 300),
    x2 = seq(min(treino$x2), max(treino$x2), length.out = 300)
  )

grid$.prob = predict(fit_svm, new_data = grid, type = "prob")[[2]]

ggplot() +
  geom_point(
    data = treino,
    aes(x1, x2, color = classe),
    size = 2
  ) +
  geom_contour(
    data = grid,
    aes(x1, x2, z = .prob),
    breaks = 0.5,
    linewidth = 1
  ) +
  theme_void()
