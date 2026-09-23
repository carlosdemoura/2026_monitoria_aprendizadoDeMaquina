library(tidymodels)
library(tidyverse)
# pak::pak("glmnet")

dados = tibble(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100),
  x4 = rnorm(100),
  y = 1+x1+2*x2+rnorm(100),
)

rec =
  recipe(y ~ ., data = dados) |>
  step_normalize(all_predictors())

mod =
  linear_reg(penalty = 0, mixture = 1) |>
  set_engine("glmnet")

wf =
  workflow() |>
  add_recipe(rec) |>
  add_model(mod)

fit =
  wf |>
  fit(data = dados)

fit_path = extract_fit_engine(fit)

fit_path = rbind(
  fit_path$lambda,
  coef(fit_path)
  )
rownames(fit_path)[1] = "lambda"

df_long =
  fit_path |>
  as.matrix() |>
  as.data.frame() |>
  rownames_to_column("term") |>
  pivot_longer(
    -term,
    names_to = "lambda",
    values_to = "estimate"
  ) |>
  group_by(lambda) |>
  mutate(
    lambda = estimate[term == "lambda"]
  ) |>
  ungroup() |>
  filter(term != "lambda") |>
  select(term, lambda, estimate)

ggplot(df_long, aes(x = lambda, y = estimate, group = term, colour = term)) +
  geom_line() +
  scale_x_log10() +
  labs(
    x = expression(lambda),
    y = "Standardized Coefficients",
    colour = NULL
  ) +
  theme_classic()
