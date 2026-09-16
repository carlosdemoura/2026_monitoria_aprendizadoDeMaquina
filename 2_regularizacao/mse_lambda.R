library(tidymodels)

set.seed(12345)

n = 20
beta = exp(-seq(-2, 2, .5)^2)

X = rnorm(n * length(beta)) |>
  matrix(nrow = n, ncol = length(beta))

Y = X %*% beta + rnorm(n, 0, 1)

dados = as.data.frame(X)
dados$Y = drop(Y)

mod =
  linear_reg(penalty = 1, mixture = 0) |>
  set_engine("glmnet", intercept = FALSE)

wf =
  workflow() |>
  add_recipe(recipe(Y ~ 0 + ., data = dados)) |>
  add_model(mod)

fit =
  wf |>
  fit(data = dados)

fit_path = extract_fit_engine(fit)
lambdas = fit_path$lambda

M_beta_estimado = coef(fit_path)[-1,] |> as.matrix()
M_beta_real = replicate(length(lambdas), beta)

mse =
  colMeans((M_beta_estimado - M_beta_real)^2) |>
  rbind(lambdas) |>
  `rownames<-`(c("mse", "lambda"))

plot(
  mse["lambda", ],
  mse["mse", ],
  type = "l",
  lwd = 2,
  log = "x",
  xlab = expression(lambda),
  ylab = "MSE"
)
