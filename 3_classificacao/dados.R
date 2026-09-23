library(tidymodels)

set.seed(12345)
n = 1000
dados = tibble(
  idade = rnorm(n, mean = 40, sd = 10),
  renda = rlnorm(n, meanlog = log(3000), sdlog = 0.4),
  horas_estudo = rnorm(n, mean = 8, sd = 3),
  experiencia = rnorm(n, mean = 10, sd = 5),
  escolaridade = sample(
    c("medio", "superior", "pos"),
    n,
    replace = TRUE,
    prob = c(.4, .45, .15)
  )
)

eta =
  -4 +
  0.04 * dados$idade +
  0.0002 * dados$renda +
  0.15 * dados$horas_estudo +
  0.10 * dados$experiencia +
  ifelse(dados$escolaridade == "superior", 0.8, 0) +
  ifelse(dados$escolaridade == "pos", 1.5, 0)

p = plogis(eta)

dados =
  dados |>
  mutate(
    sucesso = rbinom(n, 1, p) |> factor()
  )

dados


set.seed(12345)
split = initial_split(dados, prop = 0.8, strata = sucesso)
treino = training(split)
teste  = testing(split)