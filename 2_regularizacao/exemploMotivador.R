library(tidyverse)

dados = tibble(
  PIB = rnorm(100, 100, 5),
  IDH = 1+2*PIB+rnorm(100,0,2),
  literacia = 10 + 1.2 * IDH + rnorm(100,0,1)
)

lm(literacia ~ PIB, data = dados) |>
  summary()

with(dados, plot(literacia, IDH))
