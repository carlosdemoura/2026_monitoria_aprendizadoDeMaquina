library(tidymodels)

set.seed(12345)
dados100 = tibble(x = rnorm(100))
dados20 = tibble(x = rnorm(20))

###  n = 100  ###

set.seed(12345)
split100 = initial_split(dados100, prop = 0.8, strata = x)
dados100_treino = training(split100)
dados100_teste  = testing(split100)

par(mfrow=c(1,3))
hist(dados100$x)
hist(dados100_teste$x)
hist(dados100_treino$x)

summary(dados100$x)
summary(dados100_teste$x)
summary(dados100_treino$x)


###  n = 20  ###

set.seed(12345)
split20 = initial_split(dados20, prop = 0.8, strata = x)
dados20_treino = training(split20)
dados20_teste  = testing(split20)


par(mfrow=c(1,3))
hist(dados20$x)
hist(dados20_teste$x)
hist(dados20_treino$x)

summary(dados20$x)
summary(dados20_teste$x)
summary(dados20_treino$x)



###  dados categóricos  ###

set.seed(12345)
dados = tibble(x = sample(LETTERS[1:3], 100, replace = T))

set.seed(12345)
split = initial_split(dados, prop = 0.8, strata = x)
dados_treino = training(split)
dados_teste  = testing(split)

table(dados$x) |> {\(.) 100*./sum(.)}() |> round(1)
table(dados_treino$x) |> {\(.) 100*./sum(.)}() |> round(1)
table(dados_teste$x) |> {\(.) 100*./sum(.)}() |> round(1)

