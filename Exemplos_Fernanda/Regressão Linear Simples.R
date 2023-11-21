
######################### Regressão Linear Simples #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpmisc)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diret?rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 11.csv',
                   fileEncoding = "latin1") # Carregamento do arquivo csv
View(dados)                                 # Visualiza??o dos dados em janela separada
glimpse(dados)                              # Visualiza??o de um resumo dos dados



# Passo 3: Verifica??o dos pressupostos para a regress?o linear


## Rela??o linear entre a VD e a VI:
### VD: Vendas
### VI: Publicidade

plot(dados$Publicidade, dados$Vendas)


## Constru??o do modelo:
mod <- lm(Vendas ~ Publicidade, dados)


## An?lise gr?fica:

par(mfrow=c(2,2))

plot(mod)

### Interpreta??o: https://data.library.virginia.edu/diagnostic-plots/

par(mfrow=c(1,1))


## Normalidade dos res?duos:
shapiro.test(mod$residuals)


## Outliers nos res?duos:
summary(rstandard(mod))


## Independ?ncia dos res?duos (Durbin-Watson):
durbinWatsonTest(mod)


## Homocedasticidade (Breusch-Pagan):
bptest(mod)


# Passo 4: An?lise do modelo
summary(mod)


ggplot(data = dados, mapping = aes(x = Publicidade, y = Vendas)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label),
                                 sep = "*plain(\",\")~~")),
               label.x = 0.05, label.y = 400,
               parse = TRUE, coef.digits = 5) +
  theme_classic()


# https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph

