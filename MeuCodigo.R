# Modelo de regressao linear simples

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse, plotly, lmtest, ggfortify, broom, ggExtra, car)

#Definição da notação do p-valor
options(scipen=999)

rm(list=ls(all=TRUE))

# Dados
dt <- read.csv("~/EstR/DadosProjetos/TestesQuantitativos/FoodExpenditure.csv")

glimpse(dt)

# Estatisticas descritivas
#y(resposta/dependente)=salário -- x(explicativa/independente)=experiência
summary(dt$food)
summary(dt$income)
cor(dt$income, dt$food)


#food
#despesas domésticas com alimentação.

#income
#renda familiar.

resposta <- ggplot(dt, aes(x = income))+
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 bins = 7, fill="blue")+
  scale_y_continuous(labels = scales::percent)+
  labs(y="FR",
       x="food (em *preencher unidade*)",
       title="Histograma")+
  theme(text = element_text(size = 12))
ggplotly(resposta)

disp <- ggplot(data = dt, aes(x = income, y = food)) +
  geom_point(color='blue') +
  ggtitle("Grafico de dispersao") +
  geom_smooth(method = "lm", se = TRUE)
ggplotly(disp)

disp2 <- ggplot(data = dt, aes(x = income, y = food)) +
  geom_point(color='blue') +
  ggtitle("Grafico de dispersao e boxplots marginais") +
  geom_smooth(method = "lm", se = TRUE)
ggplotly(disp2)

ggMarginal(disp,
           type = "boxplot",
           fill = "lightblue")


# Teste de correlacao
cor.test(dt$food, dt$income)


# Regressao linear
# y ~ x (y = resposta, x = explicativa)
modelo <- lm(food ~ income, data = dt)
summary(modelo)
confint(modelo)

# Graficos para analise do modelo:
# Aqui sera feita a analise da suposicao de independencia, de
# normalidade e de homocedasticidade. Tambem e possivel avaliar os
# pontos mais influentes
autoplot(modelo)


# Outra forma
plot(residuals(modelo))
plot(modelo,1)
plot(modelo,2)
plot(modelo,3)
plot(modelo,4)
plot(modelo,5)


res <- residuals(modelo) %>%
  as.data.frame()
names(res) <- "residuos"
res$x <- as.numeric(rownames(res))
ggres <- res %>%
  ggplot(aes(x = x, y = residuos)) +
  geom_point(color='blue', size = 3) +
  ggtitle("Residuos do modelo") +
  geom_hline(aes(yintercept = 0, colour = "red"), size=1) +
  guides(color = FALSE, size = FALSE)

ggMarginal(ggres,
           type = "boxplot",
           fill = "lightblue",
           margins = "y")

ggMarginal(ggres,
           type = "histogram",
           bins = ceiling(sqrt(nrow(res)))+2,
           fill = "lightblue",
           margins = "y")


model.diag.metrics <- augment(modelo)
head(model.diag.metrics)
ggplot(model.diag.metrics, aes(income, food)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = income, yend = .fitted), color = "red", size = 0.3) +
  ggtitle("Residuos do modelo")


# ANOVA
summary(aov(modelo))

# Normalidade
# H0: residuos normais
# H1: residuos nao normais
shapiro.test( residuals(modelo) )


# Realizar o teste de Breusch-Pagan
# H0: residuos homocedasticos
# H1: residuos heteroscedasticos
bptest(modelo)

# Teste de Durbin-Watson
# H0: autocorrelacao de residuos igual a zero
# H1: autocorrelacao de residuos maior zero
dwtest(modelo)


# gvlma::gvlma(modelo)

