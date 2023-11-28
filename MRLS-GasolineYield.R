# Modelo de regressao linear simples

if(!require(pacman)) install.packrams("pacman")
library(pacman)

pacman::p_load(tidyverse, plotly, lmtest, ggfortify, broom, ggExtra, car)

#Definição da notação do p-valor
options(scipen=999)

rm(list=ls(all=TRUE))

# Dados
dt <- read.csv("~/EstR/DadosProjetos/TestesQuantitativos/GasolineYield.csv")

glimpse(dt)

# x = pressure
#pressão de vapor do petróleo bruto (lbf/in2).

#y = yield
#proporção de petróleo bruto convertido em gasolina após destilação e fracionamento.

# Estatisticas descritivas
#y(resposta/dependente)=yield -- x(explicativa/independente)=pressure
summary(dt$yield)
summary(dt$pressure)
cor(dt$pressure, dt$yield)

# Histograma
resposta <- ggplot(dt, aes(x = pressure))+
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 bins = 7, fill="blue")+
  scale_y_continuous(labels = scales::percent)+
  labs(y="FR",
       x="yield ",
       title="Histograma")+
  theme(text = element_text(size = 12))
ggplotly(resposta)

# Verificar - Grafico de dispersao
disp <- ggplot(data = dt, aes(x = pressure, y = yield)) +
  geom_point(color='blue') +
  ggtitle("Grafico de dispersao") +
  geom_smooth(method = "lm", se = TRUE)
ggplotly(disp)

# Importante - Grafico de dispersão e boxplots marginais
disp2 <- ggplot(data = dt, aes(x = pressure, y = yield)) +
  geom_point(color='blue') +
  ggtitle("Grafico de dispersao e boxplots marginais") +
  geom_smooth(method = "lm", se = TRUE)
ggplotly(disp2)

ggMarginal(disp2,
           type = "boxplot",
           fill = "lightblue")


# Teste de correlacao
cor.test(dt$yield, dt$pressure)


# Modelo de Regressao linear
# y ~ x (y = resposta, x = explicativa)
modelo <- lm(yield ~ pressure, data = dt)
summary(modelo)
confint(modelo)

autoplot(modelo)

### verificar
res <- residuals(modelo) %>%
  as.data.frame()
names(res) <- "residuos"
res$x <- as.numeric(rownames(res))
ggres <- res %>%
  ggplot(aes(x = x, y = residuos)) +
  geom_point(color='blue', size = 3) +
  ggtitle("Residuos do modelo") +
  geom_hline(aes(yintercept = 0, colour = "red"), linewidth=1) +
  guides(color = FALSE, size = none)

# Verificar - Grafico com boxplot
ggMarginal(ggres,
           type = "boxplot",
           fill = "lightblue",
           margins = "y")

# Verificar - grafico com histograma
ggMarginal(ggres,
           type = "histogram",
           bins = ceiling(sqrt(nrow(res)))+2,
           fill = "lightblue",
           margins = "y")

# Verificar - gráfico de resíduos
model.diag.metrics <- augment(modelo)
head(model.diag.metrics)
ggplot(model.diag.metrics, aes(pressure, yield)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = pressure, yend = .fitted), color = "red", linewidth = 0.3) +
  ggtitle("Residuos do modelo")


# ANOVA
summary(aov(modelo))

# Graficos para analise do modelo:
# Aqui sera feita a analise da suposicao de independencia, de
# normalidade e de homocedasticidade. Tambem e possivel avaliar os
# pontos mais influentes
autoplot(modelo)

# Suposições primárias

### 1 Teste de Independência - Durbin-Watson
# H0: autocorrelacao de residuos igual a zero
# H1: autocorrelacao de residuos maior zero
dwtest(modelo)

plot(residuals(modelo))

### 2 Linearidade Gráfico de resíduos x ^y
plot(modelo,1)

##3 3 Teste de Normalidade
# H0: residuos normais
# H1: residuos nao normais
shapiro.test( residuals(modelo) )

plot(modelo,2)

### 4 Teste de Homoscedasticidade Breusch-Pagan
# H0: residuos homocedasticos
# H1: residuos heteroscedasticos
bptest(modelo)

plot(modelo,3)

### 5 Ausencia de outliers influentes e pontos de alavancramm
# Outliers nos resíduos
summary(rstandard(modelo)) 

plot(modelo,5)

# Distancia de Cook (se precisar)
plot(modelo,4)

# gvlma::gvlma(modelo) ?
