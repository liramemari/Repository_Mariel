library(openxlsx)
library(visreg)
library(AICcmodavg)
library(ggplot2)
library(MASS)

setwd(choose.dir()) # vai abrir uma janela pra escolher a pasta onde esta a planilha com os dados
dados = read.xlsx("testbox.xlsx")

mod1 = glm(Numero_de_aves ~ Area + FE + ESC + ZR + A_verde, family = poisson, data = dados)
summary.glm(mod1) # veja os coeficientes estimados pelo modelo
anova(mod1, test = "LRT") # use o teste de significancia daqui, e nao do summary! (LRT = Likelihood Ratio)
plot(mod1) # para ver a distribuicao dos residuos
visreg(mod1) # para ver o grafico do modelo

ggplot(dados, aes(Localidade, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()

ggplot(dados, aes(Idade, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()

ggplot(dados, aes(Escolaridade, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()

ggplot(dados, aes(Moradia_zona_rural, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()

ggplot(dados, aes(Contato_area_verde, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()
par(mfrow=c(2,2))

hist(dados$Numero_de_aves)
