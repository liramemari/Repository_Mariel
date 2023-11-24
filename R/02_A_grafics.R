library(openxlsx)
library(visreg)
library(AICcmodavg)
library(ggplot2)
library(MASS)

setwd(choose.dir()) # vai abrir uma janela pra escolher a pasta onde esta a planilha com os dados
dados = read.xlsx("testbox.xlsx")

ggplot(dados, aes(Localidade, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()+ 
  labs(x = "Localidade",
       y = "Número de aves citadas")

ggplot(dados, aes(Idade, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()+ 
  labs(x = "Idade",
       y = "Número de aves citadas")

ggplot(dados, aes(Escolaridade, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()+ 
  labs(x = "Escolaridade",
       y = "Número de aves citadas")

ggplot(dados, aes(Moradia_zona_rural, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()+ 
  labs(x = "Moradia na zona rural",
       y = "Número de aves citadas")

ggplot(dados, aes(Contato_area_verde, Numero_de_aves))+
  stat_summary(geom = "point", fun = mean, size = 2)+
  stat_summary(geom = "errorbar", width = 0.5, fun.data = mean_cl_normal)+
  theme_classic()+ 
  labs(x = "Contato com área verde",
       y = "Número de aves citadas")

setwd(choose.dir()) # vai abrir uma janela pra escolher a pasta onde esta a planilha com os dados
dados = read.xlsx("testranking.xlsx")

ggplot(dados, aes(x= reorder(Sp_gyn,-tot_gyn), y= tot_gyn), beside = TRUE, 
       width = 1, 
       ylim = c(0, 0), 
       las = 0.5) +
geom_bar(stat = 'identity', color= 'black', fill= 'lightgreen')+
  labs (x="Citação por nome popular em Goiânia",
        y= "Quantidade de citações")+
  theme_classic()

ggplot(dados, aes(x= reorder (Sp_AB, -tot_AB), y= tot_AB)) +
  geom_bar(stat = 'identity', color= 'black', fill= 'violet')+
  labs (x="Citação por nome popular em Aragarças e Barra",
        y= "Quantidade de citações")+
theme_classic()

ggplot(dados, aes(x= reorder(Sp_geral, -Total), y= Total)) +
  geom_bar(stat = 'identity', color= 'black', fill= 'lightblue')+
  labs (x="Citação por nome popular geral",
        y= "Quantidade de citações")+
theme_classic()
