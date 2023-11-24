### TCC mariel

install.packages("readxl")
library(readxl)


# carregando dados
data <- read_xlsx('teste2.1.xlsx')
head(data)

# declarando fatores
data$Localidade <- as.factor(data$Localidade)
data$Idade <- as.factor(data$Idade)
data$Escolaridade <- as.factor(data$Escolaridade)

# resumo dados
summary(data)

par(mfrow=c(1,1))
hist(data$Numero_de_aves)
plot(Numero_de_aves~Localidade , data=data)
plot(Numero_de_aves~Idade , data=data)
plot(Numero_de_aves~Escolaridade , data=data)

## tetes anova fatores independetes

# localiadede
modelo1 <- lm(Numero_de_aves~Localidade , data=data)
summary(modelo1)
par(mfrow=c(2,2))
plot(modelo1)
anova(modelo1)

# Idade
modelo2 <- lm(Numero_de_aves~Idade , data=data)
summary(modelo2)
par(mfrow=c(2,2))
plot(modelo2)
anova(modelo2)

# escolaridade 
modelo3 <- lm(Numero_de_aves~Escolaridade , data=data)
summary(modelo3)
par(mfrow=c(2,2))
plot(modelo3)
anova(modelo3)


#tests modelo
modelo <- lm(Numero_de_aves~Localidade+Idade+Escolaridade, data=data)
plot (modelo)
summary(modelo)
anova(modelo)

