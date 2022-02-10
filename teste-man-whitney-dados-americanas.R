
library(dplyr)
library(rstatix)
library(ggplot2)
options(scipen = 999)

####################### Carregando dados #################################
dados <- read.csv("~/04 - Projetos/02 - Python/04 - Case americanas/Teste/joint_base.csv", sep = ';', dec = ',')
dados = dados[dados$Departamento != 'Maquinas Fotogr?ficas', ]
dados$Departamento = factor(dados$Departamento)


####################### Realizando o teste #################################
wilcox.test(Custo_Mkt ~ Departamento, dados)

# histograma
par(mfrow=c(2,2))
hist(dados$Custo_Mkt[dados$Departamento == "Bolas de Gude"],
     ylab="Frequ�ncia", xlab="Custo MKT", main="Bolas de Gude")
hist(dados$Custo_Mkt[dados$Departamento == "Cadeiras"],
     ylab="Frequ�ncia", xlab="Custo MKT", main="Cadeiras")

# boxplot
#par(mfrow=c(1,2))
boxplot(dados$Custo_Mkt[dados$Departamento == "Bolas de Gude"],
        ylab="Frequ�ncia", xlab="Custo MKT", main="Bolas de Gude")
boxplot(dados$Custo_Mkt[dados$Departamento == "Cadeiras"],
        ylab="Frequ�ncia", xlab="Custo MKT", main="Cadeiras")

#
wilcox.test(Desconto_Produto ~ Departamento, dados)



par(mfrow=c(1,2))

hist(dados$Desconto_Produto[dados$Departamento == "Bolas de Gude"],
     ylab="Frequ�ncia", xlab="Desconto_Produto", main="Bolas de Gude")
hist(dados$Desconto_Produto[dados$Departamento == "Cadeiras"],
     ylab="Frequ�ncia", xlab="Desconto_Produto", main="Cadeiras")

#
wilcox.test(Faturamento_Produto ~ Departamento, dados)

par(mfrow=c(1,2))

hist(dados$Faturamento_Produto[dados$Departamento == "Bolas de Gude"],
     ylab="Frequ�ncia", xlab="Faturamento_Produto", main="Bolas de Gude")
hist(dados$Faturamento_Produto[dados$Departamento == "Cadeiras"],
     ylab="Frequ�ncia", xlab="Faturamento_Produto", main="Cadeiras")

#
wilcox.test(Faturamento_Frete ~ Departamento, dados)

par(mfrow=c(1,2))

hist(dados$Faturamento_Frete[dados$Departamento == "Bolas de Gude"],
     ylab="Frequ�ncia", xlab="Faturamento_Frete", main="Bolas de Gude")
hist(dados$Faturamento_Frete[dados$Departamento == "Cadeiras"],
     ylab="Frequ�ncia", xlab="Faturamento_Frete", main="Cadeiras")

####################### Interpreta��o do teste #################################

# p-valor > 0.05 -> h0: mediana grupo A = mediana grupo B
# p-valor <= 0.05 -> h1: mediana grupo A != mediana grupo B

####################### An�lise descritiva dos dados ###########################

dados %>% group_by(Departamento) %>% 
  get_summary_stats(Custo_Mkt, Desconto_Produto, Faturamento_Produto, 
                    Faturamento_Frete, type = "median_iqr")

# IQR = amplitude interquartil. Quando um quantil varia pro outro




