######################### Teste t Pareado #########################


# Passo 1: Carregar os pacotes que ser�o usados

if(!require(dplyr)) install.packages("dplyr") # Instala��o do pacote caso n�o esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(psych)) install.packages("psych") # Instala��o do pacote caso n�o esteja instalado
library(psych)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diret�rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de c�digo abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Dados/Banco de Dados 4.csv', sep = ';', dec = ',') %>%
  rename(Convulsoes_PT = Convuls�es_PT, Convulsoes_S1 = Convuls�es_S1,
         Convulsoes_S6 = Convuls�es_S6, Genero = G�nero)
View(dados)                                              # Visualiza��o dos dados em janela separada
glimpse(dados)                                                 # Visualiza��o de um resumo dos dados

# Passo 3: Verifica��o da normalidade dos dados

dados$DiferencaPTS1 <- dados$Convulsoes_PT - dados$Convulsoes_S1

shapiro.test(dados$DiferencaPTS1)


# Passo 4: Realiza��o do teste t pareado

t.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = TRUE)


# Passo 5 (opcional): Visualiza��o da distribui��o dos dados

par(mfrow=c(2,2)) # Estabeleci que quero que os gr�ficos saiam na mesma linha
boxplot(dados$Convulsoes_PT, ylab="Quantidade de Convuls�es", xlab="Pr�-Tratamento")
boxplot(dados$Convulsoes_S1, ylab="Quantidade de Convuls�es", xlab="1� semana de Tratamento")

hist(dados$Convulsoes_PT, ylab="Quantidade de Convuls�es", xlab="Pr�-Tratamento")
hist(dados$Convulsoes_S1, ylab="Quantidade de Convuls�es", xlab="1� semana de Tratamento")

# Passo 6 (opcional): An�lise descritiva dos dados
summary(dados$Convulsoes_PT)
summary(dados$Convulsoes_S1)

## Outra forma: pela fun��o describe do pacote Psych
describe(dados$Convulsoes_PT)
describe(dados$Convulsoes_S1)