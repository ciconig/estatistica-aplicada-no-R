######################### Teste de Mann-Whitney #########################


# Passo 1: Carregar os pacotes que ser�o usados

if(!require(dplyr)) install.packages("dplyr") # Instala��o do pacote caso n�o esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(dplyr)) install.packages("rstatix") # Instala��o do pacote caso n�o esteja instalado
library(rstatix)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diret�rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de c�digo abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 3.csv', sep = ';', dec = ',',
                  stringsAsFactors = T)           # Carregamento do arquivo csv
View(dados)                                       # Visualiza��o dos dados em janela separada
glimpse(dados)                                    # Visualiza��o de um resumo dos dados


# Passo 3: Realiza��o do teste de Mann-Whitney

wilcox.test(Nota_Biol ~ Posicao_Sala, data = dados)
wilcox.test(Nota_Fis ~ Posicao_Sala, data = dados)
wilcox.test(Nota_Hist ~ Posicao_Sala, data = dados)

# Observa��o:
# O teste bicaudal � o default; caso deseje unicaudal, necess�rio incluir:
# alternative = "greater" ou alternative = "less"
# Exemplo: wilcox.test(Nota_Hist ~ Posicao_Sala, data = dados, alternative="greater")
# Nesse caso, o teste verificar� se � a mediana do primeiro grupo � maior que a mediana do segundo
# O R est� considerando "Frente" como primeiro grupo


# Passo 4: An�lise descritiva dos dados

dados %>% group_by(Posicao_Sala) %>% 
  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "median_iqr")

# Dados param�tricos?
# dados %>% group_by(Posicao_Sala) %>% 
#  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "mean_sd")


# Passo 5: Visualiza��o da distribui��o
par(mfrow=c(1,2))
hist(dados$Nota_Biol[dados$Posicao_Sala == "Frente"],
     ylab="Frequ�ncia", xlab="Nota", main="Grupo Frente")
hist(dados$Nota_Biol[dados$Posicao_Sala == "Fundos"],
     ylab="Frequ�ncia", xlab="Nota", main="Grupo Fundos")