######################### Carregando os dados ##################################


# Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") 
library(dplyr)                                


# Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Dados/Banco de Dados 2.csv', 
                  sep = ';', 
                  dec = ',',
                  stringsAsFactors = T) # Carregamento do arquivo csv
View(dados)                             # Visualização dos dados em janela separada
glimpse(dados)                          # Visualização de um resumo dos dados

######################### Teste de normalidade de Shapiro ######################

# Como interpretar o teste:

# p-value > 0.05 -> h0: distribuição dos dados = distribuição normal
# p-value <= 0.05 -> h1: distribuição dos dados != distribuição normal

# Verificação da normalidade dos dados

shapiro.test(dados$Altura)
shapiro.test(dados$Idade)


######################### Teste de t para uma amostra ##########################

# Realização do teste t para uma amostra
# interpretação do teste t para uma amostra:

# p-value > 0.05 -> h0: média da amostra = valor de referencia mu
# p-value <= 0.05 -> h1: média da amostra != valor de referencia mu

t.test(dados$Altura, mu = 167) # mu: média da distribuição


# Observação:
# O teste bicaudal é o default; caso deseje unicaudal, necessário incluir:
# alternative = "greater" ou alternative = "less"
# Exemplo: t.test(dados$Altura, mu = 167, alternative = "greater")
# Nesse caso, o teste verificará se é a média amostral é maior que a média testada


# Passo 5 (opcional): Visualização da distribuição dos dados

boxplot(dados$Altura, ylab = "Altura (cm)")
hist(dados$Altura, xlab = "Altura (cm)")
