######################### Carregando os dados ##################################


# Carregar os pacotes que ser�o usados

if(!require(dplyr)) install.packages("dplyr") 
library(dplyr)                                


# Carregar o banco de dados

# Importante: selecionar o diret�rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de c�digo abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Dados/Banco de Dados 2.csv', 
                  sep = ';', 
                  dec = ',',
                  stringsAsFactors = T) # Carregamento do arquivo csv
View(dados)                             # Visualiza��o dos dados em janela separada
glimpse(dados)                          # Visualiza��o de um resumo dos dados

######################### Teste de normalidade de Shapiro ######################

# Como interpretar o teste:

# p-value > 0.05 -> h0: distribui��o dos dados = distribui��o normal
# p-value <= 0.05 -> h1: distribui��o dos dados != distribui��o normal

# Verifica��o da normalidade dos dados

shapiro.test(dados$Altura)
shapiro.test(dados$Idade)


######################### Teste de t para uma amostra ##########################

# Realiza��o do teste t para uma amostra
# interpreta��o do teste t para uma amostra:

# p-value > 0.05 -> h0: m�dia da amostra = valor de referencia mu
# p-value <= 0.05 -> h1: m�dia da amostra != valor de referencia mu

t.test(dados$Altura, mu = 167) # mu: m�dia da distribui��o


# Observa��o:
# O teste bicaudal � o default; caso deseje unicaudal, necess�rio incluir:
# alternative = "greater" ou alternative = "less"
# Exemplo: t.test(dados$Altura, mu = 167, alternative = "greater")
# Nesse caso, o teste verificar� se � a m�dia amostral � maior que a m�dia testada


# Passo 5 (opcional): Visualiza��o da distribui��o dos dados

boxplot(dados$Altura, ylab = "Altura (cm)")
hist(dados$Altura, xlab = "Altura (cm)")
