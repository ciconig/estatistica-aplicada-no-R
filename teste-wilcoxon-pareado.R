######################### Teste de Wilcoxon #########################


# Passo 1: Carregar os pacotes que ser�o usados

if(!require(dplyr)) install.packages("dplyr") # Instala��o do pacote caso n�o esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(dplyr)) install.packages("rstatix") # Instala��o do pacote caso n�o esteja instalado
library(rstatix)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diret�rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de c�digo abaixo:

dados <- read.csv('Dados/Banco de Dados 4.csv', sep=";", dec=",", stringsAsFactors = T) %>%
  dplyr::rename(Convulsoes_PT = Convuls�es_PT, Convulsoes_S1 = Convuls�es_S1,
                Convulsoes_S6 = Convuls�es_S6, Genero = G�nero)

View(dados)                                # Visualiza��o dos dados em janela separada
glimpse(dados)                             # Visualiza��o de um resumo dos dados


# Passo 3: Realiza��o do teste de Wilcoxon

wilcox.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = TRUE)


# Observa��o:
# O teste bicaudal � o default; caso deseje unicaudal, necess�rio incluir:
# alternative = "greater" ou alternative = "less"
# Exemplo: wilcox.test(dados$Convulsoes_PT, dados$Convulsoes_S1,
# paired = TRUE, alternative="greater")
# Nesse caso, o teste verificar� se � a mediana das Convulsoes_PT � maior que a
# mediana das Convulsoes_S1


# Passo 4: An�lise descritiva dos dados

dados$dif <- dados$Convulsoes_PT - dados$Convulsoes_S1
View(dados)

dados %>% get_summary_stats(Convulsoes_PT, Convulsoes_S1, dif, type = "median_iqr")

# Dados param�tricos?
# dados %>% group_by(Posicao_Sala) %>% 
#  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "mean_sd")
