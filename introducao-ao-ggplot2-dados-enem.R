##### Introdução ao ggplot2 - Parte 1 #####


##### Tutorial bem completo para usar de referência: #####
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/

# Banco de dados traduzido de:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2018/2018-10-23/movie_profit.csv


# Carregamento dos pacotes
setwd("~/04 - Projetos/01 - R/06 - ggplot2")

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, dplyr, data.table, psych, ggcorrplot)

# Seleção do diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

# Carregamento do banco de dados
dados = fread("Dados/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv", sep = ';', stringsAsFactors = T, nrows = 200000)

dados = data.frame(dados)

dados = dados %>% 
  select(NU_INSCRICAO,CO_UF_RESIDENCIA, SG_UF_RESIDENCIA,
         NU_IDADE, TP_SEXO, TP_COR_RACA, TP_ESTADO_CIVIL, 
         TP_ESCOLA, TP_ENSINO, TP_ANO_CONCLUIU,TP_ST_CONCLUSAO,
         NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, 
         NU_NOTA_REDACAO, Q001, Q002, 
         Q006, Q024, Q025
         )

dados$TP_COR_RACA = factor(dados$TP_COR_RACA, 
                           levels = c(0,1,2,3,4), 
                           labels =  c('Não declarado', 'Branca', 'Preta', 'Parda', 'Amarela')
)

dados$Q001 = factor(dados$Q001, 
                    levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'), 
                    labels = c('Nunca estudou.',
                               'Não completou a 4ª série/5º ano do Ensino Fundamental.', 
                               'Completou a 4ª série/5º ano, mas não completou a 8ª série/9º ano do Ensino Fundamental.',
                               'Completou a 8ª série/9º ano do Ensino Fundamental, mas não completou o Ensino Médio.',
                               'Completou o Ensino Médio, mas não completou a Faculdade.',
                               'Completou a Faculdade, mas não completou a Pós-graduação.',
                               'Completou a Pós-graduação.',
                               'Não sei.')
                    )

dados$Q002 = factor(dados$Q002, 
                    levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'
                               ), 
                    labels = c('Nunca estudou.',
                               'Não completou a 4ª série/5º ano do Ensino Fundamental.', 
                               'Completou a 4ª série/5º ano, mas não completou a 8ª série/9º ano do Ensino Fundamental.',
                               'Completou a 8ª série/9º ano do Ensino Fundamental, mas não completou o Ensino Médio.',
                               'Completou o Ensino Médio, mas não completou a Faculdade.',
                               'Completou a Faculdade, mas não completou a Pós-graduação.',
                               'Completou a Pós-graduação.',
                               'Não sei.')
                    )

View(dados)
glimpse(dados)


## 


dados %>% 
  ggplot(aes(x = NU_NOTA_MT, color = TP_SEXO, fill = TP_SEXO)) + 
  geom_histogram() + 
  facet_wrap(~ TP_SEXO)

dados %>% 
  ggplot(aes(x = NU_IDADE, y = NU_NOTA_MT, color = TP_SEXO, fill = TP_SEXO)) + 
  geom_point() + 
  geom_smooth(size = 0.8, width = 0.8, color = 'grey', fill = 'light grey') + 
  facet_wrap(~ TP_SEXO)

dados %>% 
  ggplot(aes(x = TP_SEXO, y = NU_NOTA_MT, fill = TP_SEXO)) +
  geom_boxplot() + 
  facet_grid(~ TP_COR_RACA)

dados %>% 
  ggplot(aes(x = TP_SEXO, y = NU_NOTA_MT, fill = TP_SEXO)) +
  geom_boxplot() + 
  facet_grid(TP_SEXO ~ TP_COR_RACA)

dados %>% 
  ggplot(aes(x = NU_NOTA_MT, color = TP_SEXO, fill = TP_SEXO)) + 
  geom_histogram() + 
  facet_grid(TP_COR_RACA ~ TP_SEXO)

table(dados$TP_SEXO)

prop.table(table(dados$TP_SEXO))

pivot = dados %>% 
  select(NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, 
         NU_NOTA_REDACAO, Q001) %>%  
  group_by(Q001) %>% 
  summarise(N = length(Q001), 
            media_ch = mean(NU_NOTA_CH, na.rm = T), 
            media_cn = mean(NU_NOTA_CN, na.rm = T), 
            media_lc = mean(NU_NOTA_LC, na.rm = T),
            media_mt = mean(NU_NOTA_MT, na.rm = T),
            media_redacao = mean(NU_NOTA_REDACAO, na.rm = T))

pivot = dados %>% 
  select(NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, 
         NU_NOTA_REDACAO, Q001) %>%  
  group_by(Q001) %>% 
  summarise(N = length(Q001), 
            media_ch = max(NU_NOTA_CH, na.rm = T), 
            media_cn = max(NU_NOTA_CN, na.rm = T), 
            media_lc = max(NU_NOTA_LC, na.rm = T),
            media_mt = max(NU_NOTA_MT, na.rm = T),
            media_redacao = max(NU_NOTA_REDACAO, na.rm = T))

dados %>% 
  ggplot(aes(x = NU_NOTA_MT, color = Q001, fill = Q001)) + 
  geom_histogram() + 
  facet_grid(~ Q001)
  

dados %>% 
  ggplot(aes(x = NU_NOTA_MT, y = Q001)) + 
  geom_point(stat = "summary", fun = "median", position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", width = 0.1,
                fun.min = "min", fun.max = "max",
                position = position_dodge(0.1))







