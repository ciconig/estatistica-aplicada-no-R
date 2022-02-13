

##### Introdução ao ggplot2 - Parte 1 #####


##### Tutorial bem completo para usar de referência: #####
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/

# Banco de dados traduzido de:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2018/2018-10-23/movie_profit.csv


# Carregamento dos pacotes
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, dplyr)

# Seleção do diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

# Carregamento do banco de dados
dados <- read.csv2("Dados/LucroFilmes.csv", stringsAsFactors = T)

View(dados)

glimpse(dados)

## Modificando DataLancamento para o formato data
dados$DataLancamento <- as.Date(dados$DataLancamento, format = "%m/%d/%Y")

### Salvando o mês e o ano em colunas separadas
dados <- dados %>% mutate(AnoLancamento = format(DataLancamento, "%Y"),
                          MesLancamento = format(DataLancamento, "%m"))



# Entendendo a lógica das camadas do ggplot2

## As três principais camadas: dados, estética e geom

ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_point()


## O aes pode ser definido na camada ggplot ou na geom_
ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial))


ggplot() +
  geom_point(data = dados, aes(x = LucroLocal, y = LucroMundial))



## Possibilidades de camadas de geom_: https://ggplot2.tidyverse.org/reference/

### Histograma (Orçamentos de produção)
ggplot(data = dados) +
  geom_histogram(aes(x = Orcamento))


### Gráfico de barras (Quantidade de filmes por gênero)
ggplot(data = dados) +
  geom_bar(aes(x = Genero, y = ..count..)) 


### Boxplot (Lucro mundial por gênero)
ggplot(data = dados) +
  geom_boxplot(aes(x = Genero, y = LucroMundial))


### Linhas (Quantidade de filmes por ano)
ggplot(data = dados) +
  geom_line(aes(x = AnoLancamento, group = 1), stat = "count")



## Modificando argumentos dentro do geom (color, shape, size)
### E a diferença entre usá-los dentro ou fora do aes

ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial, color = Genero))


ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial, color = Genero,
                 shape = Genero))


ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial, shape = Genero),
             color = "darkred")
# Cores pré-definidas no R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf



ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial, shape = Genero),
             color = "#61988E")
# Site gerador de paletas: https://coolors.co/



ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             color = "#61988E", shape = 18)
# Shapes possíveis: http://www.sthda.com/english/wiki/ggplot2-point-shapes


ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             color = "#61988E", shape = 18, size = 1)



ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             fill = "#61988E", color = "black", shape = 25, size = 1.5)
# Shape que permite color e fill


## Adicionando outro geom
### geom_line x geom_smooth

ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             color = "#61988E", shape = 16, size = 0.7) +
  geom_line(aes(x = LucroLocal, y = LucroMundial), stat = "smooth",
            method = "lm")



ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(aes(x = LucroLocal, y = LucroMundial), method = "lm", se = F,
              color = "black", size = 0.5)



# Deixando o código mais enxuto
ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5)



## Modificando a ordem das camadas (a ordem dos geoms importa!)
ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5) +
  geom_point(color = "#61988E", shape = 16, size = 0.7)



## Especificando aes específicas para um dos geoms
ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5,
              aes(linetype = Genero))



## Usando o filtro (dplyr) para selecionar dados para o gráfico
dados %>% filter(Genero == "Terror") %>% 
  ggplot(aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5,
              aes(linetype = Genero))


dados %>% filter(Orcamento <= 9000000 & Classificacao == "PG") %>% 
  ggplot(aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5,
              aes(linetype = Genero))



## Usando o geom para representar um "summary"
### (stat = summary) x stat_summary()


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean")

  
ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  stat_summary(geom = "point", fun = "mean")



ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  stat_summary(geom = "point", fun = "median")



## Incluindo barras de erros (usando também o summary)


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se")


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.min = "min", fun.max = "max")


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3)


## Usando IC 95% ao invés de erro-padrão (pacote ggpubr)

pacman::p_load(ggpubr)

ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3)


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.3)


## Incluindo a classificação

dados %>% filter(Classificacao %in% c("PG", "PG-13", "R")) %>% 
  ggplot(aes(x = Genero, y = LucroLocal, color = Classificacao)) +
  geom_point(stat = "summary", fun = "mean", position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3,
                position = position_dodge(0.4))


## Renomeando os eixos e legenda

dados %>% filter(Classificacao %in% c("PG", "PG-13", "R")) %>% 
  ggplot(aes(x = Genero, y = LucroLocal, color = Classificacao)) +
  geom_point(stat = "summary", fun = "mean", position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3,
                position = position_dodge(0.4)) +
  labs(y = "Lucro local (US$)", x = "Gênero do filme", color = "Classificação")



## Adicionando título, subtítulo e legenda


dados %>% filter(Classificacao %in% c("PG", "PG-13", "R")) %>% 
  ggplot(aes(x = Genero, y = LucroLocal, color = Classificacao)) +
  geom_point(stat = "summary", fun = "mean", position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3,
                position = position_dodge(0.4)) +
  labs(y = "Lucro local (US$)", x = "Gênero do filme", color = "Classificação",
       title = "Lucro local em US$, de acordo com o gênero e classificação do filme",
       subtitle = "Dados representados como média e IC 95%",
       caption = "Fonte: FiveThirtyEight")

dados %>% 
  filter(LucroMundial <= mean(LucroMundial) + 1.5*IQR(LucroMundial) &
           LucroMundial >= mean(LucroMundial) - 1.5*IQR(LucroMundial) & 
           Classificacao %in% c("PG", 'PG-13', 'R')) %>% 
  ggplot(aes(x = Genero, y = LucroMundial)) + 
  geom_boxplot() + 
  facet_wrap(~ Classificacao)

dados %>% 
  filter(LucroMundial <= mean(LucroMundial) + 1.5*IQR(LucroMundial) &
           LucroMundial >= mean(LucroMundial) - 1.5*IQR(LucroMundial) & 
           Classificacao %in% c("PG", 'PG-13', 'R')) %>% 
  ggplot(aes(x = Genero, y = LucroMundial, fill = Classificacao)) + 
  geom_boxplot(size = 0.8, width = 0.8) +
  labs(x = 'Gênero', y = 'Lucro Mundial em U$', fill = 'Classificação')



##### Introdução ao ggplot2 - Parte 2 #####


gm <- dados %>% filter(Classificacao %in% c("PG", "PG-13", "R")) %>%
  ggplot(aes(x = Genero, y = LucroLocal, color = Classificacao)) +
  geom_point(stat = "summary", fun = "mean", position = position_dodge(.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3,
                position = position_dodge(.4)) +
  labs(y = "Lucro Local (US$)", x = "Gênero", color = "Classificação")


gm


## Modificando os limites dos eixos
### coord_cartesian x scale_y_continuous

gm + coord_cartesian(ylim = c(0, 100000000))

gm + scale_y_continuous(limits = c(0, 100000000))



## Modificando a expansão

gb <- ggplot(data = dados) +
  geom_bar(aes(x = Genero, y = ..count.., fill = Genero), show.legend = F) +
  labs(y = "Quantidade de filmes", x = "Gênero")


gb + scale_y_continuous(expand = expansion(add = c(0,100)))

gb + scale_y_continuous(expand = expansion(mult = c(0,0.05)))



## Modificando a quantidade de dígitos e os separadores (milhar e decimal)


gb + scale_y_continuous(expand = expansion(add = c(0,100)),
                        labels = scales::number_format(accuracy = 0.1,
                                                       decimal.mark = ",",
                                                       big.mark = "."))



## Usando porcentagens nos eixos

gbp <- ggplot(data = dados) +
  geom_bar(aes(x = Genero, y = (..count..)/sum(..count..),
               fill = Genero), show.legend = F) +
  labs(y = "Quantidade de filmes", x = "Gênero")


gbp <- gbp + scale_y_continuous(expand = expansion(mult = c(0,0.05)),
                                labels = scales::percent_format(accuracy = 0.1,
                                                                decimal.mark = ","))



## Modificando os rótulos do eixo x (categórico)

gbp + scale_x_discrete(labels = c("Ação", "Aventura", "Comédia",
                                  "Drama", "Terror"))



## Modificando as ordens das categorias do eixo x (categórico)

gbp + scale_x_discrete(limits = c("Drama", "Comedia", "Acao",
                                  "Aventura", "Terror"),
                       labels = c("Drama", "Comédia", "Ação",
                                  "Aventura", "Terror"))


## Outra opção: usando o pacote forcats
pacman::p_load(forcats)

ggplot(data = dados) +
  geom_bar(aes(x = fct_infreq(Genero), y = (..count..)/sum(..count..),
               fill = Genero), show.legend = F) +
  labs(y = "Quantidade de filmes", x = "Gênero") +
  scale_y_continuous(expand = expansion(mult = c(0,0.05)),
                     labels = scales::percent_format(accuracy = 0.1,
                                                     decimal.mark = ","))


## Modificando o intervalo entre os valores do eixo (variável numérica)

gl <- ggplot(data = dados) +
  geom_line(aes(x = as.numeric(AnoLancamento), group = 1), stat = "count") +
  labs(y = "Quantidade de filmes", x = "Ano de lançamento")


gl + scale_x_continuous(n.breaks = 10)


gl + scale_x_continuous(breaks = seq(1935, 2020, by = 5))


gl + scale_x_continuous(breaks = seq(min(dados$AnoLancamento),
                                     max(dados$AnoLancamento),
                                     by = 5))


## Formatando a unidade para moeda

gs <- ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 1) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5) +
  labs(y = "Lucro Mundial (US$)", x = "Lucro Local (US$)")


gs + scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                       decimal.mark = ",")) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ","))


gs + scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                       decimal.mark = ",",
                                                       prefix = "US$ ")) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "US$ "))


gs + scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::dollar_format())




## Formatando a unidade para "em milhões"


gs + scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                       decimal.mark = ",",
                                                       prefix = "US$ ",
                                                       scale = 1e-6,
                                                       suffix = " M")) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "US$ ",
                                                    scale = 1e-6,
                                                    suffix = " M"))



gs <- gs + scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                             decimal.mark = ",",
                                                             prefix = "US$ ",
                                                             scale = 1e-6)) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "US$ ",
                                                    scale = 1e-6)) +
  labs(y = "Lucro mundial em milhões", x = "Lucro local em milhões")




## Facet wrap x facet grid


### Wrap
gs + facet_wrap(~ Genero)


### Modificando a quantidade de colunas ou linhas
gs + facet_wrap(~ Genero, ncol = 2)


### Escala fixa ou livre (fixed, free, free_y, free_x)
gs + facet_wrap(~ Genero, ncol = 2, scales = "free")


### Grid
gs + facet_grid(Classificacao ~ Genero)


### Escala fixa ou livre (fixed, free, free_y, free_x)
gs + facet_grid(Classificacao ~ Genero, scales = "free")