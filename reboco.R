library(ggplot2)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
municipios <- read.csv("C:/Users/Gustavo/Downloads/municipios_virgula.csv")
View(municipios)
str(municipios)

mnicipios2 <- read.csv("C:/Users/Gustavo/Downloads/municipios_pontovirgula.csv")

municipos2 <- read.csv2("C:/Users/Gustavo/Downloads/municipios_pontovirgula.csv",
                        fileEncoding = "latin1")

eleicoes22 <- read.csv("C:/Users/Gustavo/Downloads/eleicoes_2022.csv")
filter(eleicoes22, municipio == "Pau D'Arco")

IDH_municipios <- read.csv2("C:/Users/Gustavo/Downloads/idh_municipios.csv",
                            fileEncoding = "latin1")
str(IDH_municipios)

municipios %>%
  ggplot(aes(x = idh, y = reorder(nome_municipio, idh))) + geom_col() +
  xlab("IDH") + ylab("Município") +
  theme_light() + theme(text=element_text(size = 20)) +
  ggtitle("IDH dos municípios em ordem\ndo maior para o menor")

df <- municipios %>%
  summarise('populacao total' = sum(populacao),
            'populacao media' = mean(populacao))

kable(df)

df <- eleicoes22 %>%
  group_by(candidato) %>%
  summarise(soma_candidatos = sum(votos)) %>%
  arrange(desc(soma_candidatos))

kable(df)

df <- IDH_municipios %>%
  group_by(regiao) %>%
  summarise(soma_idh = mean(idh_2010)) %>%
  arrange(desc(soma_idh))
kable(df)

df <- eleicoes22 %>%
  group_by(municipio) %>%
  slice_max(order_by = votos) %>%
  ungroup() %>%
  mutate(vencedor = candidato)
kable(df)

municipios %>%
  ggplot(aes(x = reorder(nome_municipio, populacao), y = populacao)) + geom_col() +
  xlab("Município") + ylab("População") + coord_flip() +
  theme_light() + theme(text = element_text(size = 20)) +
  ggtitle("População dos municípios em ordem\ndo menor para o maior") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1))

IDH_municipios %>%
  ggplot(aes(x = idh_educacao, y = idh_renda, color = regiao)) + geom_point() +
  xlab("IDH na Educação") + ylab("IDH na Renda") +
  theme_light() + theme(text=element_text(size = 20)) +
  ggtitle("IDH na educação e na renda por municípios")
