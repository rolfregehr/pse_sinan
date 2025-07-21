rm(list = ls())
pacman::p_load(tidyverse, RColorBrewer, ggiraph, gtable, grid)
load('./rda/sinan.rda')

# idade | raca | genero ####

# Auxiliares
fator_raca_cor <- sinan |> group_by(raca_cor) |> reframe(n=n()) |> arrange(-n) |> slice(1:6) |> pull(raca_cor)

sinan |>
  filter(idade >= 0 & idade <= 100,
         year(DT_OCOR) == 2024,
         genero != 'NI') |> 
  mutate(raca_cor = factor(raca_cor, levels = fator_raca_cor)) |> 
  group_by(genero, idade, raca_cor) |> 
  reframe(n=n()) %>% 
  write.csv('csv/tab_idade_raca_genero.csv')




# idade | raca | genero | tipo_viol ####
sinan %>%
  select(DT_OCOR, genero, idade, raca_cor, starts_with("VIOL")) %>% 
  select (-c(VIOL_ESPEC, VIOL_MOTIV)) %>% 
  mutate(VIOL_FISIC = if_else(VIOL_FISIC == 1, 1, 0),
         VIOL_PSICO = if_else(VIOL_PSICO == 1, 1, 0),
         VIOL_TORT = if_else(VIOL_TORT == 1, 1, 0),
         VIOL_SEXU = if_else(VIOL_SEXU == 1, 1, 0),
         VIOL_TRAF = if_else(VIOL_TRAF == 1, 1, 0),
         VIOL_NEGLI = if_else(VIOL_NEGLI == 1, 1, 0),
         VIOL_INFAN = if_else(VIOL_INFAN == 1, 1, 0),
         VIOL_LEGAL = if_else(VIOL_LEGAL == 1, 1, 0),
         VIOL_OUTR = if_else(VIOL_OUTR == 1, 1, 0),
         VIOL_FINAN  = if_else(VIOL_FINAN  == 1, 1, 0)) %>% 
  pivot_longer(5:14, names_to = 'tipo_viol', values_to = 'n',values_drop_na = T) %>% 
  mutate(data = floor_date(DT_OCOR, unit = 'month'),
         tipo_viol = case_when(tipo_viol == 'VIOL_FISIC' ~ 'Física',
                               tipo_viol == 'VIOL_PSICO' ~ 'Psicológica',
                               tipo_viol == 'VIOL_TORT' ~ 'Tortura',
                               tipo_viol == 'VIOL_TRAF' ~ 'Tráfico',
                               tipo_viol == 'VIOL_NEGLI' ~ 'Negligência',
                               tipo_viol == 'VIOL_SEXU' ~ 'Sexual',
                               tipo_viol == 'VIOL_INFAN' ~ 'Trabalho infantil',
                               tipo_viol == 'VIOL_LEGAL' ~ 'Intervenção legal',
                               tipo_viol == 'VIOL_OUTR' ~ 'Outras',
                               T ~ 'Outras')) %>% 
  select(-DT_OCOR, data, everything()) %>% 
  filter(data >= as.Date('2015-01-01') & data <=as.Date('2024-12-31'),
         idade >=0 & idade <=100,
         genero != 'NI') %>% 
  group_by(genero, raca_cor, idade, tipo_viol) %>% 
  reframe(n=sum(n)) %>% 
  write.csv('./csv/genero_raca_cor_idade_tipo_viol.csv') 
