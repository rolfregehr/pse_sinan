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
  mutate(data = floor_date(DT_OCOR, unit = 'month')) %>% 
  select(-DT_OCOR, data, everything()) %>% 
  filter(data >= as.Date('2015-01-01') & data <=as.Date('2024-12-31')) %>% 
  group_by(genero, raca_cor, idade, tipo_viol) %>% 
  reframe(n=sum(n)) %>% 
  write.csv('./csv/genero_raca_cor_idade_tipo_viol.csv')
  
