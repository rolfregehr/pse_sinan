rm(list = ls())
pacman::p_load(read.dbc, tidyverse)
arq_sinan_dbc <- list.files('./dados_sinan/', full.names = T)

colunas_selecionadas <- c('ID_MUNICIP',
  'ID_MN_RESI',
  'DT_OCOR',
  'NU_IDADE_N',
  'CS_SEXO',
  'CS_RACA',
  'VIOL_FISIC',
  'VIOL_PSICO',
  'VIOL_TORT',
  'VIOL_SEXU',
  'VIOL_TRAF',
  'VIOL_FINAN',
  'VIOL_NEGLI',
  'VIOL_INFAN',
  'VIOL_LEGAL',
  'VIOL_OUTR',
  'VIOL_ESPEC',
  'REL_PAI',
  'REL_MAE',
  'REL_PAD',
  'REL_CONJ',
  'REL_EXCON',
  'REL_NAMO',
  'REL_EXNAM',
  'REL_FILHO',
  'REL_DESCO',
  'REL_IRMAO',
  'REL_CONHEC',
  'REL_CUIDA',
  'REL_PATRAO',
  'REL_INST',
  'REL_POL',
  'REL_PROPRI',
  'REL_OUTROS',
  'REL_ESPEC',
  'AUTOR_SEXO',
  'AUTOR_ALCO',
  'ORIENT_SEX',
  'IDENT_GEN',
  'VIOL_MOTIV',
  'CICL_VID')

sinan_temp <- read.dbc(arq_sinan_dbc[1]) |> 
  select(all_of(colunas_selecionadas))

for(arq in arq_sinan_dbc[2:length(arq_sinan_dbc)]){
  print(arq)
  temp <- read.dbc(arq)|> 
    select(all_of(colunas_selecionadas))
  sinan_temp <- bind_rows(sinan_temp, temp)
}


sinan <- sinan_temp |> 
  mutate(genero = case_when(CS_SEXO == 'F' ~ 'Fem',
                            CS_SEXO == 'M' ~ 'Masc',
                            T ~ 'NI'),
         idade = case_when(str_sub(NU_IDADE_N, 1, 1) != 4 ~ NA,
                           str_sub(NU_IDADE_N, 1, 1) == 4 ~ NU_IDADE_N - 4000,
                           T ~ NA),
         raca_cor = case_when(CS_RACA == 1 ~ 'Branca',
                              CS_RACA == 2 ~ 'Preta',
                              CS_RACA == 3 ~ 'Amarela',
                              CS_RACA == 4 ~ 'Parda',
                              CS_RACA == 5 ~ 'Indígena',
                              T ~ 'Ignorado'),
         raca_cor = factor(raca_cor, levels = c('Parda', 'Branca', 'Ignorado', 'Preta', 'Indígena', 'Amarela')))

save(sinan, file = './rda/sinan')


