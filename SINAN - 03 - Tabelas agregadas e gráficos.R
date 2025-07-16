rm(list = ls())
pacman::p_load(tidyverse, RColorBrewer, ggiraph, gtable, grid)
load('./rda/sinan')


# Auxiliares
fator_raca_cor <- sinan |> group_by(raca_cor) |> reframe(n=n()) |> arrange(-n) |> slice(1:6) |> pull(raca_cor)


# Por sexo e idade da vítima ####

graf_genero_idade <- sinan |>
  filter(idade >= 0 & idade <= 100,
         year(DT_OCOR) == 2024,
         genero != 'NI') |> 
  mutate(raca_cor = factor(raca_cor, levels = fator_raca_cor)) |> 
  group_by(genero, idade, raca_cor) |> 
  reframe(n=n()) |> 
  ggplot(aes(x = idade, y = n, colour = genero))+
  geom_line(linewidth = 1.3)+
  facet_wrap(~raca_cor, ncol = 3) +
  theme_minimal()+
  scale_color_brewer(name = "Gênero", palette = "Set2")+
  labs(title = 'Nº de violências por gênero da vítima',
       x = 'Idade',
       y = 'Vítimas',
       subtitle = 'Ano: 2024')+
  scale_x_continuous(breaks = seq(0, 100, by = 10))


ggsave('graf_genero_idade.svg', plot = graf_genero_idade)

# Tipo de violência por ano ####

tipos <- sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(DT_OCOR,  VIOL_FISIC, VIOL_PSICO,VIOL_TORT, VIOL_SEXU, VIOL_TRAF, VIOL_FINAN, VIOL_NEGLI, VIOL_INFAN, VIOL_LEGAL, VIOL_OUTR) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0),
         data = floor_date(DT_OCOR, unit = 'month')) |> 
  group_by(tipo) |> 
  reframe(n=sum(n)) |> 
  arrange(-n) |> 
  slice(1:6) |> pull(tipo)


graf <- sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(DT_OCOR, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  filter(tipo %in% tipos) |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0),
         data = floor_date(DT_OCOR, unit = 'month')) |> 
  group_by(data, tipo) |> 
  reframe(n= sum(n)) |> 
  mutate(tipo = case_when(tipo == 'VIOL_FISIC' ~ 'Física',
                          tipo == 'VIOL_NEGLI' ~ 'Negligência',
                          tipo == 'VIOL_PSICO' ~ 'Psicológica',
                          tipo == 'VIOL_SEXU' ~ 'Sexual',
                          tipo == 'VIOL_TORT' ~ 'Tortura',
                          T ~ 'Outras'),
         tipo = factor(tipo, levels = c('Física', 'Psicológica', 'Outras', 'Sexual', 'Negligência', 'Tortura')),
         n = replace_na(n, 0)
         ) |> 
  ggplot(mapping = aes(
    x = data,
    y = n,
    color = tipo,
    tooltip = tipo,
    data_id = tipo
  ))+
  geom_line_interactive(hover_nearest = T)+
  theme_classic()+
  scale_color_brewer(name = "Tipo", palette = "Set2")+
  labs(title = 'Violências por tipo',
       x = 'Ano',
       y = 'Vítimas',
       subtitle = '2024: dados preliminares')+
  # Adiciona a caixa de texto
  annotate("text",
           x = as.Date('2015-01-01'),       # Posição x da caixa de texto
           y = 28000,      # Posição y da caixa de texto
           label = "Outras: tortura, financeira\nabuso infantil, policial,\ntráfico, etc.", # O texto
           hjust = 0,   # Alinhamento horizontal (0=esquerda, 0.5=centro, 1=direita)
           vjust = 1,   # Alinhamento vertical (0=baixo, 0.5=centro, 1=topo)
           size = 5,    # Tamanho da fonte
           color = "darkgrey", # Cor do texto
           # Para uma "caixa" visual (com fundo e borda), combine com geom_rect
           # ou use geom_label em vez de geom_text se quiser fundo.
           # Para simular uma caixa com annotate, pode-se usar um rect e depois o texto
           # annotate("rect", xmin = 2.5, xmax = 7.5, ymin = 8.5, ymax = 11,
           #          fill = "lightgreen", alpha = 0.5, color = "black", linetype = "solid") +
           # annotate("text", x = 5, y = 9.75, label = "Minha Caixa de Texto", size = 4)
  )

  

interactive_plot <- girafe(ggobj = graf)     

interactive_plot <- girafe_options(
  interactive_plot,
  opts_hover(css = "stroke:#a69b38; stroke-width: 2px; transition: all 0.3s ease;"),
  opts_hover_inv("opacity:0.5;filter:saturate(10%);"),
  opts_toolbar(saveaspng = FALSE)
)
htmltools::save_html(interactive_plot, "graf_tipo.html")





# Tipo de violência por idade ####

tipos <- sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(DT_OCOR, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0),
         data = floor_date(DT_OCOR, unit = 'month')) |> 
  group_by(tipo) |> 
  reframe(n=sum(n)) |> 
  arrange(-n) |> 
  slice(1:6) |> pull(tipo)


graf_idade <- sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(idade, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  filter(tipo %in% tipos ) |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0)) |> 
  group_by(idade, tipo) |> 
  reframe(n= sum(n)) |> 
  filter(idade>0 & idade <= 100) |> 
  mutate(tipo = case_when(tipo == 'VIOL_FISIC' ~ 'Física',
                          tipo == 'VIOL_NEGLI' ~ 'Negligência',
                          tipo == 'VIOL_PSICO' ~ 'Psicológica',
                          tipo == 'VIOL_SEXU' ~ 'Sexual',
                          tipo == 'VIOL_TORT' ~ 'Tortura',
                          T ~ 'Outras'),
         tipo = factor(tipo, levels = c('Física', 'Psicológica', 'Outras', 'Sexual', 'Negligência', 'Tortura')),
         n = replace_na(n, 0)
  ) |> 
  ggplot(mapping = aes(
    x = idade,
    y = n,
    color = tipo,
    tooltip = tipo,
    data_id = tipo
  ))+
  geom_line_interactive(hover_nearest = T)+
  theme_classic()+
  scale_color_brewer(name = "Tipo", palette = "Set2")+
  labs(title = 'Violências por tipo',
       x = 'Idade',
       y = 'Vítimas',
       subtitle = '2024: dados preliminares')+
  # Adiciona a caixa de texto
  annotate("text",
           x = 40,       # Posição x da caixa de texto
           y = 65000,      # Posição y da caixa de texto
           label = "Outras: tortura, financeira\nabuso infantil, policial,\ntráfico, etc.", # O texto
           hjust = 0,   # Alinhamento horizontal (0=esquerda, 0.5=centro, 1=direita)
           vjust = 1,   # Alinhamento vertical (0=baixo, 0.5=centro, 1=topo)
           size = 5,    # Tamanho da fonte
           color = "darkgrey", # Cor do texto
           # Para uma "caixa" visual (com fundo e borda), combine com geom_rect
           # ou use geom_label em vez de geom_text se quiser fundo.
           # Para simular uma caixa com annotate, pode-se usar um rect e depois o texto
           # annotate("rect", xmin = 2.5, xmax = 7.5, ymin = 8.5, ymax = 11,
           #          fill = "lightgreen", alpha = 0.5, color = "black", linetype = "solid") +
           # annotate("text", x = 5, y = 9.75, label = "Minha Caixa de Texto", size = 4)
  )+
  scale_x_continuous(breaks = seq(0, 100, by = 10))




interactive_plot_idade <- girafe(ggobj = graf_idade)     

interactive_plot_idade <- girafe_options(
  interactive_plot_idade,
  opts_hover(css = "stroke:#a69b38; stroke-width: 2px; transition: all 0.3s ease;"),
  opts_hover_inv("opacity:0.5;filter:saturate(10%);"),
  opts_toolbar(saveaspng = FALSE)
)
htmltools::save_html(interactive_plot_idade, "graf_tipo_idade.html")




# Tipo de violência por idade e gênero ####
## Organiza os dados ####
tipos <- sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(DT_OCOR, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0),
         data = floor_date(DT_OCOR, unit = 'month')) |> 
  group_by(tipo) |> 
  reframe(n=sum(n)) |> 
  arrange(-n) |> 
  slice(1:6) |> pull(tipo)

## Gera o gráfico ####
graf_idade_genero_tipo <-   sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(genero, idade, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  filter(tipo %in% tipos ) |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0)) |> 
  group_by(idade, tipo, genero) |> 
  reframe(n= sum(n)) |> 
  filter(idade>0 & idade <= 100) |> 
  mutate(tipo = case_when(tipo == 'VIOL_FISIC' ~ 'Física',
                          tipo == 'VIOL_NEGLI' ~ 'Negligência',
                          tipo == 'VIOL_PSICO' ~ 'Psicológica',
                          tipo == 'VIOL_SEXU' ~ 'Sexual',
                          tipo == 'VIOL_TORT' ~ 'Tortura',
                          T ~ 'Outras'),
         tipo = factor(tipo, levels = c('Física', 'Psicológica', 'Outras', 'Sexual', 'Negligência', 'Tortura')),
         n = replace_na(n, 0)
  ) |> 
  ggplot(mapping = aes(
    x = idade,
    y = n,
    color = genero
  ))+
  geom_line(linewidth = 1.)+
  facet_wrap(~tipo)+
  theme_minimal()+
  scale_color_brewer(name = "Gênero", palette = "Set2")+
  # Adições para melhorar o gráfico:
  labs(
    title = "Distribuição de Casos de Violência\npor Idade e Tipo (a partir de 2015)",
    x = "Idade (anos)",
    y = "Número de Ocorrências",
    caption = "Fonte: SINAN - Sistema de Informação de Agravos de Notificação"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Centraliza e bold o título
    strip.text = element_text(face = "bold") # Deixa os títulos dos facets em negrito
  )+
  theme(
    # Título principal do gráfico
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # Aumenta para 16
    
    # Títulos dos eixos (X e Y)
    axis.title = element_text(size = 16, face = "bold"), # Aumenta para 14 e negrito
    
    # Texto dos valores nos eixos (os números e rótulos)
    axis.text = element_text(size = 14), # Aumenta para 12
    
    # Títulos dos facets (Física, Negligência, etc.)
    strip.text = element_text(face = "bold", size = 15), # Aumenta para 13 e negrito
    
    # Título da legenda (Gênero)
    legend.title = element_text(size = 14, face = "bold"), # Aumenta para 12 e negrito
    
    # Texto da legenda (Feminino, Masculino)
    legend.text = element_text(size = 12) # Aumenta para 10
  )

## Salva o gráfico ####
ggsave('graf_idade_genero_tipo.svg',
       plot = graf_idade_genero_tipo)



# Tipo de violência por idade e Raça/Cor ####
## Organiza os dados ####
tipos <- sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(DT_OCOR, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0),
         data = floor_date(DT_OCOR, unit = 'month')) |> 
  group_by(tipo) |> 
  reframe(n=sum(n)) |> 
  arrange(-n) |> 
  slice(1:6) |> pull(tipo)

## Gera o gráfico ####

graf_idade_racacor_tipo <-   sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(raca_cor, idade, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  filter(tipo %in% tipos ) |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0)) |> 
  group_by(idade, tipo, raca_cor) |> 
  reframe(n= sum(n)) |> 
  filter(idade>0 & idade <= 100) |> 
  mutate(tipo = case_when(tipo == 'VIOL_FISIC' ~ 'Física',
                          tipo == 'VIOL_NEGLI' ~ 'Negligência',
                          tipo == 'VIOL_PSICO' ~ 'Psicológica',
                          tipo == 'VIOL_SEXU' ~ 'Sexual',
                          tipo == 'VIOL_TORT' ~ 'Tortura',
                          T ~ 'Outras'),
         tipo = factor(tipo, levels = c('Física', 'Psicológica', 'Outras', 'Sexual', 'Negligência', 'Tortura')),
         n = replace_na(n, 0)
  ) |> 
  ggplot(mapping = aes(
    x = idade,
    y = n,
    color = raca_cor
  ))+
  geom_line(linewidth = 1.)+
  facet_wrap(~tipo)+
  theme_minimal()+
  scale_color_brewer(name = "Raça/Cor", palette = "Set2")+
  # Adições para melhorar o gráfico:
  labs(
    title = "Distribuição de Casos de Violência\npor Idade e Tipo (a partir de 2015)",
    x = "Idade (anos)",
    y = "Número de Ocorrências",
    caption = "Fonte: SINAN - Sistema de Informação de Agravos de Notificação"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Centraliza e bold o título
    strip.text = element_text(face = "bold") # Deixa os títulos dos facets em negrito
  )+
  theme(
    # Título principal do gráfico
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # Aumenta para 16
    
    # Títulos dos eixos (X e Y)
    axis.title = element_text(size = 16, face = "bold"), # Aumenta para 14 e negrito
    
    # Texto dos valores nos eixos (os números e rótulos)
    axis.text = element_text(size = 14), # Aumenta para 12
    
    # Títulos dos facets (Física, Negligência, etc.)
    strip.text = element_text(face = "bold", size = 15), # Aumenta para 13 e negrito
    
    # Título da legenda (Gênero)
    legend.title = element_text(size = 14, face = "bold"), # Aumenta para 12 e negrito
    
    # Texto da legenda (Feminino, Masculino)
    legend.text = element_text(size = 12) # Aumenta para 10
  )


  
  
  
## Salva o gráfico ####
ggsave('graf_idade_racacor_tipo.svg',
       plot = graf_idade_racacor_tipo)




# Tipo de violência por Raça/Cor e idade  ####

## Gera o gráfico ####

graf_idade_tipo_racacor <-   sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(raca_cor, idade, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'),
               names_to = 'tipo', 
               values_to = 'n') |> 
  filter(tipo %in% tipos ) |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0)) |> 
  group_by(idade, tipo, raca_cor) |> 
  reframe(n= sum(n)) |> 
  filter(idade>0 & idade <= 100) |> 
  mutate(tipo = case_when(tipo == 'VIOL_FISIC' ~ 'Física',
                          tipo == 'VIOL_NEGLI' ~ 'Negligência',
                          tipo == 'VIOL_PSICO' ~ 'Psicológica',
                          tipo == 'VIOL_SEXU' ~ 'Sexual',
                          tipo == 'VIOL_TORT' ~ 'Tortura',
                          T ~ 'Outras'),
         tipo = factor(tipo, levels = c('Física', 'Psicológica', 'Outras', 'Sexual', 'Negligência', 'Tortura')),
         n = replace_na(n, 0)
  ) |> 
  ggplot(mapping = aes(
    x = idade,
    y = n,
    color = tipo
  ))+
  geom_line(linewidth = 1.)+
  facet_wrap(~raca_cor)+
  theme_minimal()+
  scale_color_brewer(name = "Tipo de violência", palette = "Set2")+
  # Adições para melhorar o gráfico:
  labs(
    title = "Distribuição de Casos de Violência\npor Idade e Tipo (a partir de 2015)",
    x = "Idade (anos)",
    y = "Número de Ocorrências",
    caption = "Fonte: SINAN - Sistema de Informação de Agravos de Notificação"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Centraliza e bold o título
    strip.text = element_text(face = "bold") # Deixa os títulos dos facets em negrito
  )+
  theme(
    # Título principal do gráfico
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # Aumenta para 16
    
    # Títulos dos eixos (X e Y)
    axis.title = element_text(size = 16, face = "bold"), # Aumenta para 14 e negrito
    
    # Texto dos valores nos eixos (os números e rótulos)
    axis.text = element_text(size = 14), # Aumenta para 12
    
    # Títulos dos facets (Física, Negligência, etc.)
    strip.text = element_text(face = "bold", size = 15), # Aumenta para 13 e negrito
    
    # Título da legenda (Gênero)
    legend.title = element_text(size = 14, face = "bold"), # Aumenta para 12 e negrito
    
    # Texto da legenda (Feminino, Masculino)
    legend.text = element_text(size = 12) # Aumenta para 10
  )





## Salva o gráfico ####
ggsave('graf_idade_tipo_racacor.svg',
       plot = graf_idade_tipo_racacor)




# Tabelas fonte ####

pacman::p_load(tidyverse)
load('./rda/sinan')

tipos <- sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(DT_OCOR,  VIOL_FISIC, VIOL_PSICO,VIOL_TORT, VIOL_SEXU, VIOL_TRAF, VIOL_FINAN, VIOL_NEGLI, VIOL_INFAN, VIOL_LEGAL, VIOL_OUTR) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0),
         data = floor_date(DT_OCOR, unit = 'month')) |> 
  group_by(tipo) |> 
  reframe(n=sum(n)) |> 
  arrange(-n) |> 
  slice(1:6) |> pull(tipo)


## Tabela 1 - por tipo de violência ####

tab_01 <- 
sinan |> 
  filter(DT_OCOR >= as.Date('2015-01-01')) |> 
  select(DT_OCOR, starts_with('VIOL')) |> 
  pivot_longer(starts_with('VIOL'), names_to = 'tipo', values_to = 'n') |> 
  filter(tipo %in% tipos) |> 
  mutate(n = case_when(n == 1 ~ 1,
                       T ~ 0),
         data = floor_date(DT_OCOR, unit = 'month')) |> 
  group_by(data, tipo) |> 
  reframe(n= sum(n)) |> 
  mutate(tipo = case_when(tipo == 'VIOL_FISIC' ~ 'Física',
                          tipo == 'VIOL_NEGLI' ~ 'Negligência',
                          tipo == 'VIOL_PSICO' ~ 'Psicológica',
                          tipo == 'VIOL_SEXU' ~ 'Sexual',
                          tipo == 'VIOL_TORT' ~ 'Tortura',
                          T ~ 'Outras'),
         tipo = factor(tipo, levels = c('Física', 'Psicológica', 'Outras', 'Sexual', 'Negligência', 'Tortura')),
         n = replace_na(n, 0)  ) |> 
  pivot_wider(names_from = tipo, values_from = n, values_fill = 0)


write.csv(tab_01, './csv/tab_01.csv')
