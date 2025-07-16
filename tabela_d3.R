temp <- sinan |> group_by(idade, genero) |> 
  reframe(n=n()) |> 
  filter(idade >= 0,
       idade <= 100) |> 
  pivot_wider(names_from = genero, values_from = n, values_fill = 0) |> 
  mutate(feminino = Fem, masculino = Masc) |> 
  select(idade, feminino, masculino)

write.csv(temp, 'dados_populacao.csv')
