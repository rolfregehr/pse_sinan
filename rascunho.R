teste <- read.dbc::read.dbc('./sihsus_sp/pamg2503a.dbc')

teste |> 
  mutate(cid = str_sub(PA_CIDPRI, 1, 1),
         ae = if_else(cid %in% LETTERS[22:25], 'S', 'N')) |> 
  group_by(PA_SEXO, ae, PA_OBITO) |> 
  reframe(n=n()) |> 
  filter(ae == "S")
