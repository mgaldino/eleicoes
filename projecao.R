# faz projeção

# Save an object to a file
# saveRDS(object, file = "my_MODEL.rds")

# Carrega coeficientes ajustado
readRDS(file = "my_MODEL.rds")

# dados

# calcula quanto deveria ter por seção

vote_22_zona_full_proj <- vote_22_zona_full %>%
  mutate(across(paste("v",minha_amostra, sep=""), ~ .*total),) %>% # 6240
  ungroup() %>%
  group_by(nr_zona, sg_uf, nm_municipio, cd_municipio) %>%
  summarise(voto_total = sum(total),
            across(paste("v",minha_amostra, sep=""), sum, .names = "total_{.col}"),
            across(paste("total_v",minha_amostra, sep=""), ~ ./voto_total, .names = "perc_{.col}")) %>%
  select(nr_zona, sg_uf, nm_municipio, cd_municipio, starts_with("perc"))

## sim antes de sair os dados

dados1 <- apuracao_agrupado %>%
  clean_names() %>%
  inner_join(vote_22_zona_full_proj, by = c("uf" = "sg_uf", "codigo_do_municipio" = "cd_municipio", "zona" = "nr_zona")) %>% # by nr_zona
  ungroup() %>%
  summarise(observado = sum(voto_bolso*total_valido),
            observado_total = sum(total_valido),
            perc_observado = observado/observado_total,
            projetado_v7052 = sum(perc_total_v7052*total_valido),
            projetado_v8284  =sum(perc_total_v8284*total_valido),
            projetado_v5954 = sum(perc_total_v5954*total_valido),
            projetado_v5461 = sum(perc_total_v5461*total_valido),
            perc_projetado_v705 = projetado_v7052/observado_total,
            perc_projetado_v8284 = projetado_v8284/observado_total,
            perc_projetado_v5954 = projetado_v5954/observado_total,
            perc_projetado_v5461 = projetado_v5461/observado_total,
            dif_v7052 = perc_observado - perc_projetado_v705,
            dif_v8284 = perc_observado - perc_projetado_v8284,
            dif_v5954 = perc_observado - perc_projetado_v5954,
            dif_v5461 = perc_observado - perc_projetado_v5461) %>%
  select(dif_v7052, dif_v8284, dif_v5954, dif_v5461)
            

dados1
uf <- apuracao_agrupado %>%
  clean_names() %>%
  inner_join(vote_22_zona_full_proj, by = c("uf" = "sg_uf", "codigo_do_municipio" = "cd_municipio", "zona" = "nr_zona")) %>% # by nr_zona
  ungroup() %>%
  group_by(uf) %>%
  summarise(observado = sum(voto_bolso*total_valido),
            observado_total = sum(total_valido),
            perc_observado = observado/observado_total,
            projetado_v7052 = sum(perc_total_v7052*total_valido),
            projetado_v8284  =sum(perc_total_v8284*total_valido),
            projetado_v5954 = sum(perc_total_v5954*total_valido),
            projetado_v5461 = sum(perc_total_v5461*total_valido),
            perc_projetado_v705 = projetado_v7052/observado_total,
            perc_projetado_v8284 = projetado_v8284/observado_total,
            perc_projetado_v5954 = projetado_v5954/observado_total,
            perc_projetado_v5461 = projetado_v5461/observado_total,
            dif_v7052 = perc_observado - perc_projetado_v705,
            dif_v8284 = perc_observado - perc_projetado_v8284,
            dif_v5954 = perc_observado - perc_projetado_v5954, 
            dif_v5461 = perc_observado - perc_projetado_v5461)%>%
  select(uf, dif_v7052, dif_v8284, dif_v5954)
