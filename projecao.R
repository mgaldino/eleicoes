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
voto_bolso <- runif(6)
dados <- data.frame(n, nm, pvap, voto, municipio, nr_zona )
dados1 <- dados %>%
  inner_join(vote_22_zona_full_proj) %>% # by nr_zona
  mutate(tota)
