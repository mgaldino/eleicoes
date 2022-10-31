
setwd("/home/mgaldino/Downloads")

apuracao <- fread("2turno-apuracao-por-zona-brasil-20221030192310.csv", encoding= "Latin-1") %>%
  clean_names()

glimpse(apuracao)

apuracao_agrupado <- apuracao %>%
  group_by(uf, codigo_do_municipio, nome_do_municipio, zona) %>%
  summarise(voto_bolso = sum(jair_bolsonaro)/sum(votos_validos),
            total_valido = sum(votos_validos)) %>%
  filter(total_valido > 0)

apuracao_agrupado

