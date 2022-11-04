library(rvest)
library(tidyverse)
library(stringr)
library(readr)
library(fs)
library(data.table)
library(janitor)
library(ggplot2)
library(rsample)
library(broom)
library(modeldata)

# baixa os dados
# 
# url <- "https://politica.estadao.com.br/dados/portal/politica/eleicoes/2022/apuracao/segundo-turno/destaque/csv-apuracao.php"
# page <- read_html(url)
# links <- page %>% html_nodes("a") %>% html_attr("href") %>%
#   filter(grepl("zona"))
# 
# links1 <- links[grepl("zona", links)]
#   
# safe_download <- safely(~ download.file(.x , .y, mode = "wb"))
# 
# 
# str_sub(links1[1], start = -18, -5)
# wb_names = str_sub(links1, start = -18)

setwd("/home/mgaldino/Documentos/Pessoal/eleicoes/arquivos_apuracao_parcial")
# map2(links1, wb_names, safe_download)

csv_files <- fs::dir_ls(regexp = "\\.csv$")

csv_files <- csv_files[!grepl("103015", csv_files)]
csv_files <- csv_files[!grepl("103016", csv_files)]
csv_files <- csv_files[!grepl("10301703", csv_files)]
# csv_files <- csv_files[!grepl("1030170824", csv_files)]

csv_files1 <- csv_files[seq(2, length(csv_files), by=2)]

apuracao <- csv_files1 %>% # retira os dez primeiros, que estão vazios
  map_dfr(fread, encoding = "Latin-1", .id = "source") %>%
  clean_names()


#evolução do total
apuracao %>%
  group_by(source) %>%
  summarise(total_apurado = sum(votos_validos))


apuracao_agrupado <- apuracao %>%
  group_by(source, uf, codigo_do_municipio, nome_do_municipio, zona) %>%
  summarise(voto_bolso = sum(jair_bolsonaro)/sum(votos_validos),
            voto_lula = 1 - voto_bolso,
            total_valido = sum(votos_validos)) %>%
  filter(total_valido > 0) %>%
  mutate(hora = str_sub(source, 9, 14),
         time = as.POSIXct(x = source,
                           format = "%Y%m%d%H%M%S"))

## projeta previsão por cada source
setwd("/home/mgaldino/Documentos/Pessoal/eleicoes/")


minha_amostra <- readRDS("minha_amostra.RDS")

# saveRDS(vote_22_zona_full, file="vote_22_zona_full.RDS")

vote_22_zona_full <- readRDS("vote_22_zona_full.RDS")

vote_22_zona_full_proj <- vote_22_zona_full %>%
  mutate(across(paste("v",minha_amostra, sep=""), ~ .*total),) %>% # 6240
  ungroup() %>%
  group_by(nr_zona, sg_uf, nm_municipio, cd_municipio) %>%
  summarise(voto_total = sum(total),
            across(paste("v",minha_amostra, sep=""), sum, .names = "total_{.col}"),
            across(paste("total_v",minha_amostra, sep=""), ~ ./voto_total, .names = "perc_{.col}")) %>%
  select(nr_zona, sg_uf, nm_municipio, cd_municipio, starts_with("perc"))

vote_22_zona_full1 <- vote_22_zona_full %>%
  mutate(across(paste("v",minha_amostra, sep=""), ~ .*total),) %>% # 6240
  ungroup() %>%
  summarise(voto_total = sum(total),
            across(paste("v",minha_amostra, sep=""), sum, .names = "total_{.col}"),
            across(paste("total_v",minha_amostra, sep=""), ~ ./voto_total, .names = "perc_{.col}")) %>%
  select(starts_with("perc"))

vec_bolso <- unlist(vote_22_zona_full1)

## sim antes de sair os dados


# saveRDS(apuracao_hora, file="apuracao_hora.RDS")
apuracao_hora <- readRDS("apuracao_hora.RDS")

# tem que criar função e aplicar map_dbl no grupo de source
# apuracao_hora <- apuracao_agrupado %>%
#   group_by(source, uf, codigo_do_municipio,  zona) %>%
#   summarise(voto_bolsonaro = sum(voto_bolso*total_valido)/sum(total_valido), 
#             voto_lula = sum(voto_lula*total_valido)/sum(total_valido),
#             time = max(time),
#             total_valido = max(total_valido)) 



vote_22_zona_full_proj <- vote_22_zona_full %>%
  mutate(across(paste("v",minha_amostra, sep=""), ~ .*total),) %>% # 6240
  ungroup() %>%
  group_by(nr_zona, sg_uf, nm_municipio, cd_municipio) %>%
  summarise(voto_total = sum(total),
            across(paste("v",minha_amostra, sep=""), sum, .names = "total_{.col}"),
            across(paste("total_v",minha_amostra, sep=""), ~ ./voto_total, .names = "perc_{.col}")) %>%
  select(nr_zona, sg_uf, nm_municipio, cd_municipio, starts_with("perc"))

dados1 <- apuracao_hora %>%
  clean_names() %>%
  inner_join(vote_22_zona_full_proj, by = c("uf" = "sg_uf", "codigo_do_municipio" = "cd_municipio", "zona" = "nr_zona")) %>% # by nr_zona
  ungroup() %>%
  group_by(source) %>%
  summarise(observado = sum(voto_bolsonaro*total_valido),
            observado_total = sum(total_valido),
            perc_observado = observado/observado_total,
            projetado_v4010 = sum(perc_total_v4010*total_valido),
            projetado_v3193  =sum(perc_total_v3193*total_valido),
            projetado_v6023 = sum(perc_total_v6023*total_valido),
            projetado_v8490 = sum(perc_total_v8490*total_valido),
            projetado_v749 = sum(perc_total_v749*total_valido),
            perc_projetado_v4010 = projetado_v4010/observado_total,
            perc_projetado_v3193 = projetado_v3193/observado_total,
            perc_projetado_v6023 = projetado_v6023/observado_total,
            perc_projetado_v8490 = projetado_v8490/observado_total,
            perc_projetado_v749 = projetado_v749/observado_total,
            dif_v4010 = perc_observado - perc_projetado_v4010,
            dif_v3193 = perc_observado - perc_projetado_v3193,
            dif_v6023 = perc_observado - perc_projetado_v6023,
            dif_v8490 = perc_observado - perc_projetado_v8490,
            dif_v749 = perc_observado - perc_projetado_v749,
            total_valido = sum(total_valido)) %>%
  select(source, dif_v4010, dif_v3193, dif_v6023, dif_v8490, dif_v749, total_valido)


glimpse(dados1)


dados1 <- dados1 %>% 
  mutate(previsao_base = median(vec_bolso),
         prev1 = previsao_base + dif_v4010,
         prev2 = previsao_base + dif_v3193,
         prev3 = previsao_base + dif_v6023,
         prev4 = previsao_base + dif_v8490,
         time = as.POSIXct(x = source,
                           format = "%Y%m%d%H%M%S"))
  
dados1 %>%
  ggplot(aes(time, y=prev1)) + geom_line() + geom_point()

dados1 %>%
  ggplot(aes(time, y=prev2)) + geom_line() + geom_point()

dados1 %>%
  ggplot(aes(time, y=prev3)) + geom_line() + geom_point()

dados1 %>%
  ggplot(aes(time, y=prev4)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent, limits = c(.4,.6)) + 
  theme_bw() + ylab("Previsão de voto do Bolsonaro") + xlab("Hora da apuração")

my_plot <- dados1 %>%
  mutate(comparecimento = max(dados1$total_valido),
         comparecmento_perc = total_valido/comparecimento) %>%
  ggplot(aes(comparecmento_perc, y=prev4)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent, limits = c(.48,.51)) + 
  theme_bw() + ylab("Previsão de voto do Bolsonaro") + xlab("Percentual apurado") +
  geom_hline(aes(yintercept = .491), colour="blue")

my_plot

getwd()
  ggsave("evolucao_apuracao.png", my_plot, scale = .5)

# 
# apuracao_hora %>%
#   ggplot(aes(x=time, y = voto_bolsonaro)) + geom_line() + geom_point() +
#   geom_line(aes(y=voto_lula)) + geom_point(aes(y=voto_lula)) +
#   scale_x_datetime(
#     date_breaks = "1 hour",
#     date_labels = "%H:%M"
#   )
# 
# myx <- apuracao_hora$voto_bolsonaro - lag(apuracao_hora$voto_bolsonaro)
# sum(myx < 0, na.rm=T)/length(myx) # 71,6% do tempo foi de perda.
# 
# 
# # 

permutation_apuracao <- apuracao %>%
  group_by(zona, nome_do_municipio, uf) %>%
  summarise(voto_bolsonaro = sum(jair_bolsonaro)/sum(votos_validos),
            voto_lula = sum(lula)/sum(votos_validos)) %>%
  ungroup() %>%
  mutate(bolso_lag = lag(voto_bolsonaro),
         dif = voto_bolsonaro - bolso_lag) %>%
  ungroup() %>%
  mutate(id = 1:n())

hist_bolsonaro <- permutation_apuracao %>%
  ggplot(aes(voto_bolsonaro)) + geom_histogram()

lead_bolso <- permutation_apuracao %>%
  filter(voto_bolsonaro > .4) %>%
  sample_n(.05*n()) %>%
  mutate(id_lead = 1)
  
hist_bolsonaro <- lead_bolso %>%
  ggplot(aes(voto_bolsonaro)) + geom_histogram()

init_permutation_apuracao <- permutation_apuracao %>%
  left_join(select(lead_bolso, id, id_lead), by="id")

init_permutation_apuracao_left <- init_permutation_apuracao %>%
  filter(is.na(id_lead))

init_permutation_apuracao_left %>%
  ggplot(aes(voto_bolsonaro)) + geom_histogram()

sum(init_permutation_apuracao_left$dif > 0, na.rm=T)/length(init_permutation_apuracao_left$dif)

sum(permutation_apuracao$dif > 0, na.rm=T)/length(permutation_apuracao$dif)

myfold <- vfold_cv(permutation_apuracao, v = 2, repeats = 20)

perc_frente <- function(x) {
  dat <- as.data.frame(x)$voto_bolsonaro
  bolso_lag <- lag(dat)
  dif <- dat - bolso_lag
  sum(dif < 0, na.rm=T)/length(dif)
}

myfold <- myfold %>%
  mutate(perc_frent = map_dbl(splits, perc_frente)) %>%
  arrange(-perc_frent)



map_dbl(
  myfold$splits,
  function(x) {
    dat <- as.data.frame(x)$voto_bolsonaro
    bolso_lag <- lag(dat)
    dif <- dat - bolso_lag
    sum(dif < 0, na.rm=T)/length(dif)
  }
)

data(wa_churn, package = "modeldata")

folds1 <- vfold_cv(wa_churn, v = 5)

map_dbl(
  folds1$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)


