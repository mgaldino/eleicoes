library(data.table)
library(tidyverse)
library(rstanarm)
library(janitor)
library(ggplot2)
library(bayesplot)

setwd("/home/mgaldino/Downloads/")
library(here)
options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)

setwd(paste(here(),"/votacao_candidato_munzona_2022", sep=""))

# importando votação de 2022 para presidente

vote_2022 <- fread("votacao_candidato_munzona_2022_BR.csv",  encoding="Latin-1") %>%
  janitor::clean_names() %>%
  dplyr::filter(ds_cargo == "Presidente")


# Arrumando os dados
vote_22_zona <- vote_2022 %>%
  group_by(sg_uf, nm_municipio, cd_municipio, nr_zona) %>%
  summarise(total = sum(qt_votos_nominais),
            voto_bolsonaro = sum(ifelse(grepl("BOLSONARO", nm_candidato), qt_votos_nominais, 0)),
            voto_pt = sum(ifelse(grepl("LULA", nm_candidato), qt_votos_nominais, 0))) %>%
  group_by(sg_uf, nm_municipio, cd_municipio, nr_zona) %>%
  mutate(valido_1t = voto_bolsonaro/total,
         valido1t_pt = voto_pt/total) %>%
  ungroup() %>%
  mutate(sg_uf = as.factor(sg_uf))

voto_zona_22 <- vote_22_zona %>%
  pivot_wider(id_cols = c(sg_uf, cd_municipio, nm_municipio, nr_zona), names_from=nr_turno, values_from = c(voto_bolsonaro, voto_pt, total) ) %>%
  mutate(valido_1t = voto_bolsonaro_1/total_1,
         valido_2t = voto_bolsonaro_2/total_2 - .0001,
         valido1t_pt = voto_pt_1/total_1,
         )

# importando dado de 2018

setwd(paste(here(),"/votacao_candidato_munzona_2018", sep=""))
vote_2018 <- fread("votacao_candidato_munzona_2018_BRASIL.csv",  encoding="Latin-1") %>%
  clean_names() %>%
  dplyr::filter(ds_cargo == "Presidente")

gc()

# arrumando os dados
voto_bolso_haddad <- vote_2018 %>%
  group_by(nr_turno, sg_uf, nm_municipio, cd_municipio, nr_zona) %>%
  summarise(total = sum(qt_votos_nominais),
            voto_bolsonaro = sum(ifelse(grepl("BOLSONARO", nm_candidato), qt_votos_nominais, 0)),
            voto_pt = sum(ifelse(grepl("HADDAD", nm_candidato), qt_votos_nominais, 0)))


voto_zona_18 <- voto_bolso_haddad %>%
  pivot_wider(id_cols = c(sg_uf, cd_municipio, nm_municipio, nr_zona), names_from=nr_turno, values_from = c(voto_bolsonaro, voto_pt, total) ) %>%
  mutate(valido_1t = voto_bolsonaro_1/total_1,
         valido_2t = voto_bolsonaro_2/total_2 - .0001,
         valido1t_pt = voto_pt_1/total_1,
         sg_uf = as.factor(sg_uf))

## Estimando modelos.
#modelo simples
fit1 <- stan_betareg(valido_2t ~ valido_1t+ valido1t_pt+ sg_uf, data = voto_zona_18, link = "logit",
                     seed = 12345, iter = 5000)
round(coef(fit1), 2)

# modelo mais complexo
fit2 <- stan_betareg(valido_2t ~ valido_1t + sg_uf, data = voto_zona_18, link = "logit",
                     seed = 12345, iter = 5000)
round(coef(fit2), 2)
# priors
prior_summary(fit1)
prior_summary(fit2)

fit3 <- stan_lmer(valido_2t ~ valido_1t + valido1t_pt + (1| sg_uf), data = voto_zona_18, seed = 12345, iter = 5000)
pp_check(fit3)

bayesplot_grid(
  pp_check(fit1), pp_check(fit2), pp_check(fit3),
  xlim = c(0,1),
  ylim = c(0,4),
  grid_args = list(ncol = 3)
)

pp_check(fit1)
pp_check(fit2)

rm(vote_2018)
vote_22_zona_limpo <- vote_22_zona %>%
  select(valido_1t, valido1t_pt, sg_uf, total) %>%
  filter(!is.na(valido_1t))

newdata <- vote_22_zona_limpo %>%
  select(-total)

y_rep <- as_tibble(t(posterior_predict(fit3, newdata))) %>%
  clean_names()

y_rep <- y_rep %>%
  clean_names()
n <- 1000
minha_amostra <- sample(1:length(y_rep), n)

vote_22_zona_full <- bind_cols(vote_22_zona_limpo, y_rep[,minha_amostra])

vote_22_zona_full1 <- vote_22_zona_full %>%
  mutate(across(paste("v",minha_amostra, sep=""), ~ .*total),) %>% # 6240
  ungroup() %>%
  summarise(voto_total = sum(total),
            across(paste("v",minha_amostra, sep=""), sum, .names = "total_{.col}"),
            across(paste("total_v",minha_amostra, sep=""), ~ ./voto_total, .names = "perc_{.col}")) %>%
  select(starts_with("perc"))

vec_bolso <- unlist(vote_22_zona_full1)
round(quantile(vec_bolso, c(.025, .975)), 3)

## comparando modelos
loo1 <- loo(fit1)
loo2 <- loo(fit2)
loo_compare(loo1, loo2)