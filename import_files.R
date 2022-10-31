# caregando bibliotecas

library(googlesheets4)
library(googledrive)
library(httpuv)
library(tidyverse)
library(purrr)
library(data.table)
library(janitor)
library(stringr)


## store the URL you have
folder_url <- "https://drive.google.com/drive/folders/1RlSRfMqk2tPiSPDqhad5MmNWR9DLmm8T"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
folder <- drive_get(as_id(folder_url))

## identify the csv files in that folder
csv_files <- drive_ls(folder, type = "csv")

# download the files
walk(csv_files$id, ~ drive_download(as_id(.x)))

setwd( "/home/mgaldino/Downloads")
# 
lista_arquivos <- list.files()
n <- length(lista_arquivos)
lista_df <- list()

for(i in 1:n){
  df <- fread(lista_arquivos[i], encoding = "Latin-1")
  
  df <- df %>%
    mutate(id = i,
           hora <- substr(lista_arquivos[i], 26, 39)) %>%
  clean_names() %>%
    filter(secoes_totalizadas > 0)
  
  lista_df[[i]] <- df
  print(i)
}
df_voto1t <- lista_df %>% bind_rows()

glimpse(df_voto1t)

df_voto1t %>%
  summarise(total_lula = sum(lula),
            total_bolsonaro = sum(jair_bolsonaro),
            total_validos = sum(votos_validos),
            perc_lula = round(total_lula/total_validos, 4),
            perc_bolso =round(total_bolsonaro/total_validos, 4))
