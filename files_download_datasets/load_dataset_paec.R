# Pacotes -------------------------------------------------------------------------------------
library(googlesheets4)
library(tidyverse)
library(janitor)

# Baixar planilha do Googlesheets - PAEC dataset ----------------------------------------------
url_paec <- "https://docs.google.com/spreadsheets/d/1ET5IYKerbz8Ecgvf0YFreNdjt-aZosjE10dPqoDubwU/edit?resourcekey=&gid=1969137914#gid=1969137914"

df_paec <- read_sheet(url_paec)

# Converter sheet para um tibble
df_paec <- as_tibble(df_paec)

# Ajustar nome das colunas (janitor)
df_paec <- clean_names(df_paec)

# Verificar df
glimpse(df_paec)

# Gravar planilha
readr::write_rds(df_paec,file = "df_paec.rds")
