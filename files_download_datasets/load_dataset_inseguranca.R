# Pacotes -------------------------------------------------------------------------------------
library(googlesheets4)
library(tidyverse)
library(janitor)

# Baixar planilha do Googlesheets - Insegurança alimentar -------------------------------------
url_inseguranca <- "https://docs.google.com/spreadsheets/d/1TXdv7iKH3P335t2x3VAgGkrEAStTbwlZCL4yJiFaghQ/edit?gid=1423538133#gid=1423538133"

df_inseguranca <- read_sheet(url_inseguranca,sheet = 2)

# Converter sheet para um tibble
df_inseguranca <- as_tibble(df_inseguranca)

# Ajustar nome das colunas (janitor)
df_inseguranca <- clean_names(df_inseguranca)

# Verificar df
glimpse(df_inseguranca)

# Gravar planilha
readr::write_rds(df_inseguranca,file = "df_inseguranca.rds")

