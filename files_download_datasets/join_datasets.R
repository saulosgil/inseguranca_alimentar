# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# download datasets - insegurança alimentar e PAEC --------------------------------------------
df_inseguranca <- read_rds("df_inseguranca.rds")

df_paec <- read_rds("df_paec.rds")

# Padronizar coluna nome para fazer o Join ----------------------------------------------------

#----------------------------------------------------------------------------------------------
# Funcao para remover acentos de string
RemoveAcentos <- function(textoComAcentos) {

  # Se nao foi informado texto
  if(!is.character(textoComAcentos)){
    on.exit()
  }

  # Letras com acentos
  letrasComAcentos <- "áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ´`^~¨"

  # Letras equivalentes sem acentos
  letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC     "

  textoSemAcentos <- chartr(
    old = letrasComAcentos,
    new = letrasSemAcentos,
    x = textoComAcentos
  )

  # Retorno da funcao
  return(textoSemAcentos)
}

#-------------------------------------------------------------------------------------
# Dataset - insegurança alimentar
df_inseguranca <-
  df_inseguranca |>
  # Remove espaços
  mutate_if(is.character, str_squish) |>
  # Coloca tudo em minúsculo
  mutate_if(is.character, str_to_lower) |>
  # Remove acentos
  mutate_if(is.character, RemoveAcentos) |>
  # Remove dados duplicados
  distinct(nome_completo,.keep_all = TRUE) |>
  # renomeando para ficar igual ao outro dataset
  rename(nome = nome_completo)

# Dataset - PAEC
df_paec <-
  df_paec |>
  # Remove espaços
  mutate_if(is.character, str_squish) |>
  # Coloca tudo em minúsculo
  mutate_if(is.character, str_to_lower) |>
  # Remove acentos
  mutate_if(is.character, RemoveAcentos) |>
  # Remove dados duplicados
  distinct(nome,.keep_all = TRUE)

# join ----------------------------------------------------------------------------------------
df <- left_join(x = df_inseguranca,
                y = df_paec,
                by = "nome")

# write full dataset --------------------------------------------------------------------------
readr::write_rds(df,
                 file = "df.rds")
