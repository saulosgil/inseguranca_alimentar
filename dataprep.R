# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# Download dataset ----------------------------------------------------------------------------
df <- read_rds("df.rds")

# Cleaning dataset ----------------------------------------------------------------------------
## Selecionar caracteristicas
caracteristicas <-
  df |>
  select(
    nome,
    data_de_nascimento_ex_dia_mes_ano_17_10_2022,
    genero,
    raca,
    estado_civil,
    comorbidades_13,
    medicamentos_de_uso_continuo_em_caso_de_nenhum_escrever_nenhum
)

## Selecionar preditor - Insegurança alimentar
inseguranca_alimentar <-
  df |>
  select(
    starts_with("x")
  )

## Selecionar fator de risco CV - peso, estatura, circunf. cintura e quadril
fator_risco_cv <-
  df |>
  select(
    peso_em_kg_ex_71_8_separador,
    estatura_em_cm_ex_170,
    circunferencia_cintura_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador,
    circunferencia_quadril_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador
)

## Selecionar preditor - Sintomas de ansiedade
## Selecionar preditor - Sintomas de depressão
## Selecionar preditor - Indicadores de Qol

glimpse(df)
head(df)
