# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)

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
    renda_familiar_considere_a_renda_de_todas_as_pessoas_que_moral_na_sua_casa,
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
sintomas_ansiedade <-
  df |>
  select(
    starts_with('inventario_de_ansiedade')
)

## Selecionar preditor - Sintomas de depressão
sintomas_depressao <-
  df |>
  select(
    starts_with('inventario_de_depressao')
)

## Selecionar preditor - Indicadores de Qol
whoqol <-
  df |>
  select(
    starts_with('the_world')
)

## Juntar todas as variáveis
df <-
  bind_cols(
  caracteristicas,
  inseguranca_alimentar,
  fator_risco_cv,
  sintomas_ansiedade,
  sintomas_depressao,
  whoqol
)

# verifica as colunas do dataset
glimpse(df)

# tratamento das variáveis que aparecem como list ---------------------------------------------
# data de nascimento
# tira da lista e cria um vetor de datas de nascimento (porém as linhas erradas foram excluidas)
d <- unlist(df$data_de_nascimento_ex_dia_mes_ano_17_10_2022,
            use.names = TRUE)

# Muda a barra para hífen para depois converter para data
d <- str_replace_all(d,  "/", "-")

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
data_nasc <- bind_rows(d, c_d)
data_nasc

# Muda o vetor original de data de nascimento para o vetor ajustado do tipo Date
df_ajustado <-
  df |>
  mutate(
    data_de_nascimento_ex_dia_mes_ano_17_10_2022 = data_nasc$value
  ) |>
  rename(
    data_nasc = data_de_nascimento_ex_dia_mes_ano_17_10_2022
  ) |>
  mutate(
    data_nasc = as.Date(data_nasc, format = "%d-%m-%Y")
  )

# Peso
# tira da lista e cria um vetor com os pesos (porém as linhas erradas foram excluidas)
p <- unlist(df$peso_em_kg_ex_71_8_separador,
            use.names = TRUE)

# converte em tibble
p <- as_tibble(p)
p

# cria um vetor de NA para incluir nas linhas erradas
c_p <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_p <- as_tibble(c_p)
c_p

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
peso <- bind_rows(p, c_p)
peso

# Muda o vetor original de peso para o vetor ajustado do tipo Double
# df <-
df_ajustado <-
  df_ajustado |>
  mutate(
    peso_em_kg_ex_71_8_separador = peso$value
  ) |>
  rename(
    peso = peso_em_kg_ex_71_8_separador
  ) |>
  mutate(
    peso = as.double(peso)
  )

# Estatura
# tira da lista e cria um vetor com as estaturas em cm (porém as linhas erradas foram excluidas)
e <- unlist(df$estatura_em_cm_ex_170,
            use.names = TRUE)

# converte em tibble
e <- as_tibble(e)
e

# cria um vetor de NA para incluir nas linhas erradas
c_e <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_e <- as_tibble(c_e)
c_e

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
estatura <- bind_rows(e, c_e)
estatura

# Muda o vetor original de estatura para o vetor ajustado do tipo Double
# df <-
df_ajustado <-
  df_ajustado |>
  mutate(
    estatura_em_cm_ex_170 = estatura$value
  ) |>
  rename(
    estatura = estatura_em_cm_ex_170
  ) |>
  mutate(
    estatura = as.double(estatura)
  )

# Circunferencia de cintura
# tira da lista e cria um vetor com as circunferencias da cintura em cm (porém as linhas erradas foram excluidas)
cc <- unlist(df$circunferencia_cintura_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador,
            use.names = TRUE)

# converte em tibble
cc <- as_tibble(cc)
cc

# cria um vetor de NA para incluir nas linhas erradas
c_cc <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_cc <- as_tibble(c_cc)
c_cc

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
circ_cintura <- bind_rows(cc, c_cc)
circ_cintura

# Muda o vetor original da circunf. da cintura para o vetor ajustado do tipo String
# df <-
df_ajustado <-
  df_ajustado |>
  mutate(
    circunferencia_cintura_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador = circ_cintura$value
  ) |>
  rename(
    circ_cintura = circunferencia_cintura_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador
  )

# Circunferencia do quadril
# tira da lista e cria um vetor com as circunferencias da cintura em cm (porém as linhas erradas foram excluidas)
q <- unlist(df$circunferencia_quadril_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador,
            use.names = TRUE)

# converte em tibble
q <- as_tibble(q)
q

# cria um vetor de NA para incluir nas linhas erradas
c_q <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_q <- as_tibble(c_q)
c_q

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
circ_quadril <- bind_rows(q, c_q)
circ_quadril

# Muda o vetor original da circunf. da cintura para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    circunferencia_quadril_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador = circ_quadril$value
  ) |>
  rename(
    circ_quadril = circunferencia_quadril_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador
  )

# inventario_de_ansiedade_de_beck_demencia_ou_formigamento
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans1 <- unlist(df$inventario_de_ansiedade_de_beck_demencia_ou_formigamento,
               use.names = TRUE)

# converte em tibble
ans1 <- as_tibble(ans1)
ans1

# cria um vetor de NA para incluir nas linhas erradas
c_ans1 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans1 <- as_tibble(c_ans1)
c_ans1

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
v_ans1 <- bind_rows(ans1, c_ans1)
v_ans1

# Muda o vetor original da circunf. da cintura para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_demencia_ou_formigamento = v_ans1$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_demencia_ou_formigamento = as.integer(inventario_de_ansiedade_de_beck_demencia_ou_formigamento)
  )

df_ajustado |>
  select(where(is.list)) |>
  glimpse()
























