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

# Muda o vetor original da inventario_de_ansiedade_de_beck_demencia_ou_formigamento para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_demencia_ou_formigamento = v_ans1$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_demencia_ou_formigamento = as.integer(inventario_de_ansiedade_de_beck_demencia_ou_formigamento)
  )

# inventario_de_ansiedade_de_beck_sensacao_de_calor
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans2 <- unlist(df$inventario_de_ansiedade_de_beck_sensacao_de_calor,
               use.names = TRUE)

# converte em tibble
ans2 <- as_tibble(ans2)
ans2 <-
  ans2 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans2 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans2 <- as_tibble(c_ans2)
c_ans2

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
v_ans2 <- bind_rows(ans2, c_ans2)
v_ans2

# Muda o vetor original da inventario_de_ansiedade_de_beck_sensacao_de_calor para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_sensacao_de_calor = v_ans2$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_sensacao_de_calor = as.integer(inventario_de_ansiedade_de_beck_sensacao_de_calor)
  )

# inventario_de_ansiedade_de_beck_tremores_nas_pernas
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans3 <- unlist(df$inventario_de_ansiedade_de_beck_tremores_nas_pernas,
               use.names = TRUE)

# converte em tibble
ans3 <- as_tibble(ans3)
ans3 <-
  ans3 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans3 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans3 <- as_tibble(c_ans3)
c_ans3

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
v_ans3 <- bind_rows(ans3, c_ans3)
v_ans3

# Muda o vetor original da inventario_de_ansiedade_de_beck_tremores_nas_pernas para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_pernas = v_ans3$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_pernas = as.integer(inventario_de_ansiedade_de_beck_tremores_nas_pernas)
  )

# inventario_de_ansiedade_de_beck_incapaz_de_relaxar
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans4 <- unlist(df$inventario_de_ansiedade_de_beck_incapaz_de_relaxar,
               use.names = TRUE)

# converte em tibble
ans4 <- as_tibble(ans4)
ans4 <-
  ans4 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans4 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans4 <- as_tibble(c_ans4)
c_ans4

# Empilha as linhas para ter um vetor de 419 linhas e, assim, poder juntar no df original
v_ans4 <- bind_rows(ans4, c_ans4)
v_ans4

# Muda o vetor original da inventario_de_ansiedade_de_beck_incapaz_de_relaxar para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_incapaz_de_relaxar = v_ans4$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_incapaz_de_relaxar = as.integer(inventario_de_ansiedade_de_beck_incapaz_de_relaxar)
  )

# inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans5 <- unlist(df$inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior,
               use.names = TRUE)

# converte em tibble
ans5 <- as_tibble(ans5)
ans5 <-
  ans5 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans5 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans5 <- as_tibble(c_ans5)
c_ans5

# Empilha as linhas para ter um vetor de 519 linhas e, assim, poder juntar no df original
v_ans5 <- bind_rows(ans5, c_ans5)
v_ans5

# Muda o vetor original da inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior = v_ans5$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior = as.integer(inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior)
  )

# inventario_de_ansiedade_de_beck_sem_equilibrio
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans6 <- unlist(df$inventario_de_ansiedade_de_beck_sem_equilibrio,
               use.names = TRUE)

# converte em tibble
ans6 <- as_tibble(ans6)
ans6 <-
  ans6 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans6 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans6 <- as_tibble(c_ans6)
c_ans6

# Empilha as linhas para ter um vetor de 619 linhas e, assim, poder juntar no df original
v_ans6 <- bind_rows(ans6, c_ans6)
v_ans6

# Muda o vetor original da inventario_de_ansiedade_de_beck_sem_equilibrio para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_sem_equilibrio = v_ans6$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_sem_equilibrio = as.integer(inventario_de_ansiedade_de_beck_sem_equilibrio)
  )

# inventario_de_ansiedade_de_beck_nervoso
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans7 <- unlist(df$inventario_de_ansiedade_de_beck_nervoso,
               use.names = TRUE)

# converte em tibble
ans7 <- as_tibble(ans7)
ans7 <-
  ans7 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans7 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans7 <- as_tibble(c_ans7)
c_ans7

# Empilha as linhas para ter um vetor de 719 linhas e, assim, poder juntar no df original
v_ans7 <- bind_rows(ans7, c_ans7)
v_ans7

# Muda o vetor original da inventario_de_ansiedade_de_beck_nervoso para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_nervoso = v_ans7$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_nervoso = as.integer(inventario_de_ansiedade_de_beck_nervoso)
  )

# inventario_de_ansiedade_de_beck_sensacao_de_sufocacao
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans8 <- unlist(df$inventario_de_ansiedade_de_beck_sensacao_de_sufocacao,
               use.names = TRUE)

# converte em tibble
ans8 <- as_tibble(ans8)
ans8 <-
  ans8 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans8 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans8 <- as_tibble(c_ans8)
c_ans8

# Empilha as linhas para ter um vetor de 819 linhas e, assim, poder juntar no df original
v_ans8 <- bind_rows(ans8, c_ans8)
v_ans8

# Muda o vetor original da inventario_de_ansiedade_de_beck_sensacao_de_sufocacao para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_sensacao_de_sufocacao = v_ans8$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_sensacao_de_sufocacao = as.integer(inventario_de_ansiedade_de_beck_sensacao_de_sufocacao)
  )

# inventario_de_ansiedade_de_beck_tremores_nas_maos
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans9 <- unlist(df$inventario_de_ansiedade_de_beck_tremores_nas_maos,
               use.names = TRUE)

# converte em tibble
ans9 <- as_tibble(ans9)
ans9 <-
  ans9 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans9 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans9 <- as_tibble(c_ans9)
c_ans9

# Empilha as linhas para ter um vetor de 919 linhas e, assim, poder juntar no df original
v_ans9 <- bind_rows(ans9, c_ans9)
v_ans9

# Muda o vetor original da inventario_de_ansiedade_de_beck_tremores_nas_maos para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_maos = v_ans9$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_maos = as.integer(inventario_de_ansiedade_de_beck_tremores_nas_maos)
  )

# inventario_de_ansiedade_de_beck_tremores_nas_maos
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans10 <- unlist(df$inventario_de_ansiedade_de_beck_tremores_nas_maos,
                use.names = TRUE)

# converte em tibble
ans10 <- as_tibble(ans10)
ans10 <-
  ans10 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans10 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans10 <- as_tibble(c_ans10)
c_ans10

# Empilha as linhas para ter um vetor de 10110 linhas e, assim, poder juntar no df original
v_ans10 <- bind_rows(ans10, c_ans10)
v_ans10

# Muda o vetor original da inventario_de_ansiedade_de_beck_tremores_nas_maos para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_maos = v_ans10$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_maos = as.integer(inventario_de_ansiedade_de_beck_tremores_nas_maos)
  )
# inventario_de_depressao_de_beck_questao_11
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
dep1 <- unlist(df$inventario_de_depressao_de_beck_questao_11,
               use.names = TRUE)

# converte em tibble
dep1 <- as_tibble(dep1)
dep1 <-
  dep1 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_dep1 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_dep1 <- as_tibble(c_dep1)
c_dep1

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_dep1 <- bind_rows(dep1, c_dep1)
v_dep1

# Muda o vetor original da inventario_de_depressao_de_beck_questao_11 para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_depressao_de_beck_questao_11 = v_dep1$value
  ) |>
  mutate(
    inventario_de_depressao_de_beck_questao_11 = as.integer(inventario_de_depressao_de_beck_questao_11)
  )

# inventario_de_depressao_de_beck_questao_12
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
dep2 <- unlist(df$inventario_de_depressao_de_beck_questao_12,
               use.names = TRUE)

# converte em tibble
dep2 <- as_tibble(dep2)
dep2 <-
  dep2 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_dep2 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_dep2 <- as_tibble(c_dep2)
c_dep2

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_dep2 <- bind_rows(dep2, c_dep2)
v_dep2

# Muda o vetor original da inventario_de_depressao_de_beck_questao_12 para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_depressao_de_beck_questao_12 = v_dep2$value
  ) |>
  mutate(
    inventario_de_depressao_de_beck_questao_12 = as.integer(inventario_de_depressao_de_beck_questao_12)
  )

# inventario_de_depressao_de_beck_questao_19
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
dep3 <- unlist(df$inventario_de_depressao_de_beck_questao_19,
               use.names = TRUE)

# converte em tibble
dep3 <- as_tibble(dep3)
dep3 <-
  dep3 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_dep3 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_dep3 <- as_tibble(c_dep3)
c_dep3

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_dep3 <- bind_rows(dep3, c_dep3)
v_dep3

# Muda o vetor original da inventario_de_depressao_de_beck_questao_19 para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_depressao_de_beck_questao_19 = v_dep3$value
  ) |>
  mutate(
    inventario_de_depressao_de_beck_questao_19 = as.integer(inventario_de_depressao_de_beck_questao_19)
  )

df_ajustado |>
  select(where(is.list)) |>
  glimpse()


