# pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(tidyplots)

# dataset -------------------------------------------------------------------------------------
df <- read_rds("df_para_analise.rds")
glimpse(df)

# vetor para ajustar eixo x dos graficos ------------------------------------------------------
level_order <- c("SA","IL","IM","IG")

# IMC -----------------------------------------------------------------------------------------
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class_2, y = imc, color = ebia_class_2)|>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class, y = imc, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_value() +
  scale_x_discrete(limits = level_order)

### NADA ####
# ICQ -----------------------------------------------------------------------------------------
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class_2, y = icq, color = ebia_class_2)|>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)

df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class, y = icq, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_value() +
  scale_x_discrete(limits = level_order)

### NADA ####
# depressao -----------------------------------------------------------------------------------
## retirando outliers (depressao > 35)
df |>
  filter(
    depressao < 35
  ) |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class_2, y = depressao, color = ebia_class_2)|>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)

### TEM DIFERENÇA ####

df |>
  filter(
    depressao < 35
  ) |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class, y = depressao, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_value() +
  scale_x_discrete(limits = level_order)

### SA<IL ####

# ansiedade -----------------------------------------------------------------------------------
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class_2, y = ansiedade, color = ebia_class_2)|>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)

### NADA ####

df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class, y = ansiedade, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_value() +
  scale_x_discrete(limits = level_order)

### NADA ####

# WHO fisico ----------------------------------------------------------------------------------
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class_2, y = whoqol_fisico_escore_100, color = ebia_class_2)|>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)

### NADA ####
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class, y = whoqol_fisico_escore_100, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_value() +
  scale_x_discrete(limits = level_order)

### NADA ####
# WHO psicologico -----------------------------------------------------------------------------
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class_2, y = whoqol_psicol_escore_100, color = ebia_class_2)|>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)

### NADA ####

df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class, y = whoqol_psicol_escore_100, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_value() +
  scale_x_discrete(limits = level_order)

### NADA ####

# WHO social ----------------------------------------------------------------------------------
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class_2, y = whoqol_social_escore_100, color = ebia_class_2)|>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)

### NADA ####

df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class, y = whoqol_social_escore_100, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_value() +
  scale_x_discrete(limits = level_order)

### NADA ####

# WHO ambiente --------------------------------------------------------------------------------
df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class_2, y = whoqol_ambiente_escore_100, color = ebia_class_2)|>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)

### NADA ####

df |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    )
  ) |>
  tidyplot(x = ebia_class, y = whoqol_ambiente_escore_100, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_value() +
  scale_x_discrete(limits = level_order)

### NADA ####


# Count relativo por nivel de depressao -------------------------------------------------------
### 2 niveis
teste |>
  tidyplot(x = ebia_class_2, color = depressao_cat) |>
  add_barstack_relative() +
  scale_x_discrete(limits = c("SA", "IA"))

### 4 niveis
teste |>
  tidyplot(x = ebia_class, color = depressao_cat) |>
  add_barstack_relative() +
  scale_x_discrete(limits = c("SA", "IL", "IM", "IG"))

# regressoes ----------------------------------------------------------------------------------
teste <-
  df |>
  filter(
    depressao < 35
  ) |>
  mutate(ebia_class_2 = case_when(ebia_class != "SA" ~ "IA",
                                  .default = ebia_class)
  ) |>
  mutate(ebia_class_3 = case_when(ebia_class_2 == "SA" ~ "0_SA",
                                  ebia_class_2 == "IA" ~ "1_IA"
    )
  )

model <- glm(depressao ~ ebia_class_2 + genero + raca + estado_civil + idade + renda_familiar,
             data = teste)
sjPlot::tab_model(model)


model_2 <- glm(depressao ~ ebia_class + genero + raca + estado_civil + idade + renda_familiar,
               data = teste)
sjPlot::tab_model(model_2)


### Obs. Se remover os > 35 dos sintomas de depressao, parece que há diferença entre SA e IA quando
### comparando grupos.
### Quando comparada as frequencias de sintomas de depressao entre SA e IA, parece haver um aumento
### na frequencia de individuos com sintomas de depressao leves, moderado e severo (PRECISA TESTAR!)
### Além disso, o modelo de regressao logistica tbm indica que a IA é um preditor significante de
### sintomas de depressao (var continua).







