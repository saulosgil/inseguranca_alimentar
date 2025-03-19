# pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(tidyplots)
library(DescTools) # para poshoc de Dunnett

# dataset -------------------------------------------------------------------------------------
df <- read_rds("df_para_analise.rds")
glimpse(df)

# vetor para ajustar eixo x dos graficos ------------------------------------------------------
level_order <- c("SA","IL","IM","IG")

# depressao -----------------------------------------------------------------------------------
## retirando outliers (depressao > 35)
df <-
  df |>
  filter(
    depressao < 35
  ) |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "SA" ~ "IA",
      .default = ebia_class
    ),
    idade_class = case_when(
      idade < 60 ~ "adults",
      idade <= 60 ~ "older"
    ),
    imc_class = case_when(
      imc < 30 ~ "non obese",
      imc >= 30 ~ "obese"
    )
  )

# Grafico
df |>
  tidyplot(x = ebia_class_2, y = depressao, color = ebia_class_2) |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE)

# test T
t.test(depressao ~ ebia_class_2, data = df) ### TEM DIFERENÇA ####

df |>
  filter(
    depressao < 35
  ) |>
  tidyplot(x = ebia_class, y = depressao, color = ebia_class)|>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_data_points_beeswarm() |>
  add_test_pvalue(hide_info = TRUE) +
  scale_x_discrete(limits = level_order)

#fit the one-way ANOVA model
depressao_anova <- aov(depressao ~ ebia_class, data= df)

#view model output
summary(depressao_anova)

# Posthoc Dunnett
DunnettTest(x=df$depressao,
            g=df$ebia_class,control = "SA") ### NÃO TEM DIFERENÇA ####

# Count relativo por nivel de ansiedade -------------------------------------------------------
### 2 niveis
df |>
  tidyplot(x = ebia_class_2, color = depressao_cat) |>
  add_barstack_relative() +
  scale_x_discrete(limits = c("SA", "IA"))


### 4 niveis
df |>
  tidyplot(x = ebia_class, color = depressao_cat) |>
  add_barstack_relative() +
  scale_x_discrete(limits = c("SA", "IL", "IM", "IG"))

# regressoes ----------------------------------------------------------------------------------
# Ajustando as labels para colocar o SA como referencia
df_reg <-
  df |>
  filter(depressao < 35) |>
  mutate(ebia_class_2 = case_when(ebia_class != "SA" ~ "IA", .default = ebia_class)) |>
  mutate(ebia_class_3 = case_when(ebia_class_2 == "SA" ~ "0_SA", ebia_class_2 == "IA" ~ "1_IA")) |>
  mutate(
    depressao_cat_severe = case_when(
      depressao_cat == "Mild" ~ "Mild:Minimal",
      depressao_cat == "Minimal" ~ "Mild:Minimal",
      depressao_cat == "Moderate" ~ "Moderate:Severe",
      depressao_cat == "Severe" ~ "Moderate:Severe"
    )
  )

# modelos
model <- glm(depressao ~ ebia_class_3 + genero + raca + estado_civil + idade_class + renda_familiar + has + dm + imc_class,
             data = df_reg)
sjPlot::tab_model(model)
df$
