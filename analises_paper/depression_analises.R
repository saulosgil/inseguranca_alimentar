# pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(tidyplots)
library(DescTools) # para poshoc de Dunnett
library(patchwork)

# dataset -------------------------------------------------------------------------------------
df <- read_rds("df_para_analise.rds")
glimpse(df)

# vetor para ajustar eixo x dos graficos ------------------------------------------------------
level_order <- c("FS", "FI")

# depressao vs inseguranca alimenar (2 classes) -----------------------------------------------------------------------------------
## retirando outliers (depressao > 35)
df <-
  df |>
  filter(
    depressao < 35
  ) |>
  mutate(
    ebia_class = case_when(
      ebia_class == "SA" ~ "FS",
      ebia_class == "IL" ~ "Mild FI",
      ebia_class == "IM" ~ "Moderate FI",
      ebia_class == "IG" ~ "Severe FI"
    )
  ) |>
  mutate(
    ebia_class_2 = case_when(
      ebia_class != "FS" ~ "FI",
      .default = ebia_class
    ),
    idade_class = case_when(
      idade < 60 ~ "adults",
      idade >= 60 ~ "older"
    ),
    imc_class = case_when(
      imc < 30 ~ "non obese",
      imc >= 30 ~ "obese"
    )
  )

# Sintomas de depressao -----------------------------------------------------------------------
level_order = c("FS", "FI")

## Boxplot dos dados continuos de depressão
dep_boxplot <-
  ggbetweenstats(
    data = df,
    x = ebia_class_2,
    y = depressao,
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab(" Beck Depression Inventory Score (a.u.)") +
  theme(legend.title = element_blank()) + # remove legend label
  scale_x_discrete(limits = level_order)

### Frequência de sintomas moderados/severos
df |>
  filter(depressao_cat %in% c("Moderate","Severe")) |>
  group_by(ebia_class_2) |>
  count(depressao_cat)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c(
  "FS","FI"
)

data <- data.frame(
  count = c(6, 16),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"

dep_categ <-
  data |>
  ggplot(mapping = aes(x = names, y = count)) +
  geom_bar(
    stat = "identity",
    fill = c(orange, green),
    colour = "black",
    alpha = .6,
    width = .6
  ) +
  scale_y_continuous(
    limits = c(0, 18),
    breaks = seq(0, 18, by = 6),
    expand = c(0, 0),
    # The horizontal axis does not extend to either side
  ) +
  xlab("") +
  theme_classic() +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  ylab("Frequency of individuals showing moderate/severe\n symptoms of depression (%)") +
  scale_x_discrete(limits = c("FS", "FI"))


dep_boxplot/dep_categ

# depressao vs inseguranca alimenar (4 classes) -----------------------------------------------------------------------------------
# Sintomas de depressao -----------------------------------------------------------------------
level_order = c("FS","Mild FI", "Moderate FI", "Severe FI")

## Boxplot dos dados continuos de depressão
dep_boxplot_4 <-
  ggbetweenstats(
    data = df,
    x = ebia_class,
    y = depressao,
    pairwise.display = "none",
    results.subtitle = FALSE,
  ) +
  theme_classic() +
  xlab("") +
  ylab(" Beck Depression Inventory Score (a.u.)") +
  theme(legend.title = element_blank()) + # remove legend label
  scale_x_discrete(limits = c("FS","Mild FI", "Moderate FI", "Severe FI"))


### Frequência de sintomas moderados/severos
df |>
  filter(depressao_cat %in% c("Moderate","Severe")) |>
  group_by(ebia_class) |>
  count(depressao_cat)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c(
  "FS","Mild FI", "Moderate FI", "Severe FI"
)

data <- data.frame(
  count = c(6, 8, 7, 1),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
lilas <- "#7570b3"
pink <- "#e7298a"

dep_categ_4 <-
  data |>
  ggplot(mapping = aes(x = names, y = count)) +
  geom_bar(
    stat = "identity",
    fill = c(green,orange,lilas,pink),
    colour = "black",
    alpha = .6,
    width = .6
  ) +
  scale_y_continuous(
    limits = c(0, 18),
    breaks = seq(0, 18, by = 6),
    expand = c(0, 0),
    # The horizontal axis does not extend to either side
  ) +
  xlab("") +
  theme_classic() +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  ylab("Frequency of individuals showing moderate/severe\n symptoms of depression (%)") +
  scale_x_discrete(limits = c("FS","Mild FI", "Moderate FI", "Severe FI"))


dep_boxplot_4/dep_categ_4

# Analise estatistica - Anova ---------------------------------------------------------------
t.test(depressao ~ ebia_class_2, data = df) ### TEM DIFERENÇA ####

# ANOVA com post hoc de Dunnet ----------------------------------------------------------------
#fit the one-way ANOVA model
depressao_anova <- aov(depressao ~ ebia_class, data= df)

#view model output
summary(depressao_anova)

# Posthoc Dunnett
DunnettTest(x=df$depressao,
            g=df$ebia_class,control = "FS") ### NÃO TEM DIFERENÇA ####

# Regressoes ----------------------------------------------------------------------------------
# Ajustando as labels para colocar o FS como referencia
df_reg <-
  df |>
  mutate(ebia_class_2 = case_when(ebia_class != "FS" ~ "FI", .default = ebia_class)) |>
  mutate(ebia_class_3 = case_when(ebia_class_2 == "FS" ~ "0_FS", ebia_class_2 == "FI" ~ "1_FI")) |>
  mutate(
    depressao_cat_severe = case_when(
      depressao_cat == "Mild" ~ "Mild:Minimal",
      depressao_cat == "Minimal" ~ "Mild:Minimal",
      depressao_cat == "Moderate" ~ "Moderate:Severe",
      depressao_cat == "Severe" ~ "Moderate:Severe"
    )
  )

# modelos
model <- lm(
  depressao ~ ebia_class_3 + idade_class + genero + raca + estado_civil + renda_familiar + has + dm + imc_class,
  data = df_reg
)

sjPlot::tab_model(model)

# final ---------------------------------------------------------------------------------------
