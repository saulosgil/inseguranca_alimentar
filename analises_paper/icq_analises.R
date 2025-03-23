# pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(tidyplots)
library(DescTools) # para poshoc de Dunnett
library(patchwork)
library(ggstatsplot)

# dataset -------------------------------------------------------------------------------------
df <- read_rds("data/df_para_analise.rds")
glimpse(df)

# vetor para ajustar eixo x dos graficos ------------------------------------------------------
level_order <- c("FS", "FI")

# ICQ vs inseguranca alimenar (2 classes) -----------------------------------------------------------------------------------
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
    ),
    icq = case_when(
      icq == 0.11 ~ 1.12,
      .default = icq
    ),
    icq_class = case_when(
      icq >= 0.90 & genero == "masculino" ~ "increased risk",
      icq < 0.90 & genero == "masculino" ~ "no risk",
      icq >= 0.85 & genero == "feminino" ~ "increased risk",
      icq < 0.85 & genero == "feminino" ~ "no risk"
  )
  )

# ICQ -----------------------------------------------------------------------
level_order = c("FS", "FI")

## Boxplot dos dados continuos de ICQ
icq_boxplot <-
  ggbetweenstats(
    data = df,
    x = ebia_class_2,
    y = icq,
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab("Waist-to-hip ratio (a.u.)") +
  theme(legend.title = element_blank()) + # remove legend label
  scale_x_discrete(limits = level_order)

### Frequência de obesos
df |>
  filter(icq_class %in% "increased risk") |>
  group_by(ebia_class_2) |>
  count(ebia_class_2)

df |>
  count(ebia_class_2)

round(62/133*100,0)
round(92/182*100,0)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c(
  "FS","FI"
)

data <- data.frame(
  count = c(47, 51),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"

icq_categ <-
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
    limits = c(0, 70),
    breaks = seq(0, 70, by = 10),
    expand = c(0, 0)
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
  ylab("Frequency of individuals with increased risk of\ncardiometabolic diseases (%)") +
  scale_x_discrete(limits = c("FS", "FI"))

icq_boxplot/icq_categ

# ICQ vs inseguranca alimenar (4 classes) -----------------------------------------------------------------------------------
# Sintomas de IMC -----------------------------------------------------------------------
level_order = c("FS","Mild FI", "Moderate FI", "Severe FI")

## Boxplot dos dados continuos de ICQ
icq_boxplot_4 <-
  ggbetweenstats(
    data = df,
    x = ebia_class,
    y = icq,
    pairwise.display = "none",
    results.subtitle = FALSE,
  ) +
  theme_classic() +
  xlab("") +
  ylab("Waist-to-hip ratio (a.u.)") +
  theme(legend.title = element_blank()) + # remove legend label
  scale_x_discrete(limits = c("FS","Mild FI", "Moderate FI", "Severe FI"))


### Frequência de obesos
df |>
  filter(icq_class %in% "increased risk") |>
  group_by(ebia_class) |>
  count(ebia_class)

df |>
  count(ebia_class)

round(62/133*100,0)
round(48/84*100,0)
round(30/78*100,0)
round(14/20*100,0)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c(
  "FS","Mild FI", "Moderate FI", "Severe FI"
)

data <- data.frame(
  count = c(47, 57, 38, 20),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
lilas <- "#7570b3"
pink <- "#e7298a"

icq_categ_4 <-
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
    limits = c(0, 70),
    breaks = seq(0, 70, by = 10),
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
  ylab("Frequency of individuals with increased risk of\ncardiometabolic diseases (%)") +
  scale_x_discrete(limits = c("FS","Mild FI", "Moderate FI", "Severe FI"))


icq_boxplot_4/icq_categ_4

# Analise estatistica - Anova ---------------------------------------------------------------
t.test(icq ~ ebia_class_2, data = df) ### NÃO TEM DIFERENÇA ####

# ANOVA com post hoc de Dunnet ----------------------------------------------------------------
#fit the one-way ANOVA model
icq_anova <- aov(icq ~ ebia_class, data= df)

#view model output
summary(icq_anova)

# Posthoc Dunnett
DunnettTest(x=df$icq,
            g=df$ebia_class,control = "FS") ### NÃO TEM DIFERENÇA ####

# Regressoes ----------------------------------------------------------------------------------
# Ajustando as labels para colocar o FS como referencia
df_reg <-
  df |>
  mutate(ebia_class_2 = case_when(ebia_class != "FS" ~ "FI", .default = ebia_class)) |>
  mutate(ebia_class_3 = case_when(ebia_class_2 == "FS" ~ "0_FS", ebia_class_2 == "FI" ~ "1_FI"))

# modelos
## crude
model <- lm(
  icq ~ ebia_class,
  data = df_reg
)

sjPlot::tab_model(model)

## ajustado
ajustado_model <- lm(
  icq ~ ebia_class + idade_class + genero + raca + estado_civil + renda_familiar + has + dm + imc_class,
  data = df_reg
)

sjPlot::tab_model(ajustado_model)

# final ---------------------------------------------------------------------------------------
