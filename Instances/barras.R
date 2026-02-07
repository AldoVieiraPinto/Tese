library(ggplot2)
library(dplyr)

# ======================
# DADOS
# ======================
dados <- data.frame(
  Risk = rep(c("NEU", "CVaR", "MCVaR"), each = 4),
  Method = rep(c("Type 1", "Type 2"), each = 2, times = 3),
  grupos = rep(c("10 grupos", "5 grupos"), times = 6),
  Frequency = c(21/27, 23/27, 23/27, 23/27,
                30/54, 25/54, 32/54, 32/54,
                32/54, 36/54, 37/54, 35/54)
) %>%
  mutate(Frequency = Frequency * 100)

# ======================
# POSIÇÕES NO EIXO X
# ======================
posicoes <- c(1, 1.8, 2.6, 3.8, 4.6, 5.4)
dados$Xpos <- rep(posicoes, each = 2)

# Labels: RISCO em cima, MÉTODO embaixo
labels_x <- c(
  "NEU", "CVaR\n\n UMApHMP-UR", "MCVaR",
  "NEU", "CVaR\n\n UMApHMP-THR", "MCVaR"
)

dados$grupos <- factor(dados$grupos, levels = c("10 grupos", "5 grupos"))

# ======================
# GRÁFICO
# ======================
ggplot(dados, aes(x = Xpos, y = Frequency, fill = grupos)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  scale_fill_manual(
    values = c("10 grupos" = "blue", "5 grupos" = "red")
  ) +
  scale_x_continuous(
    breaks = posicoes,
    labels = labels_x
  ) +
  labs(
    y = "Frequência do valor ótimo (%)",
    x = "",
    fill = "Número de grupos"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      margin = margin(t = 5),
      lineheight = 0.9
    )
  )
