library(ggplot2)
library(tidyr)

# Dataset completo
df <- data.frame(
  Instancia = c(
    "H-3-0.4-0.8","H-3-0.4-0.9","H-3-0.6-0.8","H-3-0.6-0.9","H-3-0.8-0.8","H-3-0.8-0.9",
    "H-4-0.4-0.8","H-4-0.4-0.9","H-4-0.6-0.8","H-4-0.6-0.9","H-4-0.8-0.8","H-4-0.8-0.9",
    "H-5-0.4-0.8","H-5-0.4-0.9","H-5-0.6-0.8","H-5-0.6-0.9","H-5-0.8-0.8","H-5-0.8-0.9"
  ),
  CPLEX = c(
    7093.89,7091.56,8444.08,9280.93,7267.12,10890.60,
    26824.69,23571.53,26322.50,26442.44,21970.74,19883.53,
    30749.89,29268.07,34973.51,37280.35,29865.81,32434.39
  ),
  ILS = c(
    0.076937,0.072730,0.070800,0.088377,0.072593,0.081587,
    0.281253,0.289327,0.374070,0.235667,0.263333,0.277880,
    0.504403,0.761570,0.640503,0.684477,0.670320,0.719820
  ),
  Agr10 = c(
    202.4448,209.5420,274.2521,274.5977,304.6836,305.2070,
    323.4938,323.8169,412.8127,415.6927,383.7801,383.8831,
    433.6988,422.4146,500.3515,501.5982,446.6656,446.5097
  ),
  Agr5 = c(
    1167.6652,1179.7251,1578.9090,1578.8031,1727.2812,1710.8354,
    2064.2825,2064.0821,2674.6402,2614.3993,2353.6423,2368.1424,
    2142.3405,2143.9595,2488.7548,2463.8191,2473.7410,2415.8050
  )
)

# Transformar para formato longo
df_long <- pivot_longer(
  df,
  cols = c("CPLEX","ILS","Agr10","Agr5"),
  names_to = "Metodo",
  values_to = "Tempo"
)

# Ajustar nomes para legenda (INGLÊS)
df_long$Metodo <- factor(
  df_long$Metodo,
  levels = c("CPLEX","ILS","Agr10","Agr5"),
  labels = c("CPLEX", "ILS", "Agrupamento - 10 grupos", "Agrupamento - 5 grupos")
)

# Breaks nas potências de 10
y_breaks <- 10^(floor(log10(min(df_long$Tempo))) :
                  ceiling(log10(max(df_long$Tempo))))

# Gráfico
ggplot(df_long, aes(x = Instancia, y = Tempo, group = Metodo)) +
  geom_line(aes(color = Metodo), linewidth = 0.8) +
  geom_point(aes(color = Metodo), size = 3) +
  scale_color_manual(values = c(
    "CPLEX" = "#1f77b4",
    "ILS" = "#d62728",
    "Agrupamento - 10 grupos" = "#2ca02c",
    "Agrupamento - 5 grupos" = "#9467bd"
  )) +
  scale_y_log10(
    breaks = y_breaks,
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    title = "",
    x = "Instância",
    y = "Tempo (s)",
    color = "Method"
  ) +
  annotation_logticks(sides = "l") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

