library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

####### heatmap ########

dados <- read_excel("Pool-7-8-9-11.xls")

# ajuste dos dados, transf em fator
dados <- dados %>%
  mutate(
    Família = str_squish(Família),
    P = factor(P, levels = c(1, 2, 3, 4))
  )


# Somar reads por Pool e Família
dados_sum <- dados %>%
  group_by(P, Família) %>%
  summarise(
    NbReads = sum(NbReads, na.rm = TRUE),
    .groups = "drop"
  )


# Completar combinações ausentes (0 reads)
mat <- dados_sum %>%
  complete(P, Família, fill = list(NbReads = 0))


# Ordenar famílias pela abundância total
mat <- mat %>%
  group_by(Família) %>%
  mutate(total = sum(NbReads)) %>%
  ungroup() %>%
  mutate(Família = reorder(Família, total))



# Heatmap (escala log10)
ggplot(mat, aes(x = P, y = Família, fill = log10(NbReads + 1))) +
  geom_tile() + 
  scale_fill_gradientn(
    colours = c("#5e4fa2", "#3288bd", "#66c2a5", "#fee08b", "#d53e4f"),
    name   = "NbReads",
    breaks = log10(c(0, 10, 100, 1000, 10000, 100000) + 1),
    labels = c("0", "10", expression(10^2), expression(10^3), expression(10^4), expression(10^5))
  ) +
  labs(
    x = "Pool",
    y = "",
    title = "Heatmap de abundância de reads por família viral e por pool",
    subtitle = "Escala logarítmica: log10(NbReads + 1)"
  ) +
  coord_fixed(ratio = 0.25) +  
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.title  = element_text(size = 10),
    plot.title  = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )

# salvar
ggsave(
  "heatmap_abundancia_log10.pdf",
  width = 20,
  height = 24,
  units = "cm"
)


