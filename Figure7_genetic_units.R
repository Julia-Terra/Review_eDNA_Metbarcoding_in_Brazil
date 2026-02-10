ASV/OTU
library(tidyverse)
library(eulerr)
library(grid)
library(png)
library(ggplot2)
library(patchwork)

# === 1. VETOR PARA VENN COM VALORES CORRETOS ===
venn_vector <- c(
  "ASV" = 14,             # 12 + 2 do ASV&OTU
  "OTU" = 24,             # 22 + 2 do ASV&OTU
  "Species" = 10,
  "ASV&OTU" = 2,
  "ASV&Species" = 0,
  "OTU&Species" = 0,
  "ASV&OTU&Species" = 0
)

cores <- c(
  "ASV" = "#66C2A5",
  "OTU" = "#FC8D62",
  "Species" = "#7F7F7F"
)

venn_plot <- plot(
  euler(venn_vector),
  fills = list(fill = unname(cores), alpha = 0.7),
  quantities = list(type = "counts", font = 2),
  labels = list(font = 2)
)

# Salvar imagem do Venn
png("venn_final.png", width = 1000, height = 1000, res = 300)
plot(venn_plot)
dev.off()

# Importar como grob
venn_img <- png::readPNG("venn_final.png")
venn_grob <- rasterGrob(venn_img, interpolate = TRUE)

venn_gg <- ggplot() +
  annotation_custom(venn_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()

# === 2. CRIAR BARPLOT MANUAL COM DADOS FORNECIDOS ===

# Dados organizados manualmente
data_bar <- tribble(
  ~type_organism,  ~id_type,   ~count,
  "Amphibians",    "ASV",      0,
  "Amphibians",    "OTU",      4,
  "Amphibians",    "Species",  3,
  
  "Arthropods",    "ASV",      3,
  "Arthropods",    "OTU",      5,
  "Arthropods",    "Species",  1,
  
  "Mammals",       "ASV",      0,
  "Mammals",       "OTU",      4,
  "Mammals",       "Species",  1,
  
  "Fishes",        "ASV",      4,  # 2 + 2
  "Fishes",        "OTU",      9,  # 7 + 2
  "Fishes",        "Species",  2,
  
  "Benthic organisms", "ASV",      4,
  "Benthic organisms", "OTU",      2,
  "Benthic organisms", "Species",  0
  
  
)

cores <- c(
  "ASV" = "#66C2A5",
  "OTU" = "#FC8D62",
  "Species" = "#7F7F7F"
)

bar_plot <- ggplot(filter(data_bar, count > 0), aes(x = type_organism, y = count, fill = id_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = count),
    position = position_dodge(0.9),
    size = 4.2,
    vjust = -0.3,
    color = "black"
  ) +
  scale_fill_manual(values = cores, name = NULL) +
  scale_y_continuous(
    breaks = seq(0, max(data_bar$count), by = 1),
    labels = scales::number_format(accuracy = 1)
  ) +
  labs(x = NULL, y = "Number of studies") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 13),
    axis.title.y = element_text(size = 14),
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_blank()
  )

library(patchwork)

# Montar painel com labels automáticos a) e b)
final_plot <- venn_gg / bar_plot +
  plot_annotation(tag_levels = list(c("(a)", "(b)"))) &
  theme(plot.tag = element_text(size = 14, face = "plain", vjust = 1.5),
        plot.tag.position = c(0, 1),
  )

print(final_plot)
#Salvar em PDF
 ggsave("painel_venn_barplot_FINAL.pdf", final_plot, width = 10, height = 10, dpi = 300)

# Salvar como PNG (alta resolução)
ggsave("painel_venn_barplot_FINAL.png", final_plot, width = 10, height = 10, dpi = 600)

ggsave(
  filename = "Figure7.tiff",
  plot = final_plot,
  device = "tiff",
  width = 18,
  units = "cm",
  dpi = 600,
  compression = "lzw" # Uma boa compressão sem perdas
)
