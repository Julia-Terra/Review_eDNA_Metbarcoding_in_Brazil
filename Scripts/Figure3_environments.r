library(tidyverse)
library(ggplot2)
library(readxl)
library(patchwork)
library(forcats)

# Leitura dos dados
data <- read_excel("habitats.xlsx")

# ===== GRÁFICO DA DIREITA =====
domain_counts <- data %>%
  filter(!is.na(domain)) %>%
  separate_rows(domain, sep = ",\\s*") %>%
  count(domain) %>%
  arrange(n) %>%
  mutate(domain = factor(domain, levels = domain))

colors_domain <- rep("gray40", nrow(domain_counts))

graf_direita <- ggplot(domain_counts, aes(x = domain, y = n, fill = domain)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.3, size = 4.2) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = colors_domain) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),     # <<< remove todas as grades
    axis.text.x = element_blank(),    
    axis.ticks.x = element_blank(),   
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# ===== GRÁFICO DA ESQUERDA =====
eco_counts <- data %>%
  filter(!is.na(ecosystem)) %>%
  separate_rows(ecosystem, sep = ",\\s*") %>%
  filter(ecosystem %in% c("terrestrial", "marine", "freshwater", "estuary")) %>%
  count(ecosystem) %>%
  mutate(ecosystem = fct_reorder(ecosystem, n),
         color = recode(ecosystem,
                        "terrestrial" = "gray40",
                        "marine" = "gray40",
                        "freshwater" = "gray40",
                        "estuary" = "gray40"))

graf_esquerda <- ggplot(eco_counts, aes(x = ecosystem, y = n, fill = color)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.3, size = 5, fontface = "bold", color = "black") +
  coord_flip(clip = "off") +
  scale_fill_identity() +
  scale_x_discrete(labels = c(
    "terrestrial" = "Terrestrial",
    "marine" = "Marine",
    "freshwater" = "Freshwater",
    "estuary" = "Estuarine"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.grid = element_blank(),       # <<< remove todas as grades
    plot.margin = margin(10, 0, 10, 10)
  )

# ===== JUNÇÃO COM a) e b) =====
painel_lado_a_lado <- graf_esquerda + graf_direita +
  plot_layout(ncol = 2, widths = c(1, 2)) +
  plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 16, face = "bold"))

print(painel_lado_a_lado)
# ===== SALVAR =====
ggsave("Painel_Ecossistemas_Domains_Final.png", painel_lado_a_lado,
       width = 13, height = 10, dpi = 600)

ggsave("Painel_Ecossistemas_Domains_Final.pdf", painel_lado_a_lado,
       width = 13, height = 10, dpi = 600)

# Salvar o gráfico p como TIFF, com 18cm de largura, 600 dpi
ggsave(
  filename = "Figure3_final.tiff",
  plot = painel_lado_a_lado,
  device = "tiff",
  width = 18,
  units = "cm",
  dpi = 600,
  compression = "lzw" # Uma boa compressão sem perdas
)
