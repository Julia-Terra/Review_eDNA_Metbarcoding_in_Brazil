# === PACOTES ===
library(tidyverse)
library(readxl)

# === 1. LEITURA DOS DADOS ===
df <- read_excel("eDNA_BR_metadados.xlsx")

# === 2. CONTAGEM DE FREQUÊNCIA POR MARCADOR (loci) ===
freq_loci <- df %>%
  mutate(loci = strsplit(as.character(loci), ",\\s*")) %>%  # separa múltiplos loci se houver
  unnest(loci) %>%
  mutate(loci = str_trim(loci)) %>%                         # remove espaços extras
  count(loci, sort = TRUE)                                  # conta frequência e ordena

# === 3. GRÁFICO DE BARRAS ===
graf_loci <- ggplot(freq_loci, aes(x = fct_reorder(loci, - n), y = n)) +
  geom_col(fill = "gray40", color = "black") +               # barras cinza, sem cor extra
  geom_text(aes(label = n), vjust = -0.3, size = 4) +        # valores acima das barras
  labs(x = "Genetic Marker", y = "Frequency") +
  expand_limits(y = max(freq_loci$n) * 1.1) + 
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.margin = margin(10, 10, 10, 10)
  ) 

# === 4. EXIBIR O GRÁFICO ===
print(graf_loci)

# === 5. SALVAR EM ALTA RESOLUÇÃO ===
ggsave("barplot_loci_frequency_cinza.png", graf_loci, width = 8, height = 6, dpi = 600)
ggsave("barplot_loci_frequency_cinza.pdf", graf_loci, width = 8, height = 6, dpi = 600)
library(tidyverse)
library(ggplot2)
library(readxl)
library(patchwork)
library(forcats)

# Salvar o gráfico p como TIFF, com 18cm de largura, 600 dpi
ggsave(
  filename = "Figure4_final.tiff",
  plot = graf_loci,
  device = "tiff",
  width = 18,
  units = "cm",
  dpi = 600,
  compression = "lzw" # Uma boa compressão sem perdas
)
