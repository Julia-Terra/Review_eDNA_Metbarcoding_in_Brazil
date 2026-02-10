#Gráfico 1

# Pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork) # para combinar os gráficos

# Leitura da planilha
df <- read_excel("eDNA_BR_metadados_verts_inverts.xlsx")

# ---------------------
# Gráfico de BARRAS (time series)
# ---------------------
ts_data <- df %>%
  count(year) %>%
  filter(!is.na(year)) %>%
  arrange(year)

plot_ts <- ggplot(ts_data, aes(x = as.factor(year), y = n)) +
  geom_col(fill = "gray40") +
  theme_minimal() +
  labs(x = "Year", y = "Number of Studies") +
  
  # <<< CORREÇÃO AQUI
  # Deixa o ggplot calcular o limite superior (NA)
  # Mas força o inferior a ser 0 e remove o espaço de expansão inferior.
  scale_y_continuous(
    limits = c(0, NA), 
    expand = expansion(mult = c(0, 0.1)) # 0% embaixo, 10% em cima
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),     
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ---------------------
# Gráfico de barras por ecossistema e técnica
# ---------------------
bar_data <- df %>%
  filter(!is.na(ecosystem), !is.na(`eDNA x metabarcoding`)) %>%
  count(ecosystem, `eDNA x metabarcoding`) %>%
  rename(technique = `eDNA x metabarcoding`)

plot_bar <- ggplot(bar_data, aes(x = ecosystem, y = n, fill = technique)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n), vjust = -0.5, 
            position = position_dodge(width = 0.9), size = 3) +
  
  # <<< CORREÇÃO AQUI
  # Aplica EXATAMENTE as mesmas regras do gráfico da esquerda
  # O ggplot vai calcular o limite superior (NA) para incluir o geom_text
  # E vamos forçar o limite inferior (0) e a expansão.
  scale_y_continuous(
    limits = c(0, NA), 
    expand = expansion(mult = c(0, 0.1)) # 0% embaixo, 10% em cima
  ) +
  
  labs(x = "Ecosystem", y = NULL, fill = "Technique") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()  
  )

# ---------------------
# Combina os dois gráficos lado a lado
# ---------------------
# Corrigindo também a combinação - você estava sobrescrevendo o layout
combined_plot <- plot_ts + plot_bar + 
  plot_layout(ncol = 2) + # Define 2 colunas
  plot_annotation(tag_levels = "a", 
                  tag_prefix = '(', 
                  tag_suffix = ')') # Adiciona (a) e (b)

# Exibe
print(combined_plot)

# Salvar pdf 
ggsave("grafico_1.pdf", plot = combined_plot, width = 8, height = 6, units = "in", dpi = 300)

# Salvar png
ggsave("grafico_1.png", plot = combined_plot, width = 8, height = 6, units = "in", dpi = 600)

# Salvar o gráfico p como TIFF, com 18cm de largura, 600 dpi
ggsave(
  filename = "grafico1.tiff",
  plot = combined_plot,
  device = "tiff",
  width = 22,
  units = "cm",
  dpi = 600,
  compression = "lzw" # Uma boa compressão sem perdas
)
