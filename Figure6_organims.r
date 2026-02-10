
###Script Novo

# === PACOTES ===
library(tidyverse)
library(readxl)
library(patchwork)
library(cowplot)

# === 1. LEITURA E PREPARAÇÃO DOS DADOS ===
df <- read_excel("eDNA_BR_metadados_verts_inverts.xlsx")

# --- Separar valores múltiplos (com vírgula) em linhas diferentes ---
df <- df %>%
  mutate(
    ecosystem = strsplit(as.character(ecosystem), ",\\s*"),
    type_organism = strsplit(as.character(type_organism), ",\\s*"),
    `vertebrates x invertebrates` = strsplit(as.character(`vertebrates x invertebrates`), ",\\s*")
  ) %>%
  unnest(cols = c(ecosystem, type_organism, `vertebrates x invertebrates`)) %>%
  mutate(
    ecosystem = str_to_lower(trimws(ecosystem)),
    ecosystem = recode(ecosystem,
                       "marine" = "Marine",
                       "freshwater" = "Freshwater",
                       "estuary" = "Estuary",
                       "terrestrial" = "Terrestrial"),
    type_organism = str_trim(type_organism),
    `vertebrates x invertebrates` = str_trim(`vertebrates x invertebrates`)
  )

# --- Paleta fixa ---
cores_ecossistema <- c(
  "Estuary" = "#8E44AD",      # roxo
  "Freshwater" = "#F39C12",   # laranja
  "Marine" = "#1DA1F2",       # azul
  "Terrestrial" = "#27AE60"   # verde
)

# === 2. GRÁFICO A (somente Vertebrates e Invertebrates) ===
df_a <- df %>%
  filter(`vertebrates x invertebrates` %in% c("Vertebrates", "Invertebrates")) %>%
  count(`vertebrates x invertebrates`, ecosystem)

graf_a <- ggplot(df_a, aes(x = `vertebrates x invertebrates`, y = n, fill = ecosystem)) +
  geom_col(position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 4,
            fontface = "plain") +
  scale_fill_manual(values = cores_ecossistema, name = NULL) +
  labs(x = NULL, y = "Number of studies") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    axis.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# === 3. GRÁFICO B (Grupos de organismos específicos) ===
df_b <- df %>%
  filter(!is.na(type_organism)) %>%
  count(type_organism, ecosystem) %>%
  mutate(type_organism = fct_reorder(type_organism, n, sum))

graf_b <- ggplot(df_b, aes(x = type_organism, y = n, fill = ecosystem)) +
  geom_col(position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3.5,
            fontface = "plain") +
  scale_fill_manual(values = cores_ecossistema, name = NULL) +
  labs(x = NULL, y = "Number of studies") +
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# === 4. COMBINAR OS DOIS LADO A LADO ===
painel_lado_a_lado <- graf_a + graf_b +
  plot_layout(ncol = 2, widths = c(1, 2), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.7, "cm"),
    legend.box = "horizontal"
  )

# === 5. ADICIONAR (a) e (b) ===
painel_rotulado <- ggdraw(painel_lado_a_lado) +
  draw_plot_label(
    label = c("(a)", "(b)"),
    x = c(0.02, 0.37),  # posição horizontal ajustável
    y = c(0.98, 0.98),
    size = 14,
    fontface = "plain"
  )

print(painel_rotulado)
# === 6. SALVAR EM ALTA QUALIDADE ===
ggsave("stacked_bar_ecosystem_verts_inverts_FINAL_VALUES.png",
       painel_rotulado, width = 13, height = 8, dpi = 600)
ggsave("stacked_bar_ecosystem_verts_inverts_FINAL_VALUES.pdf",
       painel_rotulado, width = 13, height = 8, dpi = 600)

ggsave(
  filename = "Figure3_final.tiff",
  plot = painel_rotulado,
  device = "tiff",
  width = 22,
  units = "cm",
  dpi = 600,
  compression = "lzw" # Uma boa compressão sem perdas
)

