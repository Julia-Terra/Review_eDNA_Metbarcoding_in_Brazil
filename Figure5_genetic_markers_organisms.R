#Figure 5 - Radar Plot 5 loci e organismos 
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(fmsb)
library(tibble)
library(scales)

# Carregar os dados
data <- read_excel("eDNA_BR_metadados.xlsx")

# Separar loci múltiplos
data_loci <- data %>%
  mutate(loci = strsplit(as.character(loci), ",\\s*"),
         type_organism = strsplit(as.character(type_organism), ",\\s*")) %>%
  unnest(loci) %>%
  unnest(type_organism) # <- Aqui separamos os organismos múltiplos também!

# Contagem de organismos e loci
count_table <- data_loci %>%
  filter(!is.na(loci), !is.na(type_organism)) %>%
  group_by(type_organism, loci) %>%
  summarise(count = n(), .groups = "drop")

# Selecionar top 5 organismos
top_organisms <- count_table %>%
  group_by(type_organism) %>%
  summarise(total = sum(count)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(type_organism)

# Selecionar top 5 loci
top_loci <- count_table %>%
  group_by(loci) %>%
  summarise(total = sum(count)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(loci)

# Filtrar para top 5
filtered_data <- count_table %>%
  filter(type_organism %in% top_organisms, loci %in% top_loci)

# Transformar para formato wide
data_wide <- filtered_data %>%
  pivot_wider(names_from = type_organism, values_from = count, values_fill = 0)

# Colocar loci como rownames
data_wide <- data_wide %>%
  column_to_rownames(var = "loci")

# Adicionar linhas de máximo e mínimo
max_value <- max(data_wide)
data_ready <- rbind(rep(max_value, ncol(data_wide)),
                    rep(0, ncol(data_wide)),
                    data_wide)

# Definir cores
colors_border <- rainbow(nrow(data_ready) - 2)
colors_fill <- alpha(colors_border, 0.3)

# Salvar PDF
pdf("radarplot_top5_loci_organismos.pdf", width = 10, height = 8)

radarchart(data_ready, axistype = 1,
           pcol = colors_border, pfcol = colors_fill, plwd = 2, plty = 1,
           cglcol = "black", cglty = 3, axislabcol = "black",
           caxislabels = seq(0, max_value, by = round(max_value / 4)), cglwd = 1.5,
           vlcex = 0.8)
legend(x = "bottomright", legend = rownames(data_ready)[-c(1,2)], title = "Genetic marker",
       horiz = FALSE, bty = "n", pch = 20, col = colors_border,
       text.col = "black", cex = 1, pt.cex = 1.5, title.cex = 1.2, inset = c(-0.2, 0.05))
dev.off()

######### Radar Plot Todos os locis e todo organismos ######
### 1. Carregar pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(fmsb)
library(tibble)
library(scales)

### 2. Ler os dados
data <- read_excel("eDNA_BR_metadados.xlsx")

### 3. Separar múltiplos loci e múltiplos organismos (separados por vírgula e espaço)
data_loci <- data %>%
  mutate(loci = strsplit(as.character(loci), ",\\s*"),
         type_organism = strsplit(as.character(type_organism), ",\\s*")) %>%
  unnest(loci) %>%
  unnest(type_organism)

### 4. Conferir valores únicos
unique(data_loci$loci)
unique(data_loci$type_organism)

### 5. Remover NAs
data_loci <- data_loci %>%
  select(loci, type_organism) %>%
  drop_na()

### 6. Contar frequência
data_loci_summary <- data_loci %>%
  group_by(loci, type_organism) %>%
  summarise(count = n(), .groups = "drop")

### 7. Transformar em formato wide
data_loci_wide <- data_loci_summary %>%
  pivot_wider(names_from = type_organism, values_from = count, values_fill = 0)

### 8. Transformar loci em rownames
data_loci_wide <- data_loci_wide %>%
  column_to_rownames(var = "loci")

### 9. Adicionar linhas de máximo e mínimo (para radar chart)
max_value <- max(data_loci_wide)
data_loci_ready <- rbind(rep(max_value, ncol(data_loci_wide)),
                         rep(0, ncol(data_loci_wide)),
                         data_loci_wide)

### 10. Cores dinâmicas
colors_border <- rainbow(nrow(data_loci_wide))
colors_fill <- rainbow(nrow(data_loci_wide))

### 11. Plot do Radar
radarchart(data_loci_ready, axistype = 1,
           pcol = colors_border, pfcol = scales::alpha(colors_fill, 0.2), plwd = 2, plty = 1,
           cglcol = "grey", cglty = 3, axislabcol = "black",
           caxislabels = seq(0, max_value, by = round(max_value / 4)), cglwd = 0.8,
           vlcex = 0.9)

legend(x = "bottomright", legend = rownames(data_loci_ready[-c(1, 2),]),
       horiz = FALSE, bty = "n", pch = 20, col = colors_fill,
       text.col = "black", cex = 0.8, pt.cex = 1.2)

#Salvar em tiff
ggsave(
  filename = "Figure5.tiff",
  plot = painel_lado_a_lado,
  device = "tiff",
  width = 18,
  units = "cm",
  dpi = 600,
  compression = "lzw" 
)
