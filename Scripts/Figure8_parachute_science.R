# Figure 8 - Parachute Science - Chord Diagram 
# Carregar pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(circlize)
library(RColorBrewer)
library(grid)
library(magick)

# === Ler e preparar os dados ===
dados <- read_excel("Modelo_Tabela_Analise_Parachute_Science.xlsx")
dados$Funding_Type <- gsub("Foreing", "Foreign", dados$Funding_Type)

# === Filtrar estudos 100% brasileiros ===
dados_filtrados <- dados %>%
  filter(!(First_Author_Country == "BRA" &
             Senio_Author_Country == "BRA" &
             Funding_Type == "Brazilian"))

# === Definir cores fixas para todos os valores ===

# Valores únicos (sem NA)
valores_unicos <- unique(c(
  na.omit(dados_filtrados$First_Author_Country),
  na.omit(dados_filtrados$Senio_Author_Country),
  na.omit(dados_filtrados$Funding_Type)
))

# Excluir BRA e Brazilian das cores automáticas
valores_outros <- setdiff(valores_unicos, c("BRA", "Brazilian"))

# Paleta colorida sem verde nem amarelo
paleta_base <- brewer.pal(12, "Paired") # Cores vibrantes, seguras
# Filtrar tons muito próximos de verde/amarelo se necessário
paleta_filtrada <- paleta_base[!paleta_base %in% c("#66C2A5", "#A6D854", "#FFD92F")]

# Gerar cores fixas
cores_outros <- setNames(colorRampPalette(paleta_filtrada)(length(valores_outros)), valores_outros)

# Cores fixas para BRA e Brazilian
cores_fixas <- c("BRA" = "#FFD700", "Brazilian" = "#228B22")

# Combinar
cores_gerais <- c(cores_fixas, cores_outros)

# === Função para gerar o gráfico com essas cores ===
plot_chord <- function(origem, funding, titulo = "Título") {
  fluxo <- tibble(Pais = origem, Funding_Type = funding) %>%
    filter(!is.na(Pais), !is.na(Funding_Type)) %>%
    filter(!(Pais == "BRA" & Funding_Type == "Brazilian")) %>%
    group_by(Pais, Funding_Type) %>%
    summarise(value = n(), .groups = "drop")
  
  setores <- unique(c(fluxo$Pais, fluxo$Funding_Type))
  cores <- cores_gerais[setores]
  
  circos.clear()
  circos.par(start.degree = 90,
             gap.after = rep(5, length(setores)),
             track.margin = c(0.01, 0.01),
             track.height = 0.1)
  
  chordDiagram(
    fluxo,
    grid.col = cores,
    transparency = 0.2,
    directional = 1,
    direction.type = c("arrows", "diffHeight"),
    annotationTrack = c("name", "grid"),
    annotationTrackHeight = c(0.04, 0.05),
    big.gap = 15,
    link.sort = TRUE,
    link.decreasing = FALSE
  )
  
  circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      circos.axis(h = "bottom",
                  major.at = seq(0, CELL_META$xlim[2], by = 5),
                  labels.cex = 0.9,
                  labels.niceFacing = TRUE,
                  major.tick.length = 0.3)
    },
    bg.border = NA
  )
  
  grid.text(titulo, x = unit(0.5, "npc"), y = unit(0.98, "npc"),
            just = c("center", "top"),
            gp = gpar(fontsize = 16, fontface = "bold"))
}

# === Gerar os dois gráficos com cores consistentes ===

# First Author
png("chord_first_author.png", width = 2000, height = 2000, res = 300)
plot_chord(
  origem = dados_filtrados$First_Author_Country,
  funding = dados_filtrados$Funding_Type,
  titulo = "First Author Country → Funding Source"
)
dev.off()

# Senior Author
png("chord_senior_author.png", width = 2000, height = 2000, res = 300)
plot_chord(
  origem = dados_filtrados$Senio_Author_Country,
  funding = dados_filtrados$Funding_Type,
  titulo = "Senior Author Country → Funding Source"
)
dev.off()

# === Juntar em um painel final ===
img1 <- image_read("chord_first_author.png")
img2 <- image_read("chord_senior_author.png")

painel_final <- image_append(c(img1, img2), stack = TRUE)

#Salvar em png
image_write(painel_final, "chord_duplo_autores_final_colorfix.png")

# Salvar em PDF
image_write(painel_final, "chord_duplo_autores_final_colorfix.pdf", format = "pdf")

#Salvar em tiff
ggsave(
  filename = "Figure8.tiff",
  plot = painel_lado_a_lado,
  device = "tiff",
  width = 18,
  units = "cm",
  dpi = 600,
  compression = "lzw" 
)
