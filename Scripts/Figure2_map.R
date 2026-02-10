# Figure 2 - Mapa do Brasil - Estudos por estado

# Carregando o shapefile dos estados brasileiros
br_map <- geobr::read_state()

data_estado <- data %>%
  mutate(Estado = strsplit(as.character(.[[22]]), ", ")) %>%
  unnest(Estado) %>%
  drop_na(Estado)

# Contagem de estudos por estado
map_estado <- data_estado %>%
  group_by(Estado) %>%
  summarise(count = n())

# Ajustando nomes para coincidir com o shapefile
map_estado$Estado <- case_when(
  map_estado$Estado == "Acre" ~ "AC",
  map_estado$Estado == "Alagoas" ~ "AL",
  map_estado$Estado == "Amapá" ~ "AP",
  map_estado$Estado == "Amazonas" ~ "AM",
  map_estado$Estado == "Bahia" ~ "BA",
  map_estado$Estado == "Ceará" ~ "CE",
  map_estado$Estado == "Distrito Federal" ~ "DF",
  map_estado$Estado == "Espírito Santo" ~ "ES",
  map_estado$Estado == "Goiás" ~ "GO",
  map_estado$Estado == "Maranhão" ~ "MA",
  map_estado$Estado == "Mato Grosso" ~ "MT",
  map_estado$Estado == "Mato Grosso do Sul" ~ "MS",
  map_estado$Estado == "Minas Gerais" ~ "MG",
  map_estado$Estado == "Pará" ~ "PA",
  map_estado$Estado == "Paraíba" ~ "PB",
  map_estado$Estado == "Paraná" ~ "PR",
  map_estado$Estado == "Pernambuco" ~ "PE",
  map_estado$Estado == "Piauí" ~ "PI",
  map_estado$Estado == "Rio de Janeiro" ~ "RJ",
  map_estado$Estado == "Rio Grande do Norte" ~ "RN",
  map_estado$Estado == "Rio Grande do Sul" ~ "RS",
  map_estado$Estado == "Rondônia" ~ "RO",
  map_estado$Estado == "Roraima" ~ "RR",
  map_estado$Estado == "Santa Catarina" ~ "SC",
  map_estado$Estado == "São Paulo" ~ "SP",
  map_estado$Estado == "Sergipe" ~ "SE",
  map_estado$Estado == "Tocantins" ~ "TO",
  TRUE ~ Estado
)

# Mesclando com o shapefile
map_estado <- br_map %>%
  left_join(map_estado, by = c("abbrev_state" = "Estado"))

# Criando o mapa
map_2b <- ggplot(map_estado) +
  geom_sf(aes(fill = count), color = "black", size = 0.25) +
  scale_fill_viridis_c("Estudos por estado", alpha = 0.7, option="mako", direction = -1, na.value = "white") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2))

#Salvar em pdf
ggsave('map_2b_estados.pdf', map_2b, width = 21, height = 13, units = 'cm', dpi = 600)

#Salvar em tiff
ggsave(
  filename = "Figure2.tiff",
  plot = painel_lado_a_lado,
  device = "tiff",
  width = 18,
  units = "cm",
  dpi = 600,
  compression = "lzw" 
)
