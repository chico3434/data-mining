# ------------------------------------------------------------
# 0. Pacotes
# ------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(patchwork)  # Para juntar gráficos

# ------------------------------------------------------------
# 1. Diretório de trabalho e dados
# ------------------------------------------------------------
setwd("~/data-mining/T2")  # Ajuste conforme necessário
obj_name <- load("bfd_arrival_final.RData")
dados <- get(obj_name)

names(dados) # Verifica as colunas disponíveis

# ------------------------------------------------------------
# 2. Garante que 'clusters' é fator
# ------------------------------------------------------------
if ("clusters" %in% names(dados)) {
  dados <- dados %>% mutate(clusters = as.factor(clusters))
} else {
  stop("Coluna de agrupamento não encontrada. Verifique o nome no objeto 'dados'.")
}

# ------------------------------------------------------------
# 3. Tamanho de cada cluster
# ------------------------------------------------------------
contagem <- dados %>% count(clusters, name = "n_registros")
print(contagem)

grafico_tamanho <- ggplot(contagem, aes(x = clusters, y = n_registros, fill = clusters)) +
  geom_col() +
  labs(title = "Quantidade de registros por Cluster",
       x = "Cluster", y = "Número de registros", fill = "Cluster") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(grafico_tamanho) # Exibe o gráfico



# ------------------------------------------------------------
# 4. Estatísticas descritivas por cluster
# ------------------------------------------------------------

# Médias
resumo_media <- dados %>%
  group_by(clusters) %>%
  summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 2), .names = "{.col}_mean"),
            .groups = "drop")
print(resumo_media)

# Desvio-padrão
resumo_sd <- dados %>%
  group_by(clusters) %>%
  summarise(across(where(is.numeric), ~ round(sd(.x, na.rm = TRUE), 2), .names = "{.col}_sd"),
            .groups = "drop")
print(resumo_sd)

# Medianas
resumo_median <- dados %>%
  group_by(clusters) %>%
  summarise(across(where(is.numeric), ~ round(median(.x, na.rm = TRUE), 2), .names = "{.col}_median"),
            .groups = "drop")
print(resumo_median)

# ------------------------------------------------------------
# 5. Boxplots das variáveis numéricas por cluster
# ------------------------------------------------------------
variaveis_numericas <- names(dados)[sapply(dados, is.numeric)]
for (var in variaveis_numericas) {
  p <- ggplot(dados, aes(x = clusters, y = .data[[var]], fill = clusters)) +
    geom_boxplot() +
    labs(x = "Cluster", y = var, title = paste("Distribuição de", var, "por cluster")) +
    theme_minimal()
  print(p) # Exibe o boxplot
}

# ------------------------------------------------------------
# 6. Métricas agrupadas para heatmap
# ------------------------------------------------------------

vars_modelo <- c(
  "delay_arrival",
  "real_flight_length",
  "arrival_air_temperature",
  "arrival_dew_point",
  "arrival_relative_humidity",
  "arrival_wind_direction",
  "arrival_pressure",
  "arrival_visibility",
  "arrival_apparent_temperature"
)

calc_metricas <- function(df, func, sufixo) {
  df %>%
    group_by(clusters) %>%
    summarise(across(all_of(vars_modelo),
                     ~ round(func(.x, na.rm = TRUE), 2),
                     .names = "{.col}")) %>%
    pivot_longer(-clusters, names_to = "variavel", values_to = sufixo)
}

medias   <- calc_metricas(dados, mean,   "media")
medianas <- calc_metricas(dados, median, "mediana")
sds      <- calc_metricas(dados, sd,     "desvio_padrao")

perfil_clusters <- medias %>%
  left_join(medianas, by = c("clusters", "variavel")) %>%
  left_join(sds,      by = c("clusters", "variavel")) %>%
  arrange(variavel, clusters)

ordem_var <- perfil_clusters %>%
  group_by(variavel) %>%
  summarise(spread = max(media, na.rm = TRUE) - min(media, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(spread)) %>%
  pull(variavel)

pc_long <- perfil_clusters %>%
  mutate(
    variavel = factor(variavel, levels = ordem_var),
    clusters = as.factor(clusters)
  ) %>%
  pivot_longer(
    cols = c(media, mediana, desvio_padrao),
    names_to = "metrica", values_to = "valor"
  )

rotulos <- c(
  media         = "Média",
  mediana       = "Mediana",
  desvio_padrao = "Desvio Padrão"
)

plots <- lapply(names(rotulos), function(m) {
  df_plot <- pc_long %>% filter(metrica == m)
  p <- ggplot(df_plot, aes(x = clusters, y = variavel, fill = valor)) +
    geom_tile(color = "grey90") +
    geom_text(aes(label = format(round(valor, 2), nsmall = 2)),
              size = 3.5, color = "black", fontface = "bold") +
    scale_fill_viridis_c(option = "C", name = rotulos[m]) +
    labs(
      title = paste("Heatmap de", rotulos[m], "por variável e cluster"),
      x = "Cluster", y = "Variável"
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey85"),
      panel.grid.minor = element_blank()
    )
  print(p) # Exibe o heatmap
  return(p)
})
names(plots) <- names(rotulos)

# ------------------------------------------------------------
# 7. Perfil das variáveis categóricas por cluster
# ------------------------------------------------------------
categoricas <- c("arrival_sky_coverage",
                 "arrival_wind_speed_scale",
                 "arrival_wind_direction_cat")

perfil_cat <- dados %>%
  group_by(clusters) %>%
  summarise(across(all_of(categoricas),
                   ~ list(table(.x)), .names = "{.col}"))

print(perfil_cat)

# ------------------------------------------------------------
# 8. Interpretação sugerida para relatório
# ------------------------------------------------------------
# - Use o gráfico de contagem para contextualizar o tamanho dos grupos.
# - Destaque diferenças nas médias entre clusters para variáveis relevantes.
# - Comente sobre a homogeneidade/heterogeneidade interna usando o desvio-padrão.
# - Nos boxplots, aponte padrões visuais e outliers.
# - Descreva o "perfil" de cada cluster: tendência central + dispersão + categorias.