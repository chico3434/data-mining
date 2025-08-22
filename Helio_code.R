# ------------------------------------------------------------
# Importando as bibliotecas necessárias
# ------------------------------------------------------------
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(glue)
library(forcats)

# Carregando a base de dados
loaded_objs <- load("bfd_2022.rdata")
df <- get(loaded_objs[1]) 

# Lista das variáveis depart/arrival para as informações climaticas ou meteorológicas
# que serão verificadas quanto a NAs e contagem de aeroportos
vars_to_check <- c(
  "depart_air_temperature",   #degrees Celsius 
  "depart_dew_point",         #degrees Celsius
  "depart_relative_humidity", #Percentage of relative humidity
  "depart_wind_direction", #degree, based on Wind Rose
  "depart_wind_speed",        #in knots,
  "depart_sky_coverage", #cat
  "depart_pressure", # Atmospheric pressure         
  "depart_visibility", #in miles
  "depart_apparent_temperature", #in degrees 
  "depart_wind_speed_scale",  #cat
  "depart_wind_direction_cat", #cat
  
  "arrival_air_temperature",  
  "arrival_dew_point",
  "arrival_relative_humidity",
  "arrival_wind_direction",
  "arrival_wind_speed",       
  "arrival_sky_coverage",
  "arrival_pressure",         
  "arrival_visibility",
  "arrival_apparent_temperature",
  "arrival_wind_speed_scale", 
  "arrival_wind_direction_cat"
)

# Identifica a coluna de aeroporto de partida ou chegada
# com base no prefixo (depart ou arrival)
find_airport_col <- function(prefix, data) {
  candidates <- if (prefix == "depart") {
    c("depart", "departure_airport", "origin", "orig_airport")
  } else {
    c("arrival", "arrival_airport", "dest", "dest_airport")
  }
  hit <- intersect(candidates, names(data))
  if (length(hit) == 0) {
    stop(glue("Nenhuma coluna de aeroporto encontrada para '{prefix}'"))
  }
  hit[1]
}

# Define e calcula a tabela de contagem de NAs por variável e aeroporto
# para cada variável na lista vars_to_check
calc_missing_tbl <- function(var_name, data) {
  prefix <- if (str_starts(var_name, "depart_")) "depart" else "arrival"
  airport_col <- find_airport_col(prefix, data)
  
  data %>% 
    group_by(code = .data[[airport_col]]) %>%
    summarise(missing_n = sum(is.na(.data[[var_name]])), .groups = "drop") %>%
    arrange(desc(missing_n)) %>%
    mutate(variable = var_name, target = prefix) %>%
    rename(airport_code = code)
}
# Calcula a tabela de contagem de NAs para cada variável
missing_list <- map(vars_to_check, calc_missing_tbl, data = df)

# Combina todas as tabelas de contagem de NAs em uma única tabela
top3_tbl <- map_dfr(missing_list, ~ slice_max(.x, missing_n, n = 3),
                    .id = "var_index") %>%
  select(variable, airport_code, missing_n)

# dicionário: variable que contém os aeroportos no top 3
top3_dict <- top3_tbl %>% split(.$variable) %>% map(~ .$airport_code)

# ranking dos aeroportos que mais aparecem no TOP3
airport_top5 <- top3_tbl %>%
  count(airport_code, name = "freq") %>%
  slice_max(freq, n = 5) %>%
  pull(airport_code)

# Função que calcula NAs e presentes por variável para um aeroporto
calc_missing_by_airport <- function(airport_code, prefix, data, vars) {
  airport_col <- find_airport_col(prefix, data)
  # só as variáveis do prefixo
  vars_prefix <- vars[str_starts(vars, paste0(prefix, "_"))]
  if (length(vars_prefix) == 0) return(tibble())
  
  # filtra registros daquele aeroporto
  df_air <- data %>% filter(.data[[airport_col]] == airport_code)
  if (nrow(df_air) == 0) return(tibble())
  
  # constrói tibble de contagens
  tbl <- tibble(variable = vars_prefix) %>%
    mutate(
      missing_n = map_int(variable, ~ sum(is.na(df_air[[.x]]))),
      present_n = map_int(variable, ~ sum(!is.na(df_air[[.x]])))
    ) %>%
    arrange(desc(missing_n)) %>%
    mutate(
      cum_missing = cumsum(missing_n),
      cum_pct     = cum_missing / sum(missing_n),
      airport     = airport_code,
      target      = prefix
    ) %>%
    mutate(variable = factor(variable, levels = variable))
  
  tbl
}

# Função para plotagem dos dados faltantes dos aeroportos com soma acumulada
plot_missing_by_airport <- function(tbl, top_n = 20) {
  tbl2 <- tbl %>% slice_head(n = min(top_n, nrow(tbl)))
  
  ggplot(tbl2, aes(y = variable)) +
    geom_col(aes(x = missing_n), fill = "steelblue") +
    geom_text(aes(x = missing_n + max(missing_n)*0.03, label = missing_n),
              hjust = 0, size = 3) +
    geom_line(aes(x = cum_missing, group = 1), color = "darkorange", size = 0.8) +
    geom_point(aes(x = cum_missing), color = "darkorange", size = 1.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(
      title    = glue("{tbl$airport[1]} – {tbl$target[1]}"),
      subtitle = glue("Total faltantes: {sum(tbl$missing_n)}"),
      x        = "Valores ausentes",
      y        = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.text.y       = element_text(size = 8),
      plot.title        = element_text(face = "bold")
    )
}

# Função que executa o fluxo de analise especifica dos aeroportos com maiores quantidades de NAs
run_one_airport <- function(airport_code, prefix, data, dict, top_n = 20) {
  # variáveis nas quais esse aeroporto aparece no top3
  all_vars <- names(dict)
  vars_ac  <- all_vars[map_lgl(dict, ~ airport_code %in% .x) &
                        str_starts(all_vars, paste0(prefix, "_"))]
  if (length(vars_ac) == 0) return(invisible(NULL))
  
  # calcula missing/present e acumulados
  tbl <- calc_missing_by_airport(airport_code, prefix, data, vars_ac)
  if (nrow(tbl) == 0) return(invisible(NULL))
  
  #! imprime proporções no console, importante para validação dos dados e ver quais possuem somente valores NA
  tbl %>% 
    rowwise() %>%
    mutate(pct_missing = round(100*missing_n/(missing_n + present_n), 1)) %>%
    ungroup() %>%
    mutate(msg = glue(
      "{airport_code} | {variable}: ",
      "{missing_n} faltantes, {present_n} presentes ",
      "({pct_missing}% faltantes)"
    )) %>%
    pull(msg) %>%
    walk(message)
  
  # Salva os graficos por cateria
  p <- plot_missing_by_airport(tbl, top_n)
  ggsave(
    filename = glue("figures/{airport_code}_{prefix}.png"),
    plot     = p, width = 8, height = 5, dpi = 300
  )
  
  invisible(tbl)
}

# 7. Cria pasta de figuras
if (!dir.exists("figures")) dir.create("figures")

# 8. Executa para top 5 aeroportos
walk(airport_top5, ~ run_one_airport(.x, "depart",  df, top3_dict))
walk(airport_top5, ~ run_one_airport(.x, "arrival", df, top3_dict))

message("Pronto! Proporções no console e gráficos em figures/")  
