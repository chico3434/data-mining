# ------------------------------------------------------------
# Bibliotecas
# ------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# Funções auxiliares para cálculos meteorológicos e psicrométricos
# ------------------------------------------------------------
sat_vapor_hPa <- function(Tc) 6.112 * exp((17.62 * Tc) / (243.12 + Tc))         # Pressão de vapor saturado
act_vapor_hPa <- function(Td) 6.112 * exp((17.62 * Td) / (243.12 + Td))         # Pressão de vapor atual
mixing_ratio <- function(e, p) 0.622 * e / (p - e)                             # Razão de mistura (kg/kg)
specific_humidity <- function(e, p) (0.622 * e) / (p - 0.378 * e)              # Umidade específica (kg/kg)
tw_stull <- function(Tc, RH){                                                  # Temperatura de bulbo úmido (Stull)
  atan <- base::atan
  Tc*atan(0.151977 * sqrt(RH + 8.313659)) +
    atan(Tc + RH) - atan(RH - 1.676331) +
    0.00391838 * RH^(3/2) * atan(0.023101 * RH) - 4.686035
}
wind_chill_c <- function(Tc, V_kmh){                                           # Sensação térmica por vento
  13.12 + 0.6215*Tc - 11.37*(V_kmh^0.16) + 0.3965*Tc*(V_kmh^0.16)
}
kt_to_kmh <- function(kt) kt * 1.852                                           # Conversão de nós para km/h

# ------------------------------------------------------------
# Carregando e filtrando dados
# ------------------------------------------------------------
loaded_objs <- load("bfd_2022.rdata")      # Carrega o arquivo .rdata
df <- get(loaded_objs[1])                  # Extrai o data.frame do ambiente

# Seleção das variáveis de interesse (apenas arrival neste exemplo)
vars_to_check <- c(
  # "depart_air_temperature",   #degrees Celsius 
  # "depart_dew_point",         #degrees Celsius
  # "depart_relative_humidity", #Percentage of relative humidity
  # "depart_wind_direction", #degree, based on Wind Rose
  # "depart_wind_speed",        #in knots,
  # "depart_sky_coverage", #cat
  # "depart_pressure", # Atmospheric pressure         
  # "depart_visibility", #in miles
  # "depart_apparent_temperature", #in degrees 
  # "depart_wind_speed_scale",  #cat
  # "depart_wind_direction_cat" #cat
  
  "arrival_air_temperature",   # Temperatura do ar na chegada
  "arrival_dew_point",         # Ponto de orvalho na chegada
  "arrival_relative_humidity", # Umidade relativa na chegada
  "arrival_wind_direction",    # Direção do vento na chegada
  "arrival_wind_speed",        # Velocidade do vento na chegada
  "arrival_sky_coverage",      # Cobertura do céu na chegada
  "arrival_pressure",          # Pressão atmosférica na chegada
  "arrival_visibility",        # Visibilidade na chegada
  "arrival_apparent_temperature", # Temperatura aparente na chegada
  "arrival_wind_speed_scale",  # Escala de velocidade do vento
  "arrival_wind_direction_cat" # Categoria de direção do vento
)

# ------------------------------------------------------------
# Frequência das variáveis categóricas de interesse
# ------------------------------------------------------------
cat_cols <- c("arrival_sky_coverage",
              "arrival_wind_speed_scale",
              "arrival_wind_direction_cat")

for(c in cat_cols){
  cat("\n---", c, "---\n")                # Imprime o nome da coluna
  print(table(df_clean[[c]], useNA = "ifany")) # Imprime a frequência dos valores
}

# ------------------------------------------------------------
# Filtrando linhas sem NAs nas variáveis selecionadas
# ------------------------------------------------------------
df_clean <- df %>%
  drop_na(all_of(vars_to_check))
  # ------------------------------------------------------------
  # Criando features psicrométricas e flags meteorológicas
  # ------------------------------------------------------------

  # Renomeando variáveis
  df_feat <- df_clean %>%
    mutate(
      T = arrival_air_temperature,
      Td = arrival_dew_point,
      RH = arrival_relative_humidity,
      P = arrival_pressure,               
      V_kt = arrival_wind_speed,
      V_kmh = kt_to_kmh(V_kt)
    )

  # Cálculos psicrométricos
  df_feat <- df_feat %>%
    mutate(
      dT = T - Td,                        # Diferença entre temperatura e ponto de orvalho
      es = sat_vapor_hPa(T),              # Pressão de vapor saturado
      ea = act_vapor_hPa(Td),             # Pressão de vapor atual
      VPD = pmax(es - ea, 0),             # Déficit de pressão de vapor
      w_mixing = mixing_ratio(ea, P),     # Razão de mistura
      q_specific = specific_humidity(ea, P), # Umidade específica
      Tw = tw_stull(T, RH)                # Temperatura de bulbo úmido
    )

  # Termo-sensações
  df_feat <- df_feat %>%
    mutate(
      wind_chill = ifelse(T <= 10 & V_kmh >= 4.8, wind_chill_c(T, V_kmh), NA_real_), # Sensação térmica por vento
      heat_stress = ifelse(!is.na(arrival_apparent_temperature),
                           arrival_apparent_temperature - T, NA_real_)               # Estresse térmico
    )

  # Céu em okta aproximado
  df_feat <- df_feat %>%
    mutate(
      sky_okta = case_when(
        arrival_sky_coverage %in% c("NCD","NSC") ~ 0,  # céu limpo / nenhuma nuvem significativa
        arrival_sky_coverage == "FEW" ~ 2,            # poucas nuvens
        arrival_sky_coverage == "SCT" ~ 4,            # nuvens espalhadas (~4/8)
        arrival_sky_coverage == "BKN" ~ 6,            # muito nublado (~6-7/8)
        arrival_sky_coverage == "OVC" ~ 8,            # encoberto (8/8)
        arrival_sky_coverage == "VV"  ~ 10,            # teto indefinido, geralmente devido a nevoeiro/precipitação
        TRUE ~ NA_real_
      )
    )

  # Flags operacionais
  df_feat <- df_feat %>%
    mutate(
      low_vis_flag = arrival_visibility < 2,          # Baixa visibilidade
      fog_flag = arrival_visibility < 1 & dT <= 2,    # Possível nevoeiro
      overcast_flag = arrival_sky_coverage %in% c("BKN","OVC","VV"), # Céu encoberto
      high_RH_flag = RH >= 90,                        # Alta umidade relativa
      low_pressure_flag = P < 1000,                   # Baixa pressão atmosférica
      high_wind_flag = V_kt > 20,                     # Vento forte
      precip_likely_flag = (high_RH_flag & overcast_flag) | VPD <= 5 | dT <= 2 # Probabilidade de precipitação
    )

  # Flag de atraso
  df_feat <- df_feat %>%
    mutate(
      delayed_15m = delay_arrival > 15                # Flag de atraso superior a 15 minutos
    )

# ------------------------------------------------------------
# Correlação Spearman entre variáveis contínuas e atraso
# ------------------------------------------------------------
num_feats <- c("dT","VPD","w_mixing","q_specific","Tw","wind_chill","heat_stress",
               "sky_okta","arrival_visibility","arrival_wind_speed","arrival_pressure","RH")

corr_tbl <- lapply(num_feats, function(v){
  x <- df_feat[[v]]; y <- df_feat$delay_arrival
  ok <- is.finite(x) & is.finite(y)
  if(sum(ok) >= 10){
    ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman"))
    data.frame(feature = v, rho = unname(ct$estimate), p_value = ct$p.value, n = sum(ok))
  } else {
    data.frame(feature = v, rho = NA_real_, p_value = NA_real_, n = sum(ok))
  }
}) %>% bind_rows() %>% arrange(p_value)

print(corr_tbl) # Mostra tabela de correlações

# ------------------------------------------------------------
# Associação entre flags meteorológicas e atraso binário (>15 min)
# ------------------------------------------------------------
flag_feats <- c("low_vis_flag","fog_flag","overcast_flag","high_RH_flag",
                "low_pressure_flag","high_wind_flag","precip_likely_flag")

rate_tbl <- flag_feats %>%
  lapply(function(f){
    df_feat %>%
      group_by(.data[[f]]) %>%
      summarise(
        n = n(),
        delay_rate = mean(delayed_15m, na.rm = TRUE),
        mean_delay = mean(delay_arrival, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(flag = f)
  }) %>% bind_rows()

print(rate_tbl) # Mostra taxa de atraso por flag

# Teste qui-quadrado para associação entre flag e atraso
chi_tbl <- lapply(flag_feats, function(f){
  tab <- table(df_feat[[f]], df_feat$delayed_15m)
  if(all(dim(tab) > 1)){
    ct <- suppressWarnings(chisq.test(tab))
    data.frame(flag = f, chi2 = unname(ct$statistic), p_value = ct$p.value)
  } else {
    data.frame(flag = f, chi2 = NA_real_, p_value = NA_real_)
  }
}) %>% bind_rows()

print(chi_tbl) # Mostra resultado do teste qui-quadrado

# ------------------------------------------------------------
# Visualizações dos resultados
# ------------------------------------------------------------

# Gráfico das correlações mais significativas
corr_tbl %>%
  slice_min(p_value, n = 8) %>%
  ggplot(aes(x = reorder(feature, rho), y = rho, fill = rho > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Feature", y = "Spearman rho", title = "Correlação com atraso de chegada")

# Gráfico da taxa de atraso por flag meteorológica
for(f in flag_feats){
  p <- df_feat %>%
    group_by(val = .data[[f]]) %>%
    summarise(delay_rate = mean(delayed_15m, na.rm = TRUE),
              n = n(), .groups = "drop") %>%
    ggplot(aes(x = as.factor(val), y = delay_rate, fill = as.factor(val))) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = scales::percent(delay_rate, accuracy = 1)), vjust = -0.3) +
    labs(x = f, y = "Taxa de atraso (>15m)", title = paste("Atrasos por", f)) +
    ylim(0, 1)
  print(p)
}

# Função principal: calcula escore, nível e flag de chuva provável
chuva_prob_flag <- function(df,
                            cols = list(dT = "dT", RH = "RH", sky = "sky_okta", P = "P", V = "V_kt"),
                            thr = list(
                              dT = c(1, 2, 3),          # cortes para dT (°C)
                              RH = c(98, 95),           # cortes para RH (%)
                              sky = c(8, 6),            # 8 -> forte, 6 -> moderado
                              P   = c(1000, 1005),      # hPa (quanto MENOR, pior)
                              V   = 20                  # nós
                            ),
                            w = list(
                              sat_dT = c(3, 2, 1, 0),   # pontos por faixas dT (≤1, ≤2, ≤3, >3)
                              sat_RH = c(2, 1, 0),      # pontos por faixas RH (≥98, ≥95, <95)
                              sky    = c(2, 1, 0),      # pontos por faixas sky (≥8, ≥6, <6)
                              P      = c(2, 1, 0),      # pontos por faixas P (≤1000, ≤1005, >1005)
                              Vbonus = 1                # bônus se V ≥ thr$V e sky ≥ 6
                            ),
                            score_cut = list(high = 6, flag = 5, moderate = 4)) {

  # Extrai vetores
  dT  <- df[[cols$dT]]
  RH  <- df[[cols$RH]]
  sky <- df[[cols$sky]]
  P   <- df[[cols$P]]
  V   <- df[[cols$V]]

  n <- nrow(df)

  # Pontos por dT (menor dT => mais pontos)
  s_dT <- ifelse(!is.na(dT) & dT <= thr$dT[1], w$sat_dT[1],
          ifelse(!is.na(dT) & dT <= thr$dT[2], w$sat_dT[2],
          ifelse(!is.na(dT) & dT <= thr$dT[3], w$sat_dT[3], w$sat_dT[4])))

  # Pontos por RH (maior RH => mais pontos)
  s_RH <- ifelse(!is.na(RH) & RH >= thr$RH[1], w$sat_RH[1],
          ifelse(!is.na(RH) & RH >= thr$RH[2], w$sat_RH[2], w$sat_RH[3]))

  # Pontos por céu (maior okta => mais pontos)
  s_sky <- ifelse(!is.na(sky) & sky >= thr$sky[1], w$sky[1],
           ifelse(!is.na(sky) & sky >= thr$sky[2], w$sky[2], w$sky[3]))

  # Pontos por pressão (menor P => mais pontos)
  s_P <- ifelse(!is.na(P) & P <= thr$P[1], w$P[1],
         ifelse(!is.na(P) & P <= thr$P[2], w$P[2], w$P[3]))

  # Bônus por vento com céu carregado (opcional, fraco)
  s_V <- ifelse(!is.na(V) & !is.na(sky) & V >= thr$V & sky >= thr$sky[2], w$Vbonus, 0)

  # Escore total
  score <- s_dT + s_RH + s_sky + s_P + s_V

  # Nível qualitativo
  level <- ifelse(score >= score_cut$high, "alta",
           ifelse(score >= score_cut$moderate, "moderada", "baixa"))

  # Flag lógico
  flag <- score >= score_cut$flag

  # Razões resumidas (texto curto)
  reason <- paste0(
    ifelse(s_dT > 0 | s_RH > 0, "saturacao;", ""),
    ifelse(s_sky > 0, "ceu_carregado;", ""),
    ifelse(s_P > 0, "pressao_baixa;", ""),
    ifelse(s_V > 0, "vento_bonus;", "")
  )
  reason[reason == ""] <- NA_character_

  # Retorno como data.frame (para fazer bind_cols no pipeline)
  data.frame(
    chuva_score = score,
    chuva_level = level,
    chuva_prob_flag = flag,
    pts_dT = s_dT,
    pts_RH = s_RH,
    pts_sky = s_sky,
    pts_P = s_P,
    pts_V = s_V,
    chuva_reason = reason,
    stringsAsFactors = FALSE
  )
}

# Supondo que df_feat já tem as colunas: dT, RH, sky_okta, P, V_kt
res <- chuva_prob_flag(df_feat)

# Anexa as colunas ao seu data.frame
df_feat <- dplyr::bind_cols(df_feat, res)

# Exemplo: taxa de atraso quando chuva_prob_flag é TRUE
df_feat %>%
  dplyr::group_by(chuva_prob_flag) %>%
  dplyr::summarise(
    n = dplyr::n(),
    delay_rate = mean(delayed_15m, na.rm = TRUE),
    mean_delay = mean(delay_arrival, na.rm = TRUE),
    .groups = "drop"
  )

chuva_prob_rule <- function(dT, RH, sky_okta, P){
  cond_sat  <- (dT <= 2 & RH >= 95) | (dT <= 1) | (RH >= 98)
  cond_sky  <- sky_okta >= 6
  cond_pres <- P <= 1000
  # Chuva provável se saturação + (céu carregado ou pressão baixa)
  (cond_sat & (cond_sky | cond_pres)) |
  # Ou caso extremo de teto encoberto total
  (sky_okta >= 8 & RH >= 95)
}

df_feat$chuva_prob_flag_simple <- with(df_feat, chuva_prob_rule(dT, RH, sky_okta, P))
