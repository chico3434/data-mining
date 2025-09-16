#! Ideias
# Interações e cruzamentos de variáveis

# Rota × companhia → pode indicar hubs estratégicos.

# Status × clima → atrasos explicados por meteorologia.

# Hora × período do dia → efeitos sazonais de operação
library(daltoolbox) 
library(dplyr)
library(lubridate)
library(forcats)    
#install.packages("fastDummies")
library(fastDummies) # para one-hot encoding
library(caret)

# Função para classificar tipo de coluna
classificar_tipo <- function(x) {
  if (is.numeric(x)) {
    return("numérica")
  } else {
    return("categórica")
  }
}

# Função para calcular completude
calcular_completude <- function(x) {
  total <- length(x)
  preenchidos <- sum(!is.na(x))
  nulos <- sum(is.na(x))
  data.frame(
    Preenchidos = preenchidos,
    Nulos = nulos,
    Perc_Preenchido = round(preenchidos / total * 100, 2),
    Perc_Nulo = round(nulos / total * 100, 2)
  )
}

filtrar_por_frequencia <- function(df, coluna) {
  
  # Calcula frequência absoluta por valor
  freq_tbl <- df %>%
    count(.data[[coluna]], name = "freq")
  
  # Define limiar conforme a coluna
  if (coluna %in% c("depart", "company")) {
    limiar <- 365
  } else if (coluna == "delay_depart") {
    limiar_calc <- mean(freq_tbl$freq) - sd(freq_tbl$freq)
    limiar <- max(limiar_calc, -120)  # garante que não seja menor que -120
  } else {
    # comportamento padrão para outras colunas
    limiar <- mean(freq_tbl$freq) - sd(freq_tbl$freq)
  }
  
  # Filtra valores que atendem ao critério
  valores_validos <- freq_tbl %>%
    filter(freq >= limiar) %>%
    pull(.data[[coluna]])
  
  # Retorna dataframe filtrado
  df %>% filter(.data[[coluna]] %in% valores_validos)
}

# Função auxiliar para treinar e avaliar
avaliar_modelo <- function(model, nome_modelo) {
  train_pred <- predict(model, voos_gru_train)
  train_true <- adjust_class_label(voos_gru_train[, target])
  train_eval <- evaluate(model, train_true, train_pred)$metrics
  train_eval <- train_eval[, c("accuracy", "precision", "recall", "f1")]
  train_eval <- cbind(dataset = paste0(nome_modelo, "_treino"), train_eval)
  
  test_pred <- predict(model, voos_gru_test)
  test_true <- adjust_class_label(voos_gru_test[, target])
  test_eval <- evaluate(model, test_true, test_pred)$metrics
  test_eval <- test_eval[, c("accuracy", "precision", "recall", "f1")]
  test_eval <- cbind(dataset = paste0(nome_modelo, "_teste"), test_eval)
  
  rbind(train_eval, test_eval)
}

## summary para ter um overview dos atributos da base
obj_name <- load("bfd_2022.rdata")
voos_gru_raw <- get(obj_name)

#Detalhando as colunas
cat(names(voos_gru_raw), sep = "\n")

# Aplica a função a todas as colunas e imprime
tipos_colunas <- sapply(voos_gru_raw, classificar_tipo)

# Exibe no formato "coluna: tipo"
for (col in names(tipos_colunas)) {
  cat(col, ":", tipos_colunas[[col]], "\n")
}
# Filtra apenas Guarulhos, mantendo todas as colunas
voos_gru <- voos_gru_raw[voos_gru_raw$arrival == "SBGR" | voos_gru_raw$depart == "SBGR", ]


colunas_categoricas_arrival <- c("arrival", "flight", "di", "type",
                                 "arrival_day_period", "expected_arrival", "real_arrival",
                                 "status_arrival", 
                                 "arrival_sky_coverage", "arrival_wind_speed_scale",
                                 "arrival_wind_direction_cat")



colunas_numericas_arrival <- c("delay_arrival", "real_flight_length",
                               "arrival_air_temperature", "arrival_dew_point", "arrival_relative_humidity",
                               "arrival_wind_direction", "arrival_wind_speed", "arrival_pressure",
                               "arrival_visibility", "arrival_apparent_temperature")

colunas_categoricas_depart <- c("depart", "flight", "di", "type",
                                "depart_day_period", "expected_depart", "real_depart",
                                "status_depart",
                                "depart_sky_coverage", "depart_wind_speed_scale",
                                "depart_wind_direction_cat")

colunas_numericas_depart <- c("delay_depart", "expected_flight_length",
                              "depart_air_temperature", "depart_dew_point", "depart_relative_humidity",
                              "depart_wind_direction", "depart_wind_speed", "depart_pressure",
                              "depart_visibility", "depart_apparent_temperature")

colunas_remover <- c(colunas_categoricas_arrival, colunas_numericas_arrival)

#Filtrando somente para depart
voos_gru <- voos_gru[ , !(names(voos_gru) %in% colunas_remover)]

colunas_numericas <- c(colunas_numericas_depart) #, colunas_numericas_depart)
colunas_categoricas <- c(colunas_categoricas_depart) #,  colunas_categoricas_depart)

# 1. Criar colunas_inspecionar antes de usar
colunas_inspecionar <- c(colunas_numericas, colunas_categoricas)

# 2. Filtrar apenas colunas que existem
colunas_validas <- intersect(colunas_inspecionar, names(voos_gru))

# 3. Calcular completude só se houver colunas válidas
if (length(colunas_validas) > 0) {
  diagnostico <- lapply(voos_gru[colunas_validas], calcular_completude)
  diagnostico_df <- do.call(rbind, diagnostico)
  diagnostico_df <- cbind(Coluna = rownames(diagnostico_df), diagnostico_df)
  rownames(diagnostico_df) <- NULL
  diagnostico_df <- diagnostico_df[order(-diagnostico_df$Perc_Nulo), ]
  print(diagnostico_df)
} else {
  message("Nenhuma coluna válida encontrada para diagnóstico.")
}

#Tratamentos
# Remove colunas não importantes
colunas_remover <- c(
  "route", 
  "type", # type or company
  "outlier_depart_delay", 
  "outlier_depart_delay",
  "outlier_expected_flight_consistency", 
  "outlier_real_flight_consistency",
  "outlier_expected_flight_length", 
  "outlier_real_flight_length",
  "observation",
  "depart_sky_coverage" #Muitos valores como nulos
)
colunas_remover <- c(colunas_remover)

#Filtrando somente para depart
voos_gru <- voos_gru[ , !(names(voos_gru) %in% colunas_remover)]

# Remove linhas com NA
voos_gru <- na.omit(voos_gru)

# Lista de colunas categóricas de chegada
especificas_colunas_categoricas_depart <- c(
  "depart", "company",
  "depart_day_period", 
  "status_depart", 
  "depart_sky_coverage", "depart_wind_speed_scale",
  "depart_wind_direction_cat"
)

# Filtra apenas colunas que existem no dataframe
colunas_validas <- intersect(especificas_colunas_categoricas_depart, names(voos_gru))

# Loop para gerar contagem e frequência relativa
for (col in colunas_validas) {
  cat("\n📊 Distribuição de valores em:", col, "\n")
  
  voos_gru %>%
    group_by(.data[[col]]) %>%
    summarise(
      Contagem = n(),
      Frequencia = round((n() / nrow(voos_gru)) * 100, 2)
    ) %>%
    arrange(desc(Contagem)) %>%
    print(n = Inf) # mostra todos os valores
}

# Aplica para company
voos_gru <- filtrar_por_frequencia(voos_gru, "company")

# Aplica para depart
voos_gru <- filtrar_por_frequencia(voos_gru, "depart")

#Aplica para o status delay_depart
# voos_gru <- filtrar_por_frequencia(voos_gru, "delay_depart")

# Loop para gerar contagem e frequência relativa
for (col in colunas_validas) {
  cat("\n📊 Distribuição de valores em:", col, "\n")
  
  voos_gru %>%
    group_by(.data[[col]]) %>%
    summarise(
      Contagem = n(),
      Frequencia = round((n() / nrow(voos_gru)) * 100, 2)
    ) %>%
    arrange(desc(Contagem)) %>%
    print(n = Inf) # mostra todos os valores
}

#Modificcações das featuress:
voos_gru <- voos_gru %>%
  mutate(
    # 1️⃣ status_depart → agrupar atrasos em "Atraso"
    status_depart = case_when(
      status_depart %in% c("Atraso 30-60", "Atraso 60-120", "Atraso 120-240", "Atraso > 240") ~ "Atraso",
      TRUE ~ status_depart
    ),
    
    # 2️⃣ depart_wind_speed_scale → agrupar ventos fortes em "Gale"
    depart_wind_speed_scale = case_when(
      depart_wind_speed_scale %in% c("Near Gale", "Strong Gale", "Storm", "Violent Storm") ~ "Strong Breeze",
      TRUE ~ depart_wind_speed_scale
    ),
    
    # 3️⃣ depart_day_period → reorganizar em Morning, Afternoon, Night
    depart_day_period = case_when(
      depart_day_period %in% c("Early Morning", "Late Morning", "Mid Morning") ~ "Morning",
      depart_day_period %in% c("Afternoon", "Early Evening") ~ "Afternoon",
      depart_day_period %in% c("Night", "Late Evening") ~ "Night",
      TRUE ~ depart_day_period
    )
  )

#Criação das features
voos_gru <- voos_gru %>%
  mutate(
    # --- Expected Depart ---
    exp_depart_month = month(expected_depart, label = TRUE, abbr = TRUE),
    exp_depart_wday  = wday(expected_depart, label = TRUE, abbr = TRUE),
    
    # --- Real Depart ---
    real_depart_month = month(real_depart, label = TRUE, abbr = TRUE),
    real_depart_wday  = wday(real_depart, label = TRUE, abbr = TRUE),
    
  )

novas_colunas <- c("exp_depart_month", "exp_depart_wday",
                   "real_depart_month", "real_depart_wday")

# Filtra apenas colunas que existem no dataframe
colunas_validas <- intersect(
  union(especificas_colunas_categoricas_depart, novas_colunas),
  names(voos_gru)
)
# Loop para gerar contagem e frequência relativa
for (col in colunas_validas) {
  cat("\n📊 Distribuição de valores em:", col, "\n")
  
  voos_gru %>%
    group_by(.data[[col]]) %>%
    summarise(
      Contagem = n(),
      Frequencia = round((n() / nrow(voos_gru)) * 100, 2)
    ) %>%
    arrange(desc(Contagem)) %>%
    print(n = Inf) # mostra todos os valores
}

# -------------------------
# 1️⃣ Label Encoding
# -------------------------
colunas_label <- c(
  "depart_wind_direction_cat", 
  "exp_depart_month", "exp_depart_wday", 
  "real_depart_month", "real_depart_wday", 
  "depart_day_period", "status_depart"
)

# Aplica Label Encoding apenas nas colunas que existem no dataframe
colunas_label_validas <- intersect(colunas_label, names(voos_gru))

voos_gru <- voos_gru %>%
  mutate(across(all_of(colunas_label_validas), 
                ~ as.integer(factor(.x)), 
                .names = "{.col}_label"))

# -------------------------
# 2️⃣ One-Hot Encoding
# -------------------------
colunas_onehot <- c("depart", "company")

colunas_onehot_validas <- intersect(colunas_onehot, names(voos_gru))

voos_gru <- fastDummies::dummy_cols(
  voos_gru,
  select_columns = colunas_onehot_validas,
  remove_first_dummy = FALSE, # mantém todas as categorias
  remove_selected_columns = TRUE # remove coluna original
)

#Déficit de Temperatura do Ponto de Orvalho
voos_gru <- voos_gru %>%
  mutate(
    depart_dewpoint_dep = depart_air_temperature - depart_dew_point,
  )

#! Índice Combinado de Probabilidade de Chuva
#voos_gru <- voos_gru %>%
#  mutate(
#    depart_rain_index = 
#      (1 - depart_dewpoint_dep / max(depart_dewpoint_dep, na.rm = TRUE)) * 0.4 +
#      (depart_relative_humidity / 100) * 0.3 +
#      depart_sky_cover_num * 0.2 +
#      (1 - scale(depart_pressure)[,1]) * 0.1
#  )

colunas_remover <- c(
  "expected_depart", 
  "real_depart"
)
colunas_remover <- c(colunas_remover)

#Filtrando somente para depart
voos_gru <- voos_gru[ , !(names(voos_gru) %in% colunas_remover)]

#Reorganizar depois de avaliar os modelos
# 6. Normalização Min–Max
# Pré-processamento para normalizar (min-max)
pp <- preProcess(voos_gru, method = c("range"))

# Aplica a transformação
voos_gru_norm <- predict(pp, voos_gru)

# # 7. Detecção de outliers (IQR)
# detectar_outliers_iqr <- function(x) {
#   Q1 <- quantile(x, 0.25, na.rm = TRUE)
#   Q3 <- quantile(x, 0.75, na.rm = TRUE)
#   IQR <- Q3 - Q1
#   x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)
# }

# outliers_df <- sapply(voos_gru_norm[colunas_numericas], detectar_outliers_iqr)

# # 8. Resumo de outliers por variável
# resumo_outliers <- data.frame(
#   Variavel = colunas_numericas,
#   Qtde_Outliers = colSums(outliers_df, na.rm = TRUE)
# )



#! Modelos de Classificação
# variável alvo
target <- "status_depart"
voos_gru[[target]] <- as.factor(voos_gru[[target]])

# remover colunas não usadas
features <- setdiff(names(voos_gru), c("arrival", "depart"))

# split treino/teste
set.seed(123)
sr <- sample_random()
sr <- train_test(sr, voos_gru[, features])

voos_gru_train <- sr$train
voos_gru_test  <- sr$test

# Garantir colunas consistentes entre treino e teste
common_cols <- intersect(names(voos_gru_train), names(voos_gru_test))
voos_gru_train <- voos_gru_train[, common_cols]
voos_gru_test  <- voos_gru_test[, common_cols]

# níveis da variável alvo
slevels <- levels(voos_gru_train[[target]])

# 1. Garantir que a variável alvo é fator com os níveis corretos
voos_gru_train[[target]] <- factor(voos_gru_train[[target]], levels = slevels)
voos_gru_test[[target]]  <- factor(voos_gru_test[[target]],  levels = slevels)

# 2. Remover linhas com NA, NaN ou Inf
voos_gru_train <- voos_gru_train[complete.cases(voos_gru_train), ]
voos_gru_test  <- voos_gru_test[complete.cases(voos_gru_test), ]

# 3. Converter colunas categóricas (exceto a variável alvo) para numérico
for (col in setdiff(names(voos_gru_train), target)) {
  if (is.factor(voos_gru_train[[col]]) || is.character(voos_gru_train[[col]])) {
    voos_gru_train[[col]] <- as.numeric(as.factor(voos_gru_train[[col]]))
    voos_gru_test[[col]]  <- as.numeric(as.factor(voos_gru_test[[col]]))
  }
}

# 4. Garantir que não há colunas constantes (evita divisão por zero na normalização)
variancia <- sapply(voos_gru_train[, setdiff(names(voos_gru_train), target)], var, na.rm = TRUE)
col_const <- names(variancia[variancia == 0])
if (length(col_const) > 0) {
  voos_gru_train <- voos_gru_train[, !(names(voos_gru_train) %in% col_const)]
  voos_gru_test  <- voos_gru_test[,  !(names(voos_gru_test) %in% col_const)]
}



# Modelos
# -------------------------
# Naive Bayes (sem grid)
# -------------------------
nb_model <- cla_nb(target, slevels)
nb_model <- fit(nb_model, voos_gru_train)
nb_results <- avaliar_modelo(nb_model, "NB")

# -------------------------
# Random Forest - Grid Search
# -------------------------
ntree_grid <- c(10, 25, 50)
mtry_grid <- c(2, 3)
best_acc_rf <- 0
best_params_rf <- list()
best_model_rf <- NULL

for (ntree in ntree_grid) {
  for (mtry in mtry_grid) {
    nome_modelo <- paste0("RF_ntree", ntree, "_mtry", mtry)
    cat(sprintf("[RF] Testando ntree = %d, mtry = %d...\n", ntree, mtry))
    
    model <- cla_rf(target, slevels, ntree = ntree, mtry = mtry)
    model <- fit(model, voos_gru_train)
    
    results_grid <- avaliar_modelo(model, nome_modelo)
    acc <- results_grid$accuracy[grepl("teste", results_grid$dataset)]
    
    cat(sprintf("[RF] ntree = %d, mtry = %d, acc = %.4f\n", ntree, mtry, ifelse(length(acc) > 0, acc, NA)))
    
    if (length(acc) > 0 && acc > best_acc_rf) {
      best_acc_rf <- acc
      best_params_rf <- list(ntree = ntree, mtry = mtry)
      best_model_rf <- model
    }
  }
}

cat("📌 RF - Melhor acurácia:", best_acc_rf, "\n")
cat("📌 RF - Melhores parâmetros:", best_params_rf$ntree, "árvores e", best_params_rf$mtry, "variáveis por split\n")

rf_results <- avaliar_modelo(best_model_rf, "RF_best")

# -------------------------
# MLP - Grid Search
# -------------------------
size_grid <- c(1, 2, 3)
decay_grid <- c(0.01, 0.05)
best_acc_mlp <- 0
best_params_mlp <- list()
best_model_mlp <- NULL

for (size in size_grid) {
  for (decay in decay_grid) {
    nome_modelo <- paste0("MLP_size", size, "_decay", decay)
    cat(sprintf("[MLP] Testando size = %d, decay = %.3f...\n", size, decay))
    
    mlp_model <- cla_mlp(target, slevels, size = size, decay = decay)
    mlp_model <- fit(mlp_model, voos_gru_train)
    
    results_grid <- avaliar_modelo(mlp_model, nome_modelo)
    acc <- results_grid$accuracy[grepl("teste", results_grid$dataset)]
    
    cat(sprintf("[MLP] size = %d, decay = %.3f, acc = %.4f\n", size, decay, ifelse(length(acc) > 0, acc, NA)))
    
    if (length(acc) > 0 && acc > best_acc_mlp) {
      best_acc_mlp <- acc
      best_params_mlp <- list(size = size, decay = decay)
      best_model_mlp <- mlp_model
    }
  }
}

cat("📌 MLP - Melhor acurácia:", best_acc_mlp, "\n")
cat("📌 MLP - Melhores parâmetros: size =", best_params_mlp$size, ", decay =", best_params_mlp$decay, "\n")

mlp_results <- avaliar_modelo(best_model_mlp, "MLP_best")

# -------------------------
# SVM - Grid Search
# -------------------------
cost_grid <- c(0.1, 1, 5)
epsilon_grid <- c(0.05, 0.1)
best_acc_svm <- 0
best_params_svm <- list()
best_model_svm <- NULL

for (cost in cost_grid) {
  for (epsilon in epsilon_grid) {
    nome_modelo <- paste0("SVM_cost", cost, "_epsilon", epsilon)
    cat(sprintf("[SVM] Testando cost = %.2f, epsilon = %.2f...\n", cost, epsilon))
    
    svm_model <- cla_svm(target, slevels, cost = cost, epsilon = epsilon)
    svm_model <- fit(svm_model, voos_gru_train)
    
    results_grid <- avaliar_modelo(svm_model, nome_modelo)
    acc <- results_grid$accuracy[grepl("teste", results_grid$dataset)]
    
    cat(sprintf("[SVM] cost = %.2f, epsilon = %.2f, acc = %.4f\n", cost, epsilon, ifelse(length(acc) > 0, acc, NA)))
    
    if (length(acc) > 0 && acc > best_acc_svm) {
      best_acc_svm <- acc
      best_params_svm <- list(cost = cost, epsilon = epsilon)
      best_model_svm <- svm_model
    }
  }
}

cat("📌 SVM - Melhor acurácia:", best_acc_svm, "\n")
cat("📌 SVM - Melhores parâmetros: cost =", best_params_svm$cost, ", epsilon =", best_params_svm$epsilon, "\n")

svm_results <- avaliar_modelo(best_model_svm, "SVM_best")

# -------------------------
# Juntar resultados finais
# -------------------------
results <- rbind(
  nb_results,
  rf_results,
  mlp_results,
  svm_results
)

print(results)
