# tidyverse para dplyr e ggplot2
library(tidyverse)

## carrega a base de dados
load("bfd_2022.rdata")

## summary para ter um overview dos atributos da base
summary(bfd)
# nota-se um grande número (mais de 10% nna maioria das variáveis) de NA's nas variáveis relacionadas a tempo ou condições do voo (temperatura, umidade, vento, pressão, visibilidade).
# a variável "observação" possui apenas NA's

# boxplot de delay_depart (limitado em -250 e 250)
bfd %>% ggplot(aes(y=delay_depart)) +
  theme(legend.position="none",
        axis.text = element_text(size = 14),
        text = element_text(size = 15),
  ) +
  ylim(-250,250) + 
  geom_boxplot()

# boxplot de delay_depart por tipo de voo
ggplot(filter(bfd, !is.na(delay_depart)), aes(x=factor(type),y=delay_depart, fill=factor(type))) +
  xlab("Tipo de Voo") +
  theme(legend.position="none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8)) +
  ylim(-250,250) + 
  geom_boxplot()

# categorização de outlier usando 3*IQR para boxplot posterior
# Não foi utilizado, pois remove todos os atrasos (atraso >= 30)
bfd$is_delay_depart_outlier <- bfd$delay_depart > (quantile(bfd$delay_depart,0.75,na.rm = T) + 3*IQR(bfd$delay_depart, na.rm = T)) | bfd$delay_depart < (quantile(bfd$delay_depart,0.25,na.rm = T) - 3*IQR(bfd$delay_depart, na.rm = T))

ggplot(filter(bfd, !is_delay_depart_outlier), aes(x=factor(type),y=delay_depart, fill=factor(type))) +
  xlab("Tipo de Voo") +
  theme(legend.position="none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8)) +
  geom_boxplot()

boxplot(bfd$arrival_wind_speed)

# seleção das variáveis numéricas para corrplot
numericas <- bfd[,c(17:20,27:31,33:35,38:42,44:46)]
#install.packages("corrplot")
corrplot::corrplot(cor(na.omit(numericas)), method = "color",
                   type = "lower", tl.cex = 0.5, tl.col = "black",
                   order = 'hclust', addCoef.col = 'white',
                   cl.pos = 'n', number.cex = 0.5, col = corrplot::COL1('YlOrBr', 200))

# correlação de 0.98 entre delay_depart e delay_arrival

numericas_depart_status <- bfd[,c(14,17:20,27:31,33:35,38:42,44:46)]

# os códigos abaixo foram análise para tentar entender como foi calculada outlier_depart_delay
# onde se chegou a conclusão que não foi usado o método IQR
# para ser ter um número próximo de outliers usando o método IQR seria necessário multiplicá-lo por 111.2
analise.outlier <- bfd[,17:26]
analise.outlier$delay_depart_is_outlier <- analise.outlier$delay_depart > (quantile(analise.outlier$delay_depart,0.75,na.rm = T) + 1.5*IQR(analise.outlier$delay_depart, na.rm = T)) | analise.outlier$delay_depart < (quantile(analise.outlier$delay_depart,0.25,na.rm = T) - 1.5*IQR(analise.outlier$delay_depart, na.rm = T))
analise.outlier$delay_depart_is_outlier <- analise.outlier$delay_depart > (quantile(analise.outlier$delay_depart,0.75,na.rm = T) + 111.2*IQR(analise.outlier$delay_depart, na.rm = T)) | analise.outlier$delay_depart < (quantile(analise.outlier$delay_depart,0.25,na.rm = T) - 111.2*IQR(analise.outlier$delay_depart, na.rm = T))
table(analise.outlier$delay_depart_is_outlier)
table(analise.outlier$outlier_depart_delay)
analise.outlier$delay_depart_is_outlier <- analise.outlier$delay_depart > (3*sd(analise.outlier$delay_depart, na.rm = T)) | analise.outlier$delay_depart < (3*sd(analise.outlier$delay_depart, na.rm = T))
table(analise.outlier$outlier_depart_delay == analise.outlier$delay_depart_is_outlier) 

# top 10 atributos com maior número de NA`s
na.analise <- bfd %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "coluna", values_to = "qtd_na") %>%
  arrange(desc(qtd_na)) %>% head(10)

na.analise <- na.analise %>% mutate(perc_na = paste0(round(qtd_na/dim(bfd)[1]*100,1),"%"))
na.analise  

# exportando resultados da análise de NA.
clipr::write_clip(na.analise$coluna, dec=",")
clipr::write_clip(na.analise$qtd_na, dec=",")
clipr::write_clip(str_replace(na.analise$perc_na, "[.]", ","))

# comparação de status depart com expected flight length
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = expected_flight_length, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(0, 900) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# real expected length
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = real_flight_length, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(0, 900) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# depart_air_temperature (comportamento parecido)
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = depart_air_temperature, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(0, 50) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# depart_apparent_temperature (comportamento parecido)
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = depart_apparent_temperature, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(0, 50) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# depart_visibility (comportamento parecido)
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = depart_visibility, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(0,7.5) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# depart_relative_humidity (comportamento parecido)
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = depart_relative_humidity, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(10, 110) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# depart_wind_speed (comportamento parecido)
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = depart_wind_speed, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(0, 30) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# depart_pressure (comportamento parecido)
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = depart_pressure, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(29, 33) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# depart_dew_point (comportamento parecido)
ggplot(filter(bfd, !is.na(status_depart)), 
       aes(x = factor(status_depart, 
                      levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                 "Atraso 60-120", "Atraso 120-240", "Atraso > 240")), 
           y = depart_dew_point, 
           fill = factor(status_depart, 
                         levels = c("Antecipado", "Pontual", "Atraso 30-60", 
                                    "Atraso 60-120", "Atraso 120-240", "Atraso > 240")))) +
  geom_boxplot() +
  xlab("Status Partida") +
  ylim(-10, 35) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 8))


# Análises focadas no atraso de partida
prop.table(table(bfd$status_depart))

ggplot(filter(bfd, !is.na(delay_depart)), aes(x = delay_depart)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  xlab("Atraso na Partida (minutos)") +
  ylab("Frequência") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    text = element_text(size = 14)
  )+ xlim(-250, 250)

# plot da densidade com boxplot
ggplot(filter(bfd, !is.na(delay_depart)), aes(x = delay_depart)) +
  geom_density(fill = "steelblue", alpha = 0.5, bw = 12) +
  geom_boxplot(aes(y = -0.005),  # desloca o boxplot mais para baixo
               width = 0.01,     # controla a altura visual do boxplot
               fill = "steelblue", 
               alpha = 0.6,
               outlier.colour = "red",
               outlier.size = 1) +
  coord_cartesian(xlim = c(-250, 250)) +
  xlab("Atraso na Partida (minutos)") +
  ylab("Densidade") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    text = element_text(size = 14),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

summary(bfd$delay_depart)

# número de atrasos
sum(bfd$delay_depart >= 30, na.rm = T)/845160

# outliers calculados usando a técnica de boxplot conservadora
(quantile(bfd$delay_depart,0.75,na.rm = T) + 1.5*IQR(bfd$delay_depart, na.rm = T)) 
(quantile(bfd$delay_depart,0.25,na.rm = T) - 1.5*IQR(bfd$delay_depart, na.rm = T)) 
