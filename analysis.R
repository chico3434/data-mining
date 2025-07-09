
load("bfd_2022.rdata")

summary(bfd)

numericas <- bfd[,c(17:20,27:31,33:35,38:42,44:46)]

# nota-se um grande número (mais de 10% nna maioria das variáveis) de NA's nas variáveis relacionadas a tempo ou condições do voo (temperatura, umidade, vento, pressão, visibilidade).
# a variável "observação" possui apenas NA's

# companhia aérea
table(bfd$company)

summary(bfd$arrival_wind_direction)

hist(bfd$arrival_wind_direction)

table(bfd$arrival_wind_direction_cat)

sum(is.na(bfd$arrival_wind_direction_cat))

summary(bfd$arrival_wind_speed)

boxplot(bfd$arrival_wind_speed)

#install.packages("corrplot")
corrplot::corrplot(cor(na.omit(numericas)), method = "color",
                   type = "lower", tl.cex = 0.6, tl.col = "black",
                   order = 'hclust', addCoef.col = 'black',
                   cl.pos = 'n', number.cex = 0.6)

# correlação de 0.98 entre delay_depart e delay_arrival

dim(numericas)
dim(na.omit(numericas))
dim(na.omit(numericas))[1]/dim(numericas)[1]
dim(na.omit(numericas[,1:2]))
dim(na.omit(numericas[,1:2]))[1]/dim(numericas)[1]
cor(na.omit(numericas[,1:2]))

# removendo NA apenas do atraso tem-se ~0.99 correlação entre depart e arrival
# além disso, removendo NA de atraso se perde um pouco mais de 4% da base
# removendo NA das numéricas perde-se quase 27% da base.

numericas_depart_status <- bfd[,c(14,17:20,27:31,33:35,38:42,44:46)]

library(tidyverse)
numericas_depart_status %>% na.omit %>% group_by(status_depart) %>% 
  summarize(delay_depart = mean(delay_depart), delay_arrival = mean(delay_arrival),
            expected_flight_length = mean(expected_flight_length), real_flight_length = mean(real_flight_length),
            depart_visibility = mean(depart_visibility))

ggplot(bfd, aes(x=factor(status_depart),
                 y=expected_flight_length, fill=factor(status_depart))) +
  xlab("Status Partida") +
  theme(legend.position="none",
        axis.text = element_text(size = 14),
        text = element_text(size = 15),
        axis.text.x = element_text(size = 15)) +
  geom_boxplot()
