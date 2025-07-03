
load("bfd_2022.rdata")

summary(bfd)

# nota-se um grande número (mais de 10% nna maioria das variáveis) de NA's nas variáveis relacionadas a tempo ou condições do voo (temperatura, umidade, vento, pressão, visibilidade).
# a variável "observação" possui apenas NA's

# companhia aérea
table(bfd$company)


