library(daltoolbox)

## carrega a base de dados
load("bfd_2022.rdata")

model <- cluster_dbscan(minPts = 3)

numericas <- bfd[,c(17:20,27:31,33:35,38:42,44:46)]
numericas <- na.omit(numericas)
model <- fit(model, numericas)
clu <- cluster(model, numericas)
table(clu)
 clu
df <- data.frame(cluster = clu, numericas)
# problema do dbscan: Fez mais de 3 mil clusters.
model <- clu_tune(cluster_kmeans(k = 0),  ranges = list(k = 1:10))
model <- fit(model, numericas)
model$k

## separar variáveis depart e arrival