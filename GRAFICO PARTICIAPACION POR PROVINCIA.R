library(arrow)
library(sqldf)
ruta <- "C:/Users/Santi/Desktop/TP VIS EN R/ResultadosElectorales_PASO_2023.parquet"
datos <- arrow::read_parquet(ruta)
library(ggplot2)
tabla1 <- sqldf("SELECT sum(votos_cantidad) as total_votos FROM datos WHERE cargo_ID = 1 AND votos_tipo = 'POSITIVO' ")
tabla2 <- sqldf("SELECT agrupacion_nombre AS agrupacion,
                sum(votos_cantidad) AS votos,
                (sum(votos_cantidad) / (SELECT total_votos FROM tabla1)) * 100 AS porcentaje
              FROM datos
               WHERE cargo_ID = 1 AND votos_tipo = 'POSITIVO'
               GROUP BY 1
              ORDER BY 2 DESC
                limit 5")

library(ggplot2)
ggplot(tabla2, aes(x= agrupacion, y = votos, fill = agrupacion)) + geom_bar(stat = 'identity') + labs(title = "Cantidad de votos por partido", x = "Partido", y = "Votos") + theme_minimal()

tabla3 <- sqldf("SELECT distrito_nombre AS Provincia,
                sum(votos_cantidad) AS votos

              FROM datos
               WHERE cargo_ID = 1 
               GROUP BY 1
              ORDER BY 2 DESC
                limit 24")
ruta2 <- "D:/padron_parti.parquet"
padron_parti <- arrow::read_parquet(ruta2)



install.packages("reshape2")
library(reshape2)




padron_parti$distrito_nombre <- reorder(padron_parti$distrito_nombre, -padron_parti$participacion)

# Crea un gráfico de barras
ggplot(padron_parti, aes(x = distrito_nombre, y = participacion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Participación electoral por provincia", x = "Provincia", y = "Porcentaje de participación") +
  coord_flip()

tabla4 <- sqldf("SELECT AVG(participacion) AS Promedio_Participacion_Pais
      FROM padron_parti")

Padron_parti_paso <- arrow::read_parquet("D:/participaciones_PASO.parquet")
Padron_parti_paso$distrito_nombre <- reorder(Padron_parti_paso$distrito_nombre, -Padron_parti_paso$participacion_PASO)
ggplot(Padron_parti_paso, aes(x = distrito_nombre, y = participacion_PASO)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Participación electoral por provincia", x = "Provincia", y = "Porcentaje de participación") +
  coord_flip()

tabla5 <- sqldf("SELECT AVG(participacion_PASO) AS Promedio_Participacion_Pais
      FROM Padron_parti_paso")