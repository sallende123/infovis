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
#Grafico participacion generales 
ggplot(padron_parti, aes(x = distrito_nombre, y = participacion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Participación electoral por provincia en GENERALES", x = "Provincia", y = "Porcentaje de participación") +
  coord_flip()

tabla4 <- sqldf("SELECT AVG(participacion) AS Promedio_Participacion_Pais
      FROM padron_parti")

Padron_parti_paso <- arrow::read_parquet("D:/participaciones_PASO.parquet")
Padron_parti_paso$distrito_nombre <- reorder(Padron_parti_paso$distrito_nombre, -Padron_parti_paso$participacion_PASO)
#Grafico participacion paso
ggplot(Padron_parti_paso, aes(x = distrito_nombre, y = participacion_PASO)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Participación electoral por provincia en PASO", x = "Provincia", y = "Porcentaje de participación") +
  coord_flip()

tabla5 <- sqldf("SELECT AVG(participacion_PASO) AS Promedio_Participacion_Pais
      FROM Padron_parti_paso")



 #Grafico de porcentaje de por provincia del total
par(mar = c(7, 5, 6, 2) + 0.1)

tabla3$Provincia[tabla3$Provincia == "Tierra del Fuego, Antártida e Islas del Atlántico Sur"] <- "Tierra del Fuego"
tabla3$Provincia[tabla3$Provincia == "Ciudad Autónoma de Buenos Aires"] <- "CABA"

bp <- barplot(porcentaje, 
              ylab = "Porcentaje de votos",
              las = 2, 
              width = 0.7, 
              ylim = c(0, max(porcentaje) + 5))  
axis(1, at = bp, labels = tabla3$Provincia, las = 2, cex.axis = 0.8)



