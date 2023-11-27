install.packages("arrow")
library(arrow)

df<-read_parquet("https://github.com/maslezak/vis/raw/main/re_1v_pyv.parquet")
df2<-read_parquet("https://github.com/maslezak/vis/raw/main/re_paso.parquet")

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

install.packages("stringr")
library(stringr)

library(scales)

ab_agrupacion_nombre <- c("UNION POR LA PATRIA"="UP",
                          "LA LIBERTAD AVANZA"="LLA",
                          "JUNTOS POR EL CAMBIO"="JxC",
                          "HACEMOS POR NUESTRO PAIS"="HACEMOS",
                          "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD"="FIT")

result <- df %>%
  filter(!is.na(agrupacion_nombre), cargo_id == 1) %>%
  group_by(agrupacion_nombre) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE)) %>%
  arrange(desc(votos)) %>%
  mutate(ab_agrupacion_nombre = ab_agrupacion_nombre[agrupacion_nombre]) %>%
  top_n(10)

result2 <- df2 %>%
  filter(!is.na(agrupacion_nombre), cargo_id == 1) %>%
  group_by(agrupacion_nombre) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE)) %>%
  arrange(desc(votos)) %>%
  mutate(ab_agrupacion_nombre = ab_agrupacion_nombre[agrupacion_nombre]) %>%
  top_n(10)

ggplot(result, aes(x = reorder(ab_agrupacion_nombre, desc(votos)), y = votos, fill = ab_agrupacion_nombre)) +
  geom_col() +
  labs(x = "Agrupacion Nombre", y = "Votos") +
  theme_minimal() +
  guides(fill = "none") +
  scale_fill_manual(values = c("UP"="deepskyblue","LLA"="darkviolet","JxC"="orange","HACEMOS"="darkslateblue","FIT"="red" )) +
  scale_y_continuous(breaks = seq(0, 10000000, by = 1000000), labels = scales::number_format(scale = 1e-6, suffix = "M"), limits = c(0, 10000000))

ggplot(result2, aes(x = reorder(ab_agrupacion_nombre, desc(votos)), y = votos, fill = ab_agrupacion_nombre)) +
  geom_col() +
  labs(x = "Agrupacion Nombre", y = "Votos") +
  theme_minimal() +
  guides(fill = "none") +
  scale_fill_manual(values = c("UP"="deepskyblue","LLA"="darkviolet","JxC"="orange","HACEMOS"="darkslateblue","FIT"="red" )) +
  scale_y_continuous(breaks = seq(0, 10000000, by = 1000000), labels = scales::number_format(scale = 1e-6, suffix = "M"), limits = c(0, 10000000))

                       
ab_map <- c("Buenos Aires" = "BA",
            "Catamarca" = "CA",
            "Chaco" = "CH",
            "Chubut" = "CT",
            "Ciudad Autónoma de Buenos Aires" = "CABA",
            "Corrientes" = "CR",
            "Córdoba" = "CB",
            "Entre Ríos" = "ER",
            "Formosa" = "FO",
            "Jujuy" = "JY",
            "La Pampa" = "LP",
            "La Rioja" = "LR",
            "Mendoza" = "MZ",
            "Misiones" = "MI",
            "Neuquén" = "NQN",
            "Río Negro" = "RN",
            "Salta" = "SA",
            "San Juan" = "SJ",
            "San Luis" = "SL",
            "Santa Cruz" = "SC",
            "Santa Fe" = "SF",
            "Santiago del Estero" = "SE",
            "Tierra del Fuego, Antártida e Islas del Atlántico Sur" = "TF",
            "Tucumán" = "TU")

new_df <- df %>%
  filter(!is.na(agrupacion_nombre), cargo_id == 1) %>%
  group_by(distrito_nombre, agrupacion_nombre) %>%
  summarise(sum_votos_cantidad = sum(votos_cantidad)) %>%
  mutate(ab_map = ab_map[distrito_nombre]) %>%
  ungroup()

top_parties <- new_df %>%
  group_by(ab_map, agrupacion_nombre) %>%
  summarise(total_votos = sum(sum_votos_cantidad)) %>%
  top_n(2, total_votos) %>%
  mutate(ab_agrupacion_nombre = ab_agrupacion_nombre[agrupacion_nombre]) %>%
  ungroup()

ggplot(top_parties, aes(x = ab_map, y = total_votos, fill = ab_agrupacion_nombre)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1.0), width = 0.6,) +
  labs(x = "Provincia", y = "Votos",
       fill = "Agrupacion nombre") +
  geom_segment(aes(x = as.numeric(as.factor(ab_map)) + 0.5, xend = as.numeric(as.factor(ab_map)) + 0.5,
                   y = 0, yend = 5000000), linetype = "dashed", color = "grey", size = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("UP"="deepskyblue","LLA"="darkviolet","JxC"="orange","HACEMOS"="darkslateblue","FIT"="red" )) +
  theme(legend.position = "bottom") +
  theme( panel.border = element_blank())+
  scale_y_continuous(breaks = seq(0, 5000000, by = 1000000), labels = scales::number_format(scale = 1e-6, suffix = "M"), limits = c(0, 5000000))

