# Paquetes ----
# https://media.giphy.com/media/3o6Mbhi5olzmJNuUvu/giphy.gif

pacman::p_load(tidyverse, googlesheets4, gargle, extrafont, scales,
               ggalt, kableExtra, wordcloud, networkD3)

# Datasets ----


kiwi21 <- read_sheet("1nhpqDWuJoWhiVj0rIaV51-SdfnSxTpbV3Vcd3iYmyTw")

# Tipo de cambio
tc <- read_sheet("194DbwO2TNmYkWU5Ru1m6UxuOCfXxuRlFcrWrP_JzGv8") %>% 
  select(pais, tipo_cambio)

# Seteos generales -----
options(scipen = 999)

loadfonts(quiet = TRUE)

# Estilo de los gráficos
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#fbfcfc"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

estiloh <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.y = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia verticales en gris claro
estilov <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.x = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))


fuente <- "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam 2021"

colores <-  c("#8624F5", "#1FC3AA")

# Creo objetos para formatear las etiquetas numéricas de los ejes x e y
eje_x_n <- scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

eje_y_n <- scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

# EDA -----

names(kiwi21)
glimpse(kiwi21)

kiwi21 %>% 
  filter(Trabajo == "Relación de Dependencia") %>% 
  select(sueldo_bruto = `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`) %>% 
  mutate(sueldo_bruto = as.numeric(unlist(sueldo_bruto)))

# Análisis -------

## Participación -----
# Relación dependencia vs. freelo

trabajo_tipo <- kiwi21 %>% 
  count(Trabajo) %>% 
  mutate(porcentaje = n/sum(n)) %>% 
  arrange(-n)

# Acomoda el texto en renglones
trabajo_tipo$Trabajo <- str_wrap(trabajo_tipo$Trabajo, width = 13)

trabajo_tipo

# Calculamos los límites superiores de cada rectángulo
trabajo_tipo$ymax <- cumsum(trabajo_tipo$porcentaje)

# Calculamos el límite inferior de cada porción
trabajo_tipo$ymin <- c(0, head(trabajo_tipo$ymax, n=-1))

# Calculamos la posición de la etiqueta
trabajo_tipo$posicion_etiqueta <- (trabajo_tipo$ymax + trabajo_tipo$ymin) / 2

# Creamos las etiquetas de cada porción
trabajo_tipo$etiqueta <- paste0(trabajo_tipo$Trabajo, # paste0 pega elementos
                             "\n Cant: ", 
                             trabajo_tipo$n)

# Ver como quedó el data frame
trabajo_tipo


ggplot(trabajo_tipo, aes(ymax=ymax, 
                         ymin=ymin, 
                         xmax=4, 
                         xmin=3, 
                         fill=Trabajo)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2,4)) +
  theme_void() +             # Elimina fondos y referencias
  scale_fill_manual(values = c("#4E5667","#28877A")) +   # Define una escala de colores
  theme(legend.position = "none",
        plot.title.position = "plot",
        text = element_text(family = "Roboto") 
        ) + # Modifica posición leyendas y del título
  labs(title = "Respuestas según Tipo de Trabajo",
       fill = "Tipo de Trabajo", 
       caption = fuente) +
  geom_label(x = 3.5,
             aes(y = posicion_etiqueta,
                 label = etiqueta),
             size = 3, 
             color = "white",
             family = "Ubuntu")


## Respuestas por país ------
paises <- kiwi21 %>% 
  select(pais = `País en el que trabajas`) %>% 
  mutate(cuenta = 1) %>% 
  group_by(pais) %>% 
  summarise(conteo = sum(cuenta)) %>% 
#  filter(conteo > 4) %>% 
  arrange(-conteo)


paises

paises %>% 
  rename(País = pais,
         Cantidad = conteo) %>% 
  kbl() %>% 
  kable_paper("hover", full_width = F) %>% 
  scroll_box(height = "450px")


htmltools::div(style='height:600px; overflow-y: scroll', gt(paises) %>% 
      tab_header(title = "Cantidad de respuestas por país") 
)

## Sueldo promedio por país ----
sueldos_dolar <- kiwi21 %>% 
  filter(Trabajo !="Freelance") %>% 
  select(genero = `Identidad de Género`, 
         puesto = `¿En qué puesto trabajás?`,
         pais = `País en el que trabajas` ,
         sueldo_bruto = `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`,
         tipo_contratacion = `Tipo de contratación`)



sueldos_dolar <- sueldos_dolar %>% 
  mutate(puesto = str_trim(puesto, side = "both")) %>% 
  filter(!puesto %in% c("-", "Desarrollador", "Inspección de calidad", "Técnico"))

sueldos_dolar <- sueldos_dolar %>% 
  left_join(tc, by="pais") %>% 
  mutate(multiplicador = 1, #if_else(contrato == "Part time", 1.5, 1),
         sueldo = as.numeric(unlist(sueldo_bruto)),
         sueldo_ft = sueldo * multiplicador,
         sueldo_dolar = sueldo_ft/tipo_cambio,
         cuenta = 1)


glimpse(sueldos_dolar)

sueldo_dolar_avg <- sueldos_dolar %>% 
  filter(puesto %in% c("Gerente", "Jefe", "Responsable", "HRBP", "Analista", "Administrativo")) %>% 
  mutate(puesto = factor(puesto,
                         levels = c("Gerente", "Jefe", "Responsable", "HRBP", "Analista", "Administrativo"))) %>% 
  group_by(puesto)# %>% 
  #summarise(sueldo_dolar_promedio = mean(sueldo_dolar))

sueldo_dolar_avg

# Wordcloud ------

names(kiwi21)

nombre <- kiwi21[,31]
nombre <- nombre %>%
  rename(area = `¿Cómo se llama el área en tu empresa?`) %>% 
  filter(!is.na(area)) %>% 
  mutate(area = str_replace(area, "Adm. De personal", "Administración de Personal"),
         area = str_replace(area, "GESTION DE DESARROLLO HUMANO", "Gestión de Desarrollo Humano" ),
         area = tolower(area))

unique(nombre$area, sort = T)

nombre <- nombre %>% 
tidytext::unnest_tokens(word, area)


nombre <- nombre %>% 
  group_by(word) %>% 
  tally(sort = T) 

set.seed(99)
wordcloud(words = nombre$word, freq = nombre$n, random.order = F,
          rot.per = 0.35, scale= c(2.5,1), max.words = 200, colors=brewer.pal(8, "Dark2"))

wordcloud2::wordcloud2(nombre)

# Covid-19 -----

names(kiwi21)
covid <- kiwi21[,c(51,54)]

#var_original <- names(covid)

names(covid) <- c("decision", "satisfaccion")


covid <- covid %>% 
  filter(!is.na(satisfaccion),
         decision != "No aplica")



covid <- covid %>% 
  group_by(decision, satisfaccion) %>% 
  tally()

nodes <- data.frame(
  name = c(as.character(covid$decision),
           as.character(covid$satisfaccion)) %>% 
    unique()
)


covid$id_source <- match(covid$decision, nodes$name) - 1
covid$id_target <- match(covid$satisfaccion, nodes$name) -1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["De común acuerdo", "De la empresa","Del empleado", 
"Algo insatisfecho", "Algo satisfecho", "Muy insatisfecho", "Muy satisfecho", "No aplica / Prefiero no responder"])
.range(["blue", "purple" , "orange", "orange", "gray", "yellow", "purple", "purple"])'



covid <- covid %>%
  mutate(satisfaccion = factor(satisfaccion,
                           levels = c("No aplica / Prefiero no responder", "Muy instatisfecho",
                                      "Algo insatisfecho", "Algo satisfecho", "Muy satisfecho")))


p <- sankeyNetwork(Links = covid, Nodes = nodes,
                   Source = "id_source", Target = "id_target",
                   colourScale = my_color, fontSize = 8, fontFamily = "Roboto",
                   Value = "n", NodeID = "name", sinksRight = TRUE,
                   height = 300)
p

# Freelancers ----

names(kiwi21)
freelo <- kiwi21[, c(2:9, 56:70)]

freelo <- freelo %>% 
  rename(genero = `Identidad de Género`,
         formacion = `Máximo nivel de formación`,
         carrera = `¿Qué carrera de grado estudiaste?`,
         pais = `País en el que trabajas`,
         trayectoria = `¿Hace cuántos años trabajás como freelance?`,
         espacio_trabajo = `¿Dónde trabajás habitualmente? (sin considerar la coyuntura por COVID-19)`,
         exporta = `¿Exportás tus servicios?`,
         colaboracion = `¿Trabajás con otros freelancers de tu mismo rubro?`,
         es_recruiter = `¿Tu servicio principal está relacionado con búsqueda y selección?`,
         recruiter_it = `¿Te dedicás principalmente a realizar búsquedas de IT/Tecnología?`,
         servicio = `¿Cuál es el servicio principal que brindas? (si brindás más de un servicio, elegí el que más ingresos genere)`
         ) %>% 
  filter(Trabajo == "Freelance") %>% 
  mutate(genero = fct_collapse(genero,"Mujer cis" = "Mujer"))

freelo %>% 
  count(genero, sort = T)

freelo %>% 
  count(es_recruiter)

recruiters <- freelo %>% 
  filter(es_recruiter == "Si")

recruiters %>% 
  count(recruiter_it)

freelo %>% 
  group_by(exporta, es_recruiter) %>% 
  tally()



## Gráfico de género freelo ----

div <- freelo %>% 
  select(genero) %>% 
  mutate(genero = factor(genero, 
                         levels = c("Mujer cis", "Hombre cis", 
                                    "Prefiero no responder",
                                    "No binario", "Gay"))) %>% 
  group_by(genero) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  arrange(-n)

# Compute the cumulative percentages (top of each rectangle)
div$ymax <- cumsum(div$freq)

# Compute the bottom of each rectangle
div$ymin <- c(0, head(div$ymax, n=-1))

# Compute label position
div$labelPosition <- (div$ymax + div$ymin) / 2

# Compute a good label
div$label <- paste0(div$genero, "\n Cant: ", div$n)

# Make the plot
ggplot(div, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=genero)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("#8624F5", "#1FC3AA", "#FFD129", "#75838F", "#7583FF")) +
  theme_void() +
  theme(legend.position = "right",
        panel.background = element_blank(),
        text = element_text(family = "Roboto")) +
  labs(title = "Cantidad de respuestas según identidad de género",
       subtitle = "Freelancers",
       fill = "Género", 
       caption = fuente)

freelo %>% 
  group_by(exporta, es_recruiter) %>% 
  tally() %>% 
  ggplot(aes(x = exporta, y = n, fill = es_recruiter)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), 
            position = position_dodge(0.9),
            vjust = -0.2)+
  estiloh +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "Proporción de exportadores de servicios",
       subtitle = "Según sean servicios de reclutamiento o no",
       x = "Exporta Servicios",
       fill = "Es recruiter",
       y = "",
       caption = fuente)


freelo %>% 
  group_by(colaboracion, es_recruiter) %>% 
  tally() %>% 
  ggplot(aes(x = colaboracion, y = n, fill = es_recruiter)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), 
            position = position_dodge(0.9),
            vjust = -0.2)+
  estiloh +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "Proporción de exportadores de servicios",
       subtitle = "Según sean servicios de reclutamiento o no",
       x = "Exporta Servicios",
       fill = "Es recruiter",
       y = "",
       caption = fuente)

# Sueldo por función -----

sueldos_dolar <- kiwi21 %>% 
  filter(Trabajo !="Freelance") %>% 
  select(puesto = `¿En qué puesto trabajás?`,
         pais = `País en el que trabajas` ,
         sueldo_bruto = `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`,
         tipo_contratacion = `Tipo de contratación`,
         funcion = `¿Cuál es tu función principal en RRHH?`) %>% 
  mutate(sueldo_bruto = as.numeric(unlist(sueldo_bruto)))


sueldos_dolar <- sueldos_dolar %>% 
  mutate(puesto = str_trim(puesto, side = "both")) %>% 
  filter(!puesto %in% c("-", "Desarrollador", "Inspección de calidad", "Técnico"),
         !funcion %in% c("Deberia poder marcarse mas de una opción aquí","Salud y Seguridad",
                         "Formulación de proyectos", "No trabajo en RRHH", "Customer"))


# Agrego un multiplicador de sueldos para convertir los sueldos part time en full time
sueldos_dolar <- sueldos_dolar %>% 
  left_join(tc, by="pais") %>% 
  mutate(multiplicador = 1, #if_else(contrato == "Part time", 1.5, 1),
         sueldo = as.numeric(unlist(sueldo_bruto)),
         sueldo_ft = sueldo * multiplicador,
         sueldo_dolar = sueldo_ft/tipo_cambio,
         cuenta = 1)


# Estimamos percentiles 5 y 95 para usar valores más centrales
# podamos todo lo que esté fuera de ese rango

numericos <- funModeling::profiling_num(sueldos_dolar$sueldo_dolar)
poda_p05 <- numericos[1,6]
poda_p95 <- numericos[1,10]

sueldos_dolar <- sueldos_dolar %>% 
  filter(between(sueldo_dolar,
                 poda_p05, poda_p95))

## Limpieza de funciones ----

unique(sueldos_dolar$funcion)

sueldos_dolar <- sueldos_dolar %>% 
  mutate(funcion = fct_collapse(funcion,
                                "People Analytics" = c("HRIS", "People analytics",
                                                       "Analista de datos - Operaciones",
                                                       "Data scientist"),
                                "Payroll" = c("Payroll / Liquidación de sueldos", 
                                              "Administración personal, liquidación de sueldos",
                                              "Coordinación de Procesos de Liquidación"),
                                "Generalista" = c("Todo menos payroll", "Administración de RRHH, Comunicación Interna, Rexlutamientl dr Selección, Relaciones Laborales y RSE",
                                                  "Analista gestión de RRHH", "Todos los anteriores", "De todo un poco por ser un equipo chico", 
                                                  "todas las anteriores", "Gestión de los equipos de TH", "Todas las anteriores",
                                                  "Asistente administrativo", "ADP, comunicación interna y Reclutamiento y selección",
                                                  "Comunicación, Capacitación y RSE","Todo lo que tenga que ver con RR.HH",
                                                  "Varias funciones", "Todos los departamentos",
                                                  "de todo un poco..!", "Todas las áreas integrales de RH",
                                                  "RH y Gestión ( RH, RL, SO, CERTIFICACIONES"),
                                "Reclutamiento" = c("Reclutamiento y selección",
                                                    "Selección & Desarrollo"),
                                "Diseño Organizacional" = c("Diseño organizacional",
                                                            "Gestión Estrategica"),
                                "Clima y Cultura" = c("Clima y cultura", "People Experience",
                                                      "Candidate Experience"),
                                "Capacitación y desarrollo" = c("Capacitación y desarrollo",
                                                                "Desarrollo de talento")
                                ))

sueldos_dolar %>% 
  count(funcion, sort = T)


sueldos_dolar %>% 
  filter(!funcion %in% c("Compliance", "Control de Gestión")) %>% 
  group_by(funcion) %>% 
  summarise(sueldo_promedio = mean(sueldo_dolar)) %>% 
#  mutate(relleno = if_else(funcion == "People Analytics", "a", "b")) %>% 
  ggplot(aes(x = sueldo_promedio, y = reorder(funcion, sueldo_promedio))) +
  geom_col(fill = "#839192") +
  eje_x_n +
  geom_vline(xintercept = mean(sueldos_dolar$sueldo_dolar), color = "#0955F7", linetype = 2, size = 0.8) +
  geom_text(aes(label = paste0("U$S ", round(sueldo_promedio))),
            size = 3,
            family = "Roboto",
            color = "white", 
            hjust = 1.2) +
  annotate("text", label = paste0("Promedio","\nU$S ",round(mean(sueldos_dolar$sueldo_dolar))),
           x = mean(sueldos_dolar$sueldo_dolar)+180,
           y = "Relaciones laborales", 
           family = "Roboto",
           color = "#0955F7",
           size = 4) +
#  scale_fill_manual(values = c("#E4BB3F", "#85929E")) +
  labs(title = "Sueldo promedio por función",
       subtitle = "En U$S - Todos los países del relevamiento",
       x = "Sueldo Promedio en U$S",
       y = "",
       fill = "",
       caption = fuente) +
  theme(plot.title.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#FBFCFC"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "#AEB6BF"),
        text = element_text(family = "Roboto")) 


