####################################################################-
##########  Correo recordatorio de requerimientos enviados  ########-
############################# By LE ################################-

################ I. Librerías, drivers y directorio ################

# I.1 Librerías ----

# i) Librerias complementarias
#install.packages("dplyr")
library(dplyr)
#install.packages("lubridate")
library(lubridate)
#install.packages("readxl")
library(readxl)
#install.packages("stringr")
library(stringr)
#install.packages("purrr")
library(purrr)
#install.packages("blastula")
library(blastula)
#install.packages("kableExtra")
library(kableExtra)

# II.2 Configuración de correo
# Una vez realizado no volver a realizar
#install.packages("keyring")
# library(keyring)
# Mi_email_OSPA <- ""
# create_smtp_creds_key(
#   Mi_email_OSPA,
#   user = Mi_email_OSPA,
#   provider = "gmail",
#   use_ssl = TRUE,
#   overwrite = TRUE
# )

#-----------------------------------------------------------------

################## II. Descarga de información ##################

# II.1 Conexión a base de datos de seguimiento ----
SEG_OSPA <- ""
#*La matriz debe contener los IDs correctos para la carga
tp1 <- tempfile() # Creacion de un archivo temporal
download.file(SEG_OSPA, tp1, mode ="wb")
# Descarga de la tabla
INSUMOS <- as.data.frame(read_xlsx(tp1, sheet = "SEGUIMIENTO", skip = 2))


# II.2 Conexión a base de contactos ----
CONTACTOS <- ""
#*La matriz debe contener los IDs correctos para la carga
tp2 <- tempfile() # Creacion de un archivo temporal
download.file(CONTACTOS, tp2, mode ="wb")
# Descarga de la tabla
CONTACTOS_EFA <- as.data.frame(read_xlsx(tp2, sheet = "OSPA-EFA", skip = 1))

#-----------------------------------------------------------------

################### III. Parámetros y funciones #####################

# II.1 Parámetros para envío de correos ----

# I) Destinatarios SEFA
sefa_actores <- c("")
sefa_desarrolladores <- c("")


# II.2.1 Email: Cabecera ----
Arriba <- add_image(
  file = "https://i.imgur.com/ys8iA1b.png",
  width = 1000,
  align = c("right"))
Cabecera <- md(Arriba)

# II.2.2 Email: Pie de página ----
Logo_Oefa <- add_image(
  file = "https://i.imgur.com/ImFWSQj.png",
  width = 280)
Pie_de_pagina <- blocks(
  md(Logo_Oefa),
  block_text(md("Av. Faustino Sánchez Carrión N.° 603, 607 y 615 - Jesús María"), align = c("center")),
  block_text(md("Teléfonos: 204-9900 Anexo 7154"), align = c("center")),
  block_text("www.oefa.gob.pe", align = c("center")),
  block_text(md("**Síguenos** en nuestras redes sociales"), align = c("center")),
  block_social_links(
    social_link(
      service = "Twitter",
      link = "https://twitter.com/OEFAperu",
      variant = "dark_gray"
    ),
    social_link(
      service = "Facebook",
      link = "https://www.facebook.com/oefa.peru",
      variant = "dark_gray"
    ),
    social_link(
      service = "Instagram",
      link = "https://www.instagram.com/somosoefa/",
      variant = "dark_gray"
    ),
    social_link(
      service = "LinkedIn",
      link = "https://www.linkedin.com/company/oefa/",
      variant = "dark_gray"
    ),
    social_link(
      service = "YouTube",
      link = "https://www.youtube.com/user/OEFAperu",
      variant = "dark_gray"
    )
  ),
  block_spacer(),
  block_text(md("Imprime este correo electrónico sólo si es necesario. Cuidar el ambiente es responsabilidad de todos."), align = c("center"))
)

# II.2.2 Email: Asunto y Botones ----
# i) Asunto
mes_actual <- month(now(), label=TRUE, abbr = FALSE)
mes_actual <- str_to_lower(mes_actual)

# ii) Botón al tablero en la Web
cta_button <- add_cta_button(
  url = "https://www.oefa.gob.pe/observatorio-sinefa/",
  text = "Web del Observatorio"
)

# II.2 Funciones ----

# i) Creación de tabla
tabla <- function(efa){
  TABLA <- INSUMOS %>%
    filter(DESTINATARIO== efa) %>%
    filter(TAREA == "Pedido de información" |
             TAREA == "Pedido de información adicional" |
             TAREA == "Pedido de información urgente")  %>%
    filter(`MARCO DEL PEDIDO` != "Colaboración") %>%
    filter(`AUX - SEGUIMIENTO A LA RESPUESTA DEL DOC EMITIDO` == "En plazo") %>%
    filter(`AUX - SEGUIMIENTO A LA RESPUESTA DEL DOC EMITIDO` == "En plazo") %>%
    select(`COD DE PROBLEMA`, `N° COMPLETO DEL DOCUMENTO`, `FECHA MAXIMA PARA LA RESPUESTA`) %>%
    arrange(`FECHA MAXIMA PARA LA RESPUESTA`) %>%
    mutate(`FECHA MAXIMA PARA LA RESPUESTA` = format(`FECHA MAXIMA PARA LA RESPUESTA`,
                                                     "%d/%m/%Y"))
  
  TABLA
}

# ii) Selección de destinatarios
destinar <- function(x,
                     # Consideramos valores predeterminados
                     y = NA, z = NA){
  
  # Definimos los destinatarios eliminando posibles NA
  dest <- c(x, y, z)
  dest_f <- dest[!is.na(dest)]
  dest_f
  
}

# iii) Envío de correo
correo_auto <- function(efa, nombre,
                        d1, d2,
                        cc1, cc2, cc3){
  # i) Destinatarios 
  # Principales
  dest <- destinar(d1, d2)
  Destinatarios <- c(dest)

  # En copia
  cc <- destinar(cc1, cc2, cc3)
  cc_f <- c(cc, sefa_actores)
  Destinatarios_cc <- c(cc_f)

  # En copia oculta
  bcc <- sefa_desarrolladores
  Destinatarios_bcc <- c(bcc)


  # ii) Asunto
  Asunto <- paste("OSPA - OEFA | Recordatorio semanal del ", 
                  day(now())," de ", mes_actual, " de ", year(now()))
  
  # iii) Generación de tabla recordatorio
  TABLA_RECORDATORIO <- tabla(efa)
  # Diseño de tabla
  tabla_formato <- TABLA_RECORDATORIO %>%
    kbl(col.names = c("Código de Problema",
                      "N° Completo del Documento",
                      "Fecha máxima para la respuesta"),
        align = c("c", "c", "c")) %>%
    kable_styling(font_size = 12, bootstrap_options = "basic" , latex_options = "HOLD_position", full_width = FALSE) %>%
    column_spec(1 , latex_valign = "m", width = "3cm") %>%
    column_spec(2 , latex_valign = "m", width = "6cm") %>%
    column_spec(3 , latex_valign = "m", width = "3cm") %>%
    row_spec(0, bold = TRUE, color = "white", background = "gray", align = "c")
  
  # II.3.1 Email: Cuerpo del mensaje ----

  # Negrita en nombre de la EFA
  efa_n = paste0("**",efa,"**")
  # Texto en el correo
  Cuerpo_del_mensaje <- blocks(
  md(c("
Estimado/a,", nombre, "   
       ",
       efa_n,
"
El Organismo de Evaluación y Fiscalización Ambiental (OEFA) remite, en calidad de recordatorio periódico, el listado de documentos pendientes de
atención por parte de su representada, con el fin de que sean atendidos dentro de los plazos otorgados y evitar la emisión de reiterativos, los cuales
van con copia a su Órgano de Control Institucional.

A continuación el detalle de los requerimientos:")),
    
    md(c(tabla_formato)),
    md("
 
 Sin perjuicio de lo anterior, les recordamos que la información sobre el estado de los requerimientos emitidos por parte del OSPA,
 incluidos aquellos cuyo plazo de atención se encuentra vencido (**no considerados en el listado anterior**), es de acceso público
 a través de la web institucional del OEFA. Para conocer el detalle, haga clic en el siguiente botón:"
     ),
     md(c(cta_button)),
     md("
 
 ***
 **Tener en cuenta:**
 - En caso de haber emitido la respuesta a los requerimientos indicados, agradeceremos asegurar que los mismos hayan sido debidamente notificados por la mesa de partes virtual del OEFA, a través del siguiente enlace: https://sistemas.oefa.gob.pe/mpv/#/tramite, contando con el respectivo cargo de recepción.
 - Este correo electrónico ha sido generado de manera automática. Para mayor información contactarse con *** o ***.
 - El uso de lenguajes de programación de alto nivel para facilitar el trabajo realizado en SEFA es parte de un proyecto impulsado desde la Subdirección.
 - Para desafiliarse del servicio de correos recordatorios o cambiar los contactos a los que están dirigidos, por favor, comuníquese a ***.
      ")
   )
  
  
  # II.3.2 Email: Composición ----
  email <- compose_email(
    header = Cabecera,
    body = Cuerpo_del_mensaje, 
    footer = Pie_de_pagina,
    content_width = 1000
  )
  
  # II.3.3 Email: Envío ----
  smtp_send(
    email,
    to = Destinatarios,
    from = c("OEFA - SEFA" = ""),
    subject = Asunto,
    cc = Destinatarios_cc,
    bcc = Destinatarios_bcc,
    credentials = creds_key(id = ""),
    verbose = TRUE
  )
}


# iii) Función robustecida de descarga y carga de información
R_correo_auto <- function(efa, nombre,
                          d1, d2,
                          cc1, cc2, cc3){
  out = tryCatch(correo_auto(efa, nombre,
                             d1, d2,
                             cc1, cc2, cc3),
                 error = function(e){
                   correo_auto(efa,  nombre,
                               d1, d2,
                               cc1, cc2, cc3) 
                 })
  return(out)
}


#-----------------------------------------------------------------

################# IV. Preparación de datos ########################

# IV.1 Selección de universo de EFAs ----
EFAS_TOTALES <- INSUMOS  %>%
  filter(is.na(`FECHA DE LA RESPUESTA`)) %>%
  filter(TAREA == "Pedido de información" |
           TAREA == "Pedido de información adicional" |
           TAREA == "Pedido de información urgente") %>%
  filter(`AUX - SEGUIMIENTO A LA RESPUESTA DEL DOC EMITIDO` == "En plazo") %>%
  select(DESTINATARIO, TAREA) %>%
  group_by(DESTINATARIO) %>%
  summarise(PEDIDOS = n()) %>%
  arrange(-PEDIDOS)

# IV.2 Selección de EFAs afiliadas ----
CONTACTOS_APROBADO <- CONTACTOS_EFA %>%
  filter(AFILIADO == "SI") %>%
  select(DESTINATARIO,
         NOMBRE, CARGO, OFICINA,
         DESTINATARIO_1, DESTINATARIO_2,
         CC1, CC2, CC3)

# IV.2 Selección de población de EFAs afiliadas al servicio
EFAS_CORREO <- merge(EFAS_TOTALES, CONTACTOS_APROBADO)
EFAS_CORREO <- EFAS_CORREO %>%
  arrange(-PEDIDOS)

#-----------------------------------------------------------------

#################### V. Envío de correos ########################
# V.1 Envío de correos a EFA ----
pwalk(list(EFAS_CORREO$DESTINATARIO,
           EFAS_CORREO$NOMBRE,
           EFAS_CORREO$DESTINATARIO_1,
           EFAS_CORREO$DESTINATARIO_2,
           EFAS_CORREO$CC1,
           EFAS_CORREO$CC2,
           EFAS_CORREO$CC3),
      slowly(R_correo_auto, 
             rate_backoff(10, max_times = Inf)))

# V.2 Envío de correo a SEFA ----

# Asunto de validación
Asunto_val <- paste("OSPA - OEFA | Recordatorios enviados la semana del ", 
                day(now())," de ", mes_actual, " de ", year(now()))

# Tabla de EFAS que recibieron correo
EFAS_CORREO_VAL <- EFAS_CORREO %>%
  arrange(-PEDIDOS)  %>%
  mutate(DESTINATARIO_F = paste0(NOMBRE,
                                 ", ",
                                 CARGO)) %>%
  select(DESTINATARIO, DESTINATARIO_F, PEDIDOS) %>%
  relocate(DESTINATARIO, DESTINATARIO_F, PEDIDOS)

# Número de correos en negrita
n_correos <- paste0("**",nrow(EFAS_CORREO),"**")

# Diseño de tabla
tabla_formato_val <- EFAS_CORREO_VAL %>%
  kbl(col.names = c("Entidad de Fiscalización Ambiental",
                    "Destinatario",
                    "Req. pendientes"),
      align = c("c", "c", "c")) %>%
  kable_styling(font_size = 12, bootstrap_options = "basic" , latex_options = "HOLD_position", full_width = FALSE) %>%
  column_spec(1 , latex_valign = "m", width = "10cm") %>%
  column_spec(2 , latex_valign = "m", width = "8cm") %>%
  column_spec(3 , latex_valign = "m", width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray", align = "c")

# Cuerpo del mensaje
Cuerpo_del_mensaje_validacion <- blocks(
  md(c("
Buenos días, Paola Castañeda Félix:   

**Jefa del Observatorio de Solución de Problemas Ambientales**

¡Se han enviado exitosamente los ", n_correos, " correos recordatorios de esta semana!
Cabe destacar que estos correos se envían gracias a la información registrada en las bases
de datos del Observatorio y el registro de información de EFA afiliadas a este servicio.
Estos recordatorios hacen referencia a requerimientos de información que aún no tienen respuesta
y a fecha de hoy están en plazo.

A continuación el detalle de las EFA afiliadas a las que se les envió el correo recordatorio:")),
  
  md(c(tabla_formato_val)))


# V.3 Email: Composición ----
email_val <- compose_email(
  header = Cabecera,
  body = Cuerpo_del_mensaje_validacion, 
  footer = Pie_de_pagina,
  content_width = 1000
)

# V.4 Email: Envío ----
smtp_send(
  email_val,
  to = sefa_actores,
  from = c("OEFA - SEFA" = ""),
  subject = Asunto_val,
  cc = sefa_desarrolladores,
  credentials = creds_key(id = ""),
  verbose = TRUE
)
