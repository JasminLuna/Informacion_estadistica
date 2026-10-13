# ============================================================================
# GUIA 3 | DISENO MUESTRAL Y ANALISIS EXPLORATORIO DE DATOS AMBIENTALES
# Caso: calidad del agua en una microcuenca del canton Loja.
# Los datos del Excel son sintéticos y solo tienen fines didácticos.
# ============================================================================

# 0. Preparacion --------------------------------------------------------------
# Instale los paquetes una sola vez si aún no están disponibles:
# install.packages(c("readxl", "ggplot2"))
paquetes <- c("readxl", "ggplot2")
faltan_paquetes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan_paquetes) > 0) {
  stop("Instale estos paquetes antes de continuar: ", paste(faltan_paquetes, collapse = ", "))
}

# Localice el Excel junto a este script. Si se ejecuta desde RStudio con Source,
# se usa la ruta del archivo abierto; si no se detecta, se abre un selector.
argumentos <- commandArgs(trailingOnly = FALSE)
archivo_script <- grep("^--file=", argumentos, value = TRUE)
if (length(archivo_script) > 0) {
  archivo_script <- sub("^--file=", "", archivo_script[1])
} else {
  archivo_script <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
}
carpeta_script <- if (!is.null(archivo_script) && length(archivo_script) == 1 && nzchar(archivo_script)) {
  dirname(normalizePath(archivo_script, winslash = "/", mustWork = FALSE))
} else {
  getwd()
}
archivo_excel <- file.path(carpeta_script, "Base_datos_Guia_3_calidad_agua.xlsx")
if (!file.exists(archivo_excel)) {
  message("No se encontró el Excel junto al script. Seleccione Base_datos_Guia_3_calidad_agua.xlsx.")
  archivo_excel <- file.choose()
}

cat("\nGUÍA PRÁCTICA 3: DISEÑO MUESTRAL Y ANÁLISIS EXPLORATORIO\n")
cat("Archivo de trabajo:", normalizePath(archivo_excel, winslash = "/"), "\n")

# 1. Problema de investigacion -----------------------------------------------
# Pregunta: ¿cuál es la concentración media de nitratos y qué proporción de los
# puntos supera el umbral didáctico de 7.5 mg/L, que no es un límite normativo?
# El estudio es observacional y transversal: se miden puntos en una campaña,
# sin asignar tratamientos ni manipular la calidad del agua.

# 2. Importacion y reconocimiento del marco muestral -------------------------
datos <- readxl::read_excel(archivo_excel, sheet = "Datos")
si <- paste0("S", "\u00ed")
variables_requeridas <- c(
  "id_punto", "zona", "subcuenca", "uso_suelo", "fecha_muestreo",
  "nitratos_mg_l", "ph", "turbidez_ntu", "conductividad_us_cm",
  "alerta_nitratos", "seleccion_muestra"
)
faltan <- setdiff(variables_requeridas, names(datos))
if (length(faltan) > 0) {
  stop("Faltan variables requeridas en la hoja Datos: ", paste(faltan, collapse = ", "))
}

cat("\n--- Población, marco y unidades ---\n")
cat("Población objetivo: puntos elegibles de monitoreo de la microcuenca durante la campaña definida.\n")
cat("Marco muestral accesible (N):", nrow(datos), "puntos con identificador único.\n")
cat("Unidad muestral y de análisis: cada punto de monitoreo.\n")
cat("Unidad de observación: la muestra de agua y sus mediciones asociadas al punto.\n")
cat("Las lecturas repetidas del mismo punto no son nuevas unidades independientes.\n")
cat("Diseño: estudio observacional, transversal, con muestreo aleatorio simple sin reemplazo.\n")
cat("Frecuencias del marco por zona:\n")
print(table(datos$zona))

# 3. Diseno de muestreo y calculo del tamano -------------------------------
# La hoja incluye una muestra aleatoria simple sin reemplazo de 35 puntos.
# La variable zona sirve para describir cobertura, no define estratos de sorteo.
N <- nrow(datos)
n_plan <- 35L
cat("\n--- Diseño de selección y representación de zonas ---\n")
cat("Muestreo aleatorio simple sin reemplazo; selección fija y marcada en la hoja Datos.\n")
print(table(datos$zona, datos$seleccion_muestra))

# Para una media con población finita, se usa la desviación piloto de 3.5 mg/L,
# confianza de 95% (Z=1.96) y error máximo de 1 mg/L.
Z <- qnorm(0.975)
sigma_piloto <- 3.5
error_media <- 1
n_media <- N * Z^2 * sigma_piloto^2 /
  ((N - 1) * error_media^2 + Z^2 * sigma_piloto^2)
cat("\n--- Tamaño para estimar la media de nitratos ---\n")
cat("Z para 95%:", round(Z, 3), " | n calculado antes de redondear:", round(n_media, 2), "\n")
cat("Para el ejercicio se adoptan 35 puntos, redondeando hacia arriba el cálculo finito.\n")

# Para una proporcion con error de 0.10 se comparan p previa=0.30 y el caso
# conservador p=0.50. q siempre es 1-p. El redondeo se hace hacia arriba.
error_prop <- 0.10
tamano_proporcion <- function(p, N, Z, e) {
  q <- 1 - p
  N * Z^2 * p * q / (e^2 * (N - 1) + Z^2 * p * q)
}
n_prop_previa <- tamano_proporcion(0.30, N, Z, error_prop)
n_prop_conservadora <- tamano_proporcion(0.50, N, Z, error_prop)
cat("\n--- Tamaño para estimar una proporción (error = 0.10) ---\n")
cat("Con p=0.30 y q=0.70: n sin redondear =", round(n_prop_previa, 2),
    "; n operativo =", ceiling(n_prop_previa), "\n")
cat("Sin información previa, p=q=0.50: n sin redondear =", round(n_prop_conservadora, 2),
    "; n operativo =", ceiling(n_prop_conservadora), "\n")
cat("El tamaño adoptado también depende de los recursos y del objetivo del estudio.\n")

# Analisis de sensibilidad: muestra como cambia n al variar p y el error tolerado.
escenarios_n <- expand.grid(p = c(0.30, 0.50), error = c(0.05, 0.10, 0.15))
escenarios_n$n_sin_redondear <- mapply(
  tamano_proporcion, escenarios_n$p, MoreArgs = list(N = N, Z = Z), e = escenarios_n$error
)
escenarios_n$n_requerido <- ceiling(escenarios_n$n_sin_redondear)
cat("\nSensibilidad del tamaño muestral para proporciones:\n")
print(escenarios_n)

# Dibujo de la normal estandar y valores criticos para visualizar el nivel de
# confianza, como apoyo a la tabla de valores Z de la presentacion.
curve(dnorm(x), from = -3.5, to = 3.5, lwd = 2,
      xlab = "z", ylab = "Densidad", main = "Normal estandar: confianza del 95%")
abline(v = c(-Z, Z), col = "#005B41", lty = 2, lwd = 2)
text(c(-Z, Z), y = 0.12, labels = c("-1.96", "1.96"), pos = c(2, 4), col = "#005B41")

# 4. Extraccion de la muestra e intervalos de confianza ---------------------
muestra <- datos[datos$seleccion_muestra == si, ]
if (nrow(muestra) != n_plan) stop("La muestra marcada en el Excel no tiene 35 filas.")
if (anyDuplicated(datos$id_punto)) stop("Hay identificadores de punto duplicados en el marco.")
if (anyNA(muestra$nitratos_mg_l)) stop("Hay nitratos faltantes en la muestra; revise el registro antes de estimar.")

n <- nrow(muestra)
fpc <- sqrt((N - n) / (N - 1))
media_muestral <- mean(muestra$nitratos_mg_l)
sd_muestral <- sd(muestra$nitratos_mg_l)
error_estandar_media <- sd_muestral / sqrt(n) * fpc
ic_media <- media_muestral + c(-1, 1) * qt(0.975, df = n - 1) * error_estandar_media
media_marco <- mean(datos$nitratos_mg_l, na.rm = TRUE)

prop_muestral <- mean(muestra$alerta_nitratos == si, na.rm = TRUE)
prop_marco <- mean(datos$alerta_nitratos == si, na.rm = TRUE)
error_estandar_prop <- sqrt(prop_muestral * (1 - prop_muestral) / n) * fpc
ic_prop <- pmax(0, pmin(1, prop_muestral + c(-1, 1) * qnorm(0.975) * error_estandar_prop))

cat("\n--- Estimación e intervalos de confianza del 95% ---\n")
cat("Media muestral de nitratos:", round(media_muestral, 2), "mg/L; IC con corrección finita:",
    paste(round(ic_media, 2), collapse = " a "), "mg/L\n")
cat("Desviación estándar muestral:", round(sd_muestral, 2), "mg/L\n")
cat("Proporción muestral de alerta:", round(prop_muestral, 3), "; IC normal aproximado con corrección finita:",
    paste(round(ic_prop, 3), collapse = " a "), "\n")
cat("Media de los 120 registros sintéticos:", round(media_marco, 2), "mg/L\n")
cat("Proporción en el marco sintético:", round(prop_marco, 3), "\n")
cat("El marco completo permite comparar estimadores solo en este ejercicio simulado.\n")

# 5. Descripcion de variables y comparaciones exploratorias -----------------
variables_numericas <- c("nitratos_mg_l", "ph", "turbidez_ntu", "conductividad_us_cm",
                         "temperatura_c", "lluvia_24h_mm", "altitud_m")
resumen_numerico <- do.call(rbind, lapply(variables_numericas, function(variable) {
  x <- datos[[variable]]
  data.frame(
    variable = variable, validos = sum(!is.na(x)), faltantes = sum(is.na(x)),
    media = mean(x, na.rm = TRUE), mediana = median(x, na.rm = TRUE),
    desviacion = sd(x, na.rm = TRUE), q1 = unname(quantile(x, 0.25, na.rm = TRUE)),
    q3 = unname(quantile(x, 0.75, na.rm = TRUE)), minimo = min(x, na.rm = TRUE),
    maximo = max(x, na.rm = TRUE)
  )
}))
names(resumen_numerico) <- c("Variable", "Válidos", "Faltantes", "Media", "Mediana",
                             "Desviación", "Q1", "Q3", "Mínimo", "Máximo")
cat("\nResumen numérico del marco (datos sintéticos):\n")
print(resumen_numerico, row.names = FALSE, digits = 3)

resumen_por_grupo <- function(variable) {
  grupos <- split(muestra$nitratos_mg_l, muestra[[variable]])
  resultado <- do.call(rbind, lapply(names(grupos), function(grupo) {
    x <- grupos[[grupo]]
    data.frame(grupo = grupo, n = length(x), media = mean(x), mediana = median(x),
               desviacion = sd(x), minimo = min(x), maximo = max(x))
  }))
  names(resultado) <- c("Grupo", "n", "Media", "Mediana", "Desviación", "Mínimo", "Máximo")
  resultado
}
cat("\nNitratos por zona en la muestra:\n")
print(resumen_por_grupo("zona"), row.names = FALSE, digits = 3)
cat("\nNitratos por uso de suelo en la muestra:\n")
print(resumen_por_grupo("uso_suelo"), row.names = FALSE, digits = 3)

cat("\nComparación exploratoria de nitratos entre zonas (ANOVA de Welch):\n")
print(oneway.test(nitratos_mg_l ~ zona, data = muestra, var.equal = FALSE))
cat("\nPrueba no paramétrica de Kruskal-Wallis por zona:\n")
print(kruskal.test(nitratos_mg_l ~ zona, data = muestra))
cat("Estas comparaciones describen asociaciones; no demuestran efectos causales.\n")

variables_asociacion <- c("lluvia_24h_mm", "altitud_m", "turbidez_ntu",
                          "temperatura_c", "conductividad_us_cm")
asociaciones <- do.call(rbind, lapply(variables_asociacion, function(variable) {
  x <- muestra[[variable]]
  y <- muestra$nitratos_mg_l
  validos <- complete.cases(x, y)
  prueba <- cor.test(x[validos], y[validos], method = "spearman", exact = FALSE)
  data.frame(variable = variable, n = sum(validos), rho = unname(prueba$estimate),
             valor_p = prueba$p.value)
}))
cat("\nAsociaciones exploratorias de Spearman con nitratos:\n")
print(asociaciones, row.names = FALSE, digits = 3)

# 6. Preparacion y control de calidad ---------------------------------------
cat("\n--- Control de calidad ---\n")
cat("Filas:", nrow(datos), " | IDs duplicados:", anyDuplicated(datos$id_punto), "\n")
cat("Faltantes por variable:\n")
print(sort(colSums(is.na(datos)), decreasing = TRUE))
cat("pH fuera del rango fisico 0-14:\n")
print(datos[!is.na(datos$ph) & (datos$ph < 0 | datos$ph > 14), c("id_punto", "ph")])
cat("Nitratos no positivos:\n")
print(datos[!is.na(datos$nitratos_mg_l) & datos$nitratos_mg_l <= 0,
            c("id_punto", "nitratos_mg_l")])
cuartiles_turbidez <- quantile(datos$turbidez_ntu, c(0.25, 0.75), na.rm = TRUE)
limite_inferior <- cuartiles_turbidez[1] - 1.5 * IQR(datos$turbidez_ntu, na.rm = TRUE)
limite_superior <- cuartiles_turbidez[2] + 1.5 * IQR(datos$turbidez_ntu, na.rm = TRUE)
atipicos_turbidez <- datos[datos$turbidez_ntu < limite_inferior |
                             datos$turbidez_ntu > limite_superior, c("id_punto", "turbidez_ntu")]
cat("Posibles valores atípicos de turbidez (revisar, no eliminar automáticamente):\n")
print(atipicos_turbidez)
cat("El faltante de conductividad debe verificarse con la ficha o el laboratorio; no se imputa sin justificación.\n")

# 7. Graficos exploratorios --------------------------------------------------
carpeta_resultados <- file.path(carpeta_script, "Resultados_Guia_3")
dir.create(carpeta_resultados, showWarnings = FALSE)
grafico_distribucion <- ggplot2::ggplot(datos, ggplot2::aes(x = nitratos_mg_l)) +
  ggplot2::geom_histogram(binwidth = 1, color = "white", fill = "#2E6F62") +
  ggplot2::geom_vline(xintercept = 7.5, linetype = "dashed", color = "#A52828") +
  ggplot2::labs(title = "Distribución de nitratos en el marco", x = "Nitratos (mg/L)", y = "Puntos") +
  ggplot2::theme_minimal()
grafico_zona <- ggplot2::ggplot(muestra, ggplot2::aes(x = zona, y = nitratos_mg_l, fill = zona)) +
  ggplot2::geom_boxplot(alpha = 0.65, outlier.shape = NA) +
  ggplot2::geom_jitter(width = 0.12, alpha = 0.75, show.legend = FALSE) +
  ggplot2::labs(title = "Nitratos por zona: muestra", x = "Zona", y = "Nitratos (mg/L)") +
  ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none")
grafico_lluvia <- ggplot2::ggplot(muestra, ggplot2::aes(x = lluvia_24h_mm, y = nitratos_mg_l)) +
  ggplot2::geom_point(color = "#1E508C", alpha = 0.8) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#C66E26") +
  ggplot2::labs(title = "Exploración: lluvia antecedente y nitratos", x = "Lluvia en 24 h (mm)",
                y = "Nitratos (mg/L)") + ggplot2::theme_minimal()
frecuencias_alerta <- do.call(rbind, lapply(split(muestra, muestra$uso_suelo), function(grupo) {
  data.frame(uso_suelo = grupo$uso_suelo[1], n = nrow(grupo),
             proporcion = mean(grupo$alerta_nitratos == si))
}))
grafico_alertas <- ggplot2::ggplot(frecuencias_alerta,
                                  ggplot2::aes(x = uso_suelo, y = proporcion, fill = uso_suelo)) +
  ggplot2::geom_col(show.legend = FALSE) + ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::labs(title = "Proporción con alerta didáctica por uso de suelo",
                x = "Uso de suelo", y = "Proporcion de puntos") + ggplot2::theme_minimal()

graficos <- list(distribucion_nitratos = grafico_distribucion,
                 nitratos_por_zona = grafico_zona,
                 lluvia_y_nitratos = grafico_lluvia,
                 alertas_por_uso = grafico_alertas)
for (nombre in names(graficos)) {
  ggplot2::ggsave(file.path(carpeta_resultados, paste0(nombre, ".png")),
                  plot = graficos[[nombre]], width = 8, height = 5, dpi = 300)
}
cat("\nGráficos guardados en:", normalizePath(carpeta_resultados, winslash = "/"), "\n")
cat("Interprete las pruebas y gráficos como exploratorios; los datos son sintéticos y el umbral de 7.5 mg/L es didáctico.\n")