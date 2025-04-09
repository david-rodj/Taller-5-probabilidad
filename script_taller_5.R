# Carga de librerías necesarias
if (!require(moments)) install.packages("moments")
library(moments)  # Para calcular asimetría y curtosis

# Carga de datos con codificación UTF-8
datos <- tryCatch({
  read.csv("datos_viajes.csv", encoding = "UTF-8", stringsAsFactors = FALSE, 
           check.names = FALSE, na.strings = c("NA", "", "NULL"))
}, error = function(e) {
  cat("Error al leer el archivo con codificación UTF-8:", e$message, "\n")
  cat("Intentando con codificación Latin-1...\n")
  tryCatch({
    read.csv("datos_viajes.csv", encoding = "Latin-1", stringsAsFactors = FALSE, 
             check.names = FALSE, na.strings = c("NA", "", "NULL"))
  }, error = function(e2) {
    cat("Error al leer el archivo con codificación Latin-1:", e2$message, "\n")
    stop("No se pudo leer el archivo de datos.")
  })
})

# Verificar estructura de los datos
cat("Estructura de los datos:\n")
str(datos)
cat("\nNombres de columnas en el archivo:\n")
print(names(datos))
cat("\nResumen de los datos:\n")
print(summary(datos))

# Crear directorio para guardar las gráficas si no existe
dir.create("graficas", showWarnings = FALSE)

# Iniciar archivo de informe con codificación UTF-8
sink("informe_resultados.txt")
cat("INFORME DE ANÁLISIS DE DATOS DE VIAJES\n")
cat("=====================================\n\n")
cat("Fecha de análisis:", format(Sys.time(), "%d-%m-%Y %H:%M:%S"), "\n\n")

# Función para buscar columnas que coincidan con un patrón (insensible a mayúsculas/minúsculas y acentos)
encontrar_columna <- function(nombres_columnas, patron) {
  # Convertir todo a minúsculas
  nombres_lower <- tolower(nombres_columnas)
  patron_lower <- tolower(patron)
  
  # Versiones posibles con diferentes separadores
  patrones <- c(
    patron_lower,
    gsub(" ", "_", patron_lower),
    gsub(" ", ".", patron_lower),
    gsub(" ", "", patron_lower)
  )
  
  # Versiones sin acentos (aproximación)
  patrones_sin_acentos <- c(patrones,
                            gsub("á", "a", patrones),
                            gsub("é", "e", patrones),
                            gsub("í", "i", patrones),
                            gsub("ó", "o", patrones),
                            gsub("ú", "u", patrones))
  
  # Buscar coincidencias
  for (p in patrones_sin_acentos) {
    indices <- grep(p, nombres_lower, fixed = TRUE)
    if (length(indices) > 0) {
      return(nombres_columnas[indices[1]])  # Devolver el nombre original
    }
  }
  
  return(NULL)  # No se encontró ninguna coincidencia
}

# -----------------------------------------------------
# ANÁLISIS DE VARIABLES CATEGÓRICAS
# -----------------------------------------------------

# Buscar columnas categóricas
col_motivo <- encontrar_columna(names(datos), "MOTIVO DEL VIAJE")
col_alojamiento <- encontrar_columna(names(datos), "TIPO ALOJAMIENTO")

# 1. MOTIVO DEL VIAJE
if (!is.null(col_motivo)) {
  cat("\n--- ANÁLISIS DE MOTIVO DEL VIAJE (Columna:", col_motivo, ") ---\n")
  
  # Verificar si hay valores no NA
  if (sum(!is.na(datos[[col_motivo]])) > 0) {
    # Frecuencia y moda
    tabla_motivo <- table(datos[[col_motivo]], useNA = "ifany")
    if (length(tabla_motivo) > 0) {
      prop_motivo <- prop.table(tabla_motivo) * 100
      df_motivo <- data.frame(
        Motivo = names(tabla_motivo),
        Frecuencia = as.numeric(tabla_motivo),
        Porcentaje = as.numeric(prop_motivo),
        stringsAsFactors = FALSE
      )
      df_motivo <- df_motivo[order(-df_motivo$Frecuencia), ]
      cat("Frecuencias y porcentajes:\n")
      print(df_motivo)
      cat("Moda:", df_motivo$Motivo[1], "(", df_motivo$Frecuencia[1], "ocurrencias )\n")
      
      # Gráfico de barras

      
      # Generar gráfica mejorada de barras
      tryCatch({
        # Crear un nuevo dispositivo gráfico
        jpeg(filename = "graficas/motivo_viaje_barras_mejorado.jpg", width = 1000, height = 700, quality = 100)
        
        # Aumentar los márgenes para etiquetas largas
        par(mar = c(12, 6, 4, 2) + 0.1)
        
        # Definir colores para las barras
        colores <- rainbow(length(tabla_motivo))
        
        # Crear barras con espacio entre ellas
        barplot(tabla_motivo, 
                main = "Frecuencia de Motivo del Viaje",
                col = colores, 
                las = 2,           # Etiquetas horizontales
                cex.names = 0.8,   # Tamaño de texto para etiquetas
                space = 0.8,       # Espacio entre barras
                ylim = c(0, max(tabla_motivo) * 1.1)) # Límite vertical con 10% extra
        
        # Añadir línea base
        abline(h = 0, col = "gray")
        
        # Añadir etiquetas con valores encima de cada barra
        barras <- barplot(tabla_motivo, 
                          main = "Frecuencia de Motivo del Viaje",
                          col = colores, 
                          las = 2,           
                          cex.names = 0.8,   
                          space = 0.8,       
                          ylim = c(0, max(tabla_motivo) * 1.1),
                          plot = FALSE)  # Solo para obtener las posiciones
        
        # Añade etiquetas con valores
        text(x = barras, 
             y = tabla_motivo + max(tabla_motivo) * 0.03,  # Posición ligeramente por encima
             labels = tabla_motivo,
             cex = 0.8)  # Tamaño del texto
        
        dev.off()
        cat("Gráfico mejorado guardado: graficas/motivo_viaje_barras_mejorado.jpg\n")
        
      })
    } else {
      cat("No hay datos válidos para MOTIVO DEL VIAJE\n")
    }
  } else {
    cat("La columna MOTIVO DEL VIAJE solo contiene valores NA\n")
  }
} else {
  cat("Columna MOTIVO DEL VIAJE no encontrada en los datos\n")
}

# 2. TIPO ALOJAMIENTO
if (!is.null(col_alojamiento)) {
  cat("\n--- ANÁLISIS DE TIPO ALOJAMIENTO (Columna:", col_alojamiento, ") ---\n")
  
  # Verificar si hay valores no NA
  if (sum(!is.na(datos[[col_alojamiento]])) > 0) {
    # Frecuencia y moda
    tabla_alojamiento <- table(datos[[col_alojamiento]], useNA = "ifany")
    if (length(tabla_alojamiento) > 0) {
      prop_alojamiento <- prop.table(tabla_alojamiento) * 100
      df_alojamiento <- data.frame(
        Alojamiento = names(tabla_alojamiento),
        Frecuencia = as.numeric(tabla_alojamiento),
        Porcentaje = as.numeric(prop_alojamiento),
        stringsAsFactors = FALSE
      )
      df_alojamiento <- df_alojamiento[order(-df_alojamiento$Frecuencia), ]
      cat("Frecuencias y porcentajes:\n")
      print(df_alojamiento)
      cat("Moda:", df_alojamiento$Alojamiento[1], "(", df_alojamiento$Frecuencia[1], "ocurrencias )\n")
      
      # Gráfico de barras
      # Generar gráfica mejorada de barras para TIPO ALOJAMIENTO
      tryCatch({
        # Crear un nuevo dispositivo gráfico
        jpeg(filename = "graficas/tipo_alojamiento_barras_mejorado.jpg", width = 1000, height = 700, quality = 100)
        
        # Aumentar los márgenes para etiquetas largas
        par(mar = c(8, 6, 4, 2) + 0.1)
        
        # Definir colores para las barras - un color diferente para cada tipo de alojamiento
        colores <- rainbow(length(tabla_alojamiento))
        
        # Calcular las posiciones de las barras (sin dibujarlas aún)
        barras <- barplot(tabla_alojamiento, 
                          plot = FALSE,  # Solo para obtener las posiciones
                          space = 0.8)   # Espacio entre barras
        
        # Dibujar las barras con límite Y ajustado para dar espacio a las etiquetas
        barplot(tabla_alojamiento, 
                main = "Frecuencia de Tipo de Alojamiento",
                col = colores, 
                las = 2,           # Etiquetas horizontales
                cex.names = 0.9,   # Tamaño de texto para etiquetas
                space = 0.8,       # Espacio entre barras
                ylim = c(0, max(tabla_alojamiento) * 1.1)) # Límite vertical con 10% extra
        
        # Añadir línea base
        abline(h = 0, col = "gray")
        
        # Añadir etiquetas con valores encima de cada barra
        text(x = barras, 
             y = tabla_alojamiento + max(tabla_alojamiento) * 0.03,  # Posición ligeramente por encima
             labels = tabla_alojamiento,
             cex = 0.9,   # Tamaño del texto
             font = 2)    # Negrita
        
        # Añadir título de los ejes
        title(ylab = "Frecuencia")
        
        # Añadir cuadrícula horizontal para mejor legibilidad
        grid(NA, NULL, lwd = 1, col = "lightgray")
        
        dev.off()
        cat("Gráfico mejorado guardado: graficas/tipo_alojamiento_barras_mejorado.jpg\n")
      }, error = function(e) {
        cat("Error al crear el gráfico:", e$message, "\n")
        # Asegurar que el dispositivo gráfico se cierre en caso de error
        if (names(dev.cur()) != "null device") dev.off()
      })
    } else {
      cat("No hay datos válidos para TIPO ALOJAMIENTO\n")
    }
  } else {
    cat("La columna TIPO ALOJAMIENTO solo contiene valores NA\n")
  }
} else {
  cat("Columna TIPO ALOJAMIENTO no encontrada en los datos\n")
}

# -----------------------------------------------------
# ANÁLISIS DE VARIABLES NUMÉRICAS
# -----------------------------------------------------

# Buscar columnas numéricas
col_tiempo <- encontrar_columna(names(datos), "TIEMPO DE PERMANENCIA")
col_satisfaccion <- encontrar_columna(names(datos), "RESULTADO SATISFACCIÓN")
col_viajeros <- encontrar_columna(names(datos), "CANTIDAD DE VIAJEROS")

# Función para análisis completo de variables numéricas
analizar_variable_numerica <- function(datos, col_nombre, nombre_variable) {
  cat("\n--- ANÁLISIS DE", nombre_variable, "(Columna:", col_nombre, ") ---\n")
  
  # Verificar si es un vector y tiene valores
  if (is.null(datos[[col_nombre]]) || length(datos[[col_nombre]]) == 0) {
    cat("La columna", nombre_variable, "no contiene datos\n")
    return()
  }
  
  # Verificar si se puede convertir a numérico
  variable_num <- tryCatch({
    as.numeric(as.character(datos[[col_nombre]]))
  }, warning = function(w) {
    cat("Advertencia al convertir a numérico:", w$message, "\n")
    return(datos[[col_nombre]])
  }, error = function(e) {
    cat("Error al convertir a numérico:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(variable_num)) {
    cat("No se pudieron convertir los datos a numéricos\n")
    return()
  }
  
  # Eliminar NA, Inf, -Inf
  variable_clean <- variable_num[is.finite(variable_num)]
  if (length(variable_clean) == 0) {
    cat("No hay valores válidos (finitos) para analizar\n")
    return()
  }
  
  # Estadísticas descriptivas
  media <- mean(variable_clean)
  mediana <- median(variable_clean)
  desv_est <- sd(variable_clean)
  
  # Asimetría y curtosis
  if (length(variable_clean) >= 3) {  # Necesario para moments
    asimetria <- tryCatch(skewness(variable_clean), error = function(e) NA)
    curtosis <- tryCatch(kurtosis(variable_clean) - 3, error = function(e) NA)
  } else {
    asimetria <- NA
    curtosis <- NA
  }
  
  cat("Número de valores válidos:", length(variable_clean), "\n")
  cat("Media:", round(media, 2), "\n")
  cat("Mediana:", round(mediana, 2), "\n")
  cat("Desviación estándar:", round(desv_est, 2), "\n")
  cat("Asimetría:", ifelse(is.na(asimetria), "No calculable", round(asimetria, 2)), "\n")
  cat("Curtosis:", ifelse(is.na(curtosis), "No calculable", round(curtosis, 2)), "\n")
  
  # Histograma
  tryCatch({
    nombre_archivo_hist <- paste0("graficas/", gsub("[^a-zA-Z0-9]", "_", tolower(nombre_variable)), "_histograma.jpg")
    jpeg(filename = nombre_archivo_hist, width = 800, height = 600, quality = 100)
    hist(variable_clean, main = paste("Histograma de", nombre_variable), 
         xlab = nombre_variable, col = "lightblue", border = "white",
         breaks = min(max(nclass.Sturges(variable_clean), 5), 30))
    dev.off()
    cat("Histograma guardado:", nombre_archivo_hist, "\n")
  }, error = function(e) {
    cat("Error al crear el histograma:", e$message, "\n")
    if (names(dev.cur()) != "null device") dev.off()
  })
  
  # Boxplot
  tryCatch({
    nombre_archivo_box <- paste0("graficas/", gsub("[^a-zA-Z0-9]", "_", tolower(nombre_variable)), "_boxplot.jpg")
    jpeg(filename = nombre_archivo_box, width = 800, height = 600, quality = 100)
    boxplot(variable_clean, main = paste("Boxplot de", nombre_variable), 
            col = "lightgreen", border = "darkgreen", horizontal = TRUE)
    dev.off()
    cat("Boxplot guardado:", nombre_archivo_box, "\n")
  }, error = function(e) {
    cat("Error al crear el boxplot:", e$message, "\n")
    if (names(dev.cur()) != "null device") dev.off()
  })
}

# Analizar variables numéricas
if (!is.null(col_tiempo)) {
  analizar_variable_numerica(datos, col_tiempo, "TIEMPO DE PERMANENCIA")
} else {
  cat("\n--- La columna TIEMPO DE PERMANENCIA no se encontró en los datos ---\n")
}

if (!is.null(col_satisfaccion)) {
  analizar_variable_numerica(datos, col_satisfaccion, "RESULTADO SATISFACCIÓN")
} else {
  cat("\n--- La columna RESULTADO SATISFACCION no se encontró en los datos ---\n")
}

if (!is.null(col_viajeros)) {
  analizar_variable_numerica(datos, col_viajeros, "CANTIDAD DE VIAJEROS")
} else {
  cat("\n--- La columna CANTIDAD DE VIAJEROS no se encontró en los datos ---\n")
}

# Resumen del análisis
cat("\n\n--- RESUMEN GENERAL ---\n")
cat("El análisis se ha completado.\n")
cat("Las gráficas se han guardado en el directorio 'graficas'.\n")

# Cerrar el archivo de informe
sink()

# Mensaje al usuario en consola
cat("\nAnálisis completado. Se ha generado el informe 'informe_resultados.txt' y las gráficas se han guardado en la carpeta 'graficas'.\n")