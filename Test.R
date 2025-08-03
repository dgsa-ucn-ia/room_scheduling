# --- 1. INSTALACIÓN Y CARGA DE LIBRERÍAS ---

# Asegúrate de tener las librerías instaladas. Si no, ejecuta las siguientes líneas:
# install.packages("dplyr")
# install.packages("ompr")
# install.packages("ompr.roi")
# install.packages("ROI.plugin.glpk")
# install.packages("tidyr")
# install.packages("readr")

library(dplyr)
library(tidyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(readr)
library(openxlsx)



# --- 2. CARGA DE DATOS ---

# Cargar los datos desde los archivos CSV
# Nota: Ajusta la ruta si los archivos no están en el mismo directorio que el script.
tryCatch({
  requests_raw <- read_csv2("room_applicants.csv", show_col_types = FALSE)
  rooms_raw <- read_csv2("available_rooms.csv", show_col_types = FALSE) # Usar read_csv2 por el delimitador ;
}, error = function(e) {
  stop("Error al leer los archivos. Asegúrate de que 'room_applicants.xlsx - Sheet1.csv' y 'available_rooms.csv' estén en el directorio correcto.")
})

head(requests_raw)
head(rooms_raw)




# --- 3. PROCESAMIENTO Y LIMPIEZA DE DATOS ---

# Limpiar y transformar los datos de solicitudes
requests <- requests_raw %>%
  # Convertir las columnas de días de la semana a un formato largo
  pivot_longer(cols = monday:sunday, names_to = "dayOfWeek", values_to = "is_scheduled") %>%
  filter(is_scheduled == "Y") %>%
  # Corregir nombres de días para que coincidan con el archivo de salas
  mutate(dayOfWeek = tolower(dayOfWeek)) %>%
  # Crear un ID único para cada fila original para poder agrupar los crossList
  mutate(original_row_id = row_number()) %>%
  # Agrupar por crossList para tratar los cursos relacionados como una sola entidad
  group_by(crossList) %>%
  mutate(
    # Si hay crossList, usar los asientos de la lista cruzada, si no, los asientos normales
    final_seats = if_else(!is.na(crossList), first(crossListSeats), seats),
    # Combinar los CRNs para la solicitud agrupada
    grouped_crn = if_else(!is.na(crossList), paste(unique(crn), collapse = ", "), as.character(crn))
  ) %>%
  # Seleccionar una fila por grupo de crossList/día/bloque para evitar duplicados
  ungroup() %>%
  distinct(grouped_crn, dayOfWeek, block, .keep_all = TRUE) %>%
  # Limpiar requisitos especiales
  mutate(
    specialRequirement = gsub(" ", "", na_if(specialRequirement, "")),
    requiredFloor = gsub(" ", "", na_if(requiredFloor, ""))
  ) %>%
  # Combinar requisitos en una sola columna y separar por comas
  unite("all_reqs", c(specialRequirement, requiredFloor), sep = ",", na.rm = TRUE, remove = FALSE) %>%
  mutate(
    all_reqs = if_else(all_reqs == "", NA_character_, all_reqs)
  ) %>%
  # Crear un ID único para cada solicitud procesada
  mutate(request_id = row_number()) %>%
  select(request_id, grouped_crn, final_seats, dayOfWeek, block, all_reqs, title)

# Limpiar datos de las salas
rooms <- rooms_raw %>%
  mutate(
    # Limpiar los códigos de detalle de la sala
    detailCodes = gsub(" ", "", detailCodes),
    # Crear un ID único para cada combinación de sala, día y bloque
    resourceId = paste(roomNumber, dayOfWeek, block, sep = "_")
  ) %>%
  select(resourceId, roomNumber, capacity, dayOfWeek, block, detailCodes)





# --- 4. CREAR TABLA DE ASIGNACIONES POSIBLES ---

# Función para verificar si una sala cumple con todos los requisitos de una solicitud
check_requirements <- function(request_reqs, room_details) {
  if (is.na(request_reqs) || request_reqs == "") {
    return(TRUE) # No hay requisitos especiales, la sala es compatible
  }
  # Separar los requisitos de la solicitud en una lista
  req_list <- strsplit(request_reqs, ",")[[1]]
  # Verificar que todos los requisitos estén en los detalles de la sala
  all(sapply(req_list, function(req) grepl(req, room_details, fixed = TRUE)))
}

# Crear todas las combinaciones posibles de solicitud-sala
possible_assignments <- requests %>%
  inner_join(rooms, by = c("dayOfWeek", "block")) %>%
  # Filtrar por capacidad
  filter(final_seats <= capacity) %>%
  # Filtrar por requisitos especiales
  rowwise() %>%
  filter(check_requirements(all_reqs, detailCodes)) %>%
  ungroup() %>%
  select(request_id, resourceId, grouped_crn, roomNumber, dayOfWeek, block)


write.xlsx(possible_assignments, "possible_assignments_test.xlsx", rowNames = FALSE)




# --- 5. CONSTRUIR Y RESOLVER EL MODELO DE OPTIMIZACIÓN ---

# Verificar si hay asignaciones posibles
print(nrow(possible_assignments))
if(nrow(possible_assignments) == 0) {
  stop("No se encontraron asignaciones válidas entre las solicitudes y las salas disponibles. No se puede optimizar.")
}

print("va a entrar")

model <- MIPModel()
model <- model %>%
  add_variable(x[request_id, resourceId], type = "binary", .data = possible_assignments)

print(model)


# Resolver el modelo
result <- solve_model(model, with_ROI(solver = "glpk"))

# --- 6. MOSTRAR LOS RESULTADOS ---

if (result$status == "optimal") {
  # Obtener las asignaciones del resultado
  assignments <- result$solution %>%
    filter(value > 0.9) %>%
    select(request_id, resourceId) %>%
    # Unir con la información de las solicitudes y las salas para un reporte claro
    left_join(possible_assignments, by = c("request_id", "resourceId")) %>%
    distinct(request_id, .keep_all = TRUE) %>%
    select(`Curso (CRN/crossList)` = grouped_crn,
           `Sala Asignada` = roomNumber,
           `Día` = dayOfWeek,
           `Bloque` = block)
  
  # Calcular la cobertura
  total_requests <- n_distinct(requests$request_id)
  assigned_requests <- n_distinct(assignments$`Curso (CRN/crossList)`)
  coverage <- (assigned_requests / total_requests) * 100
  
  # Imprimir resultados
  cat("=============================================\n")
  cat("       RESULTADOS DE ASIGNACIÓN DE SALAS\n")
  cat("=============================================\n\n")
  
  print(assignments)
  
  cat(sprintf("\nTotal de solicitudes únicas: %d\n", total_requests))
  cat(sprintf("Solicitudes asignadas: %d\n", assigned_requests))
  cat(sprintf("Cobertura de asignación: %.2f%%\n", coverage))
  
} else {
  cat("No se encontró una solución óptima. Estado del solver: ", result$status, "\n")
}

