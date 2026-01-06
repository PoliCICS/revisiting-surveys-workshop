# ==============================================================================
# DESAFÍO: ¿DÓNDE IMPORTA EL CONTEXTO?
# Plantilla para el Alumno
# ==============================================================================

# 1. PREPARACIÓN (NO EDITAR) ---------------------------------------------------
# Cargamos las librerías necesarias. Si no las tienes, descomenta y corre:
# install.packages(c("haven", "dplyr", "spdep", "broom"))

library(haven)
library(dplyr)
library(spdep)
library(broom)

# Cargamos los datos limpios (ANES 2016)
DATA_PATH <- "data/anes_timeseries_2016.dta"
if (!file.exists(DATA_PATH)) stop("Error: No se encuentra el archivo 'anes_timeseries_2016.dta'. Asegúrate de estar en la carpeta correcta del proyecto.")

anes_raw <- read_dta(DATA_PATH)

message("Datos cargados correctamente. N = ", nrow(anes_raw))

# ==============================================================================
# 2. SELECCIÓN DE TU VARIABLE (¡EDITAR AQUÍ!)
# ==============================================================================

# INSTRUCCIONES:
# 1. Elige UN código de variable de la lista de abajo.
# 2. Reemplaza "V16xxxx" en la línea 'my_variable_code' con el código elegido.

# --- Lista de Variables Disponibles ---
# V161126 : Liberal - Conservative Self-placement
# V161139 : Current Economy Assessment
# V161154 : Willingness to use Force (International)
# V161201 : Environment vs. Jobs Tradeoff
# V161522 : Satisfaction with Life
# V161137 : Income Gap vs 20 years ago
# V161188 : Importance of Gun Access
# V161209 : Federal Budget: Welfare Programs
# V161212 : Federal Budget: Environment


# >>>>>>>>> EDITA ESTA LÍNEA <<<<<<<<<
my_variable_code <- "V161209" # Ejemplo: Welfare Programs. ¡Cámbialo por el que quieras probar!
# >>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<


# ==============================================================================
# 3. LIMPIEZA DE DATOS (AUTOMÁTICO - NO EDITAR)
# ==============================================================================
message("Limpiando datos y seleccionando tu variable: ", my_variable_code, "...")

# Seleccionamos variables demográficas base + TU variable elegida
model_df <- anes_raw %>%
    select(
        weight = V160101,
        age_raw = V161267,
        educ_raw = V161270,
        gender_raw = V161342,
        race_raw = V161310x,
        relig_raw = V161265x,
        target_raw = all_of(my_variable_code) # Selecciona tu variable dinámicamente
    ) %>%
    filter(weight > 0) %>%
    mutate(
        # Limpieza Demográfica Estándar
        age = if_else(age_raw < 0, NA_real_, as.numeric(age_raw)),
        female = case_when(gender_raw == 2 ~ 1, gender_raw == 1 ~ 0, TRUE ~ NA_real_),
        educ = if_else(educ_raw < 0, NA_real_, as.numeric(educ_raw)),
        race_white_dummy = if_else(race_raw == 1, 1, 0),
        race_black = if_else(race_raw == 2, 1, 0),
        race_asian = if_else(race_raw == 3, 1, 0),
        race_native = if_else(race_raw == 4, 1, 0),
        race_hispanic = if_else(race_raw == 5, 1, 0),
        race_other = if_else(race_raw == 6, 1, 0),
        relig_protestant_dummy = if_else(relig_raw %in% c(1, 2, 3), 1, 0),
        relig_catholic = if_else(relig_raw == 4, 1, 0),
        relig_jewish = if_else(relig_raw == 6, 1, 0),
        relig_none = if_else(relig_raw == 8, 1, 0),
        relig_other = if_else(relig_raw %in% c(5, 7), 1, 0),

        # Limpieza de TU variable (Asumimos que -9, -8, etc son NA)
        target_clean = if_else(target_raw < 0, NA_real_, as.numeric(target_raw))
    ) %>%
    # Filtramos casos completos
    filter(!is.na(age), !is.na(educ), !is.na(female), !is.na(race_raw), !is.na(relig_raw), !is.na(target_clean)) %>%
    haven::zap_labels()

message("Datos listos. N Final para análisis: ", nrow(model_df))

# ==============================================================================
# 4. IMPUTACIÓN DE LA RED SOCIAL (LA "MAGIA" DE SMITH 2014)
# ==============================================================================
message("Calculando distancias sociales e imputando red (puede tardar unos segundos)...")

# 1. Definimos las distancias entre todos los individuos
X_age <- model_df$age
X_educ <- model_df$educ
X_sex <- model_df$female
X_race <- as.integer(model_df$race_raw)
X_relig <- as.integer(model_df$relig_raw)

D_age <- abs(outer(X_age, X_age, "-"))
D_educ <- abs(outer(X_educ, X_educ, "-"))
D_sex <- abs(outer(X_sex, X_sex, "-"))
D_race <- outer(X_race, X_race, function(x, y) ifelse(x != y, 1, 0))
D_relig <- outer(X_relig, X_relig, function(x, y) ifelse(x != y, 1, 0))

# 2. Pesos de Homofilia (Basados en Smith et al / GSS)
betas <- c(race = -1.352, relig = -1.354, sex = -0.256, age = -0.047, educ = -0.189)

# 3. Calculamos la probabilidad de vínculo (eta)
eta <- betas["age"] * D_age + betas["educ"] * D_educ + betas["sex"] * D_sex +
    betas["race"] * D_race + betas["relig"] * D_relig

# 4. Seleccionamos los 100 "vecinos" más probables para cada persona
w_raw <- exp(eta)
diag(w_raw) <- 0 # Nadie es vecino de sí mismo

K <- 100
N <- nrow(model_df)
neighbors <- vector("list", N)
weights_list <- vector("list", N)

for (i in 1:N) {
    row_w <- w_raw[i, ]
    # Ordenamos para encontrar los K más cercanos
    ord <- order(row_w, decreasing = TRUE)
    candidates <- setdiff(ord, i)
    top_k_indices <- candidates[1:K]
    top_k_weights <- row_w[top_k_indices]

    neighbors[[i]] <- top_k_indices
    weights_list[[i]] <- top_k_weights
}

# Creamos el objeto de pesos espaciales 'listw'
class(neighbors) <- "nb"
attr(neighbors, "region.id") <- as.character(1:N)
attr(neighbors, "call") <- match.call()
W_listw <- nb2listw(neighbors, glist = weights_list, style = "W", zero.policy = TRUE)

# 5. Calculamos el "Contexto" (Lag Espacial)
# Wy es el promedio de la variable 'target' en los 100 vecinos de cada persona
model_df$Wy <- lag.listw(W_listw, model_df$target_clean)

message("Red imputada y variable contextual (Wy) creada.")

# ==============================================================================
# 5. RESULTADOS: ¿ES SIGNIFICATIVO EL CONTEXTO?
# ==============================================================================

# Ajustamos dos modelos:
# Modelo 1: Solo Demografía (¿Quienes somos explica qué pensamos?)
fmla_base <- target_clean ~ age + educ + female + race_black + race_hispanic + race_asian + relig_catholic + relig_none

# Modelo 2: Demografía + CONTEXTO (Wy)
fmla_context <- update(fmla_base, ~ . + Wy)

m_context <- lm(fmla_context, data = model_df)

# Extraemos y mostramos el resultado clave
res_tidy <- tidy(m_context) %>% filter(term == "Wy")

cat("\n======================================================\n")
cat("RESULTADOS PARA LA VARIABLE:", my_variable_code, "\n")
cat("======================================================\n")
cat("Coeficiente Contextual (Rho):", round(res_tidy$estimate, 3), "\n")
cat("P-Value (Significancia):     ", format.pval(res_tidy$p.value, digits = 4), "\n")
cat("------------------------------------------------------\n")

if (res_tidy$p.value < 0.05) {
    cat("CONCLUSIÓN: ¡SÍ! El contexto ES significativo.\n")
    cat("Tus opiniones dependen de quiénes te rodean.\n")
} else {
    cat("CONCLUSIÓN: NO. El contexto NO es significativo.\n")
    cat("¡Has encontrado una variable 'inmune' a la presión social! (O al menos a esta red).\n")
    cat(">> ENVÍA ESTE RESULTADO AL PROFESOR <<\n")
}
cat("======================================================\n")
