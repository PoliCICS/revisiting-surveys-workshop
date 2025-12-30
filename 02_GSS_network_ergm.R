# ------------------------------------------------------------------------------
#   Genera 100 redes simuladas de N=1000 nodos basadas en la encuesta GSS 2004.
#   La estructura de la red se genera mediante un ERGM que utiliza coeficientes de
#   homofilia sociodemográfica estimados por Smith et al. (2014).
#   El coeficiente 'edges' se calibra para alcanzar una densidad de red objetivo.
#
# Inputs:
#   - 'B - Surveys Data/GSS 2004/GSS 2004 NORC.dta': Dataset original de GSS 2004.
#
# Outputs:
#   - Guarda 100 objetos de red en formato .rds en la carpeta:
#     'trabajo_1_files/GSS_network_ergm/GSS_network_simulated_1000_XXX.rds'
# ------------------------------------------------------------------------------

# --- 0. Cargar Librerías ---
library(ergm)
library(dplyr)
library(haven)
library(doParallel)
library(network)

# --- 1. Preparación de Datos desde GSS 2004 ---

# Cargar el dataset original de GSS 2004
GSS_2004 <- read_dta("data/02_GSS_network_ergm/GSS_2004_NORC.dta")

# Definir las variables sociodemográficas y de propensión a la acción que se usarán
demographic_vars_gss <- c("age", "sex", "educ", "race", "relig")
propensity_ingredient_vars <- c("signdpet", "avoidbuy", "joindem", "attrally", 
                                "cntctgov", "polfunds", "usemedia", "interpol", "actlaw")

# Limpiar y recodificar las variables de GSS 2004 en un nuevo dataframe
gss_df_cleaned <- GSS_2004 %>%
  rename_with(tolower) %>%
  mutate(
    sex = na_if(sex, 9),
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    
    race = case_when(
      hispanic %in% c(2:6, 10, 15, 20:24, 30, 40, 41) ~ "Hispanic",
      racecen1 == 1 ~ "White",
      racecen1 == 2 ~ "Black",
      racecen1 %in% c(3, 11, 15) ~ "Other",
      racecen1 %in% c(4:10) ~ "Asian",
      racecen1 == 16 ~ "Hispanic",
      TRUE ~ NA_character_
    ),
    race = factor(race, levels = c("Asian", "Black", "Hispanic", "White", "Other")),
    
    educ_num = ifelse(educ %in% c(98, 99), NA_real_, as.numeric(educ)),
    
    age = ifelse(age %in% c(98, 99), NA_real_, as.numeric(age)),
    
    relig = case_when(
      relig == 1 ~ "Protestant",
      relig == 2 ~ "Catholic",
      relig == 3 ~ "Jewish",
      relig == 4 ~ "None",
      relig %in% c(5:13) ~ "OtherRelig",
      TRUE ~ NA_character_
    ),
    relig = factor(relig, levels = c("Catholic", "Jewish", "None", "OtherRelig", "Protestant"))
  )

# Limpiar las variables de propensión (reemplazar códigos de NA por NA_real_)
for (p_var in propensity_ingredient_vars) {
  gss_df_cleaned[[p_var]] <- na_if(gss_df_cleaned[[p_var]], 8)
  gss_df_cleaned[[p_var]] <- na_if(gss_df_cleaned[[p_var]], 9)
}

# ******** INICIO DE LA CORRECCIÓN ********
# PASO CRÍTICO: Convertir las columnas 'labelled' de propensión a formato numérico simple.
# El paquete 'haven' las carga con metadatos que 'network' no puede manejar.
gss_df_cleaned <- gss_df_cleaned %>%
  mutate(across(all_of(propensity_ingredient_vars), as.numeric))
# ******** FIN DE LA CORRECCIÓN ********

# Definir el conjunto completo de atributos que deben estar completos
attribute_vars_full <- c("age", "sex", "educ_num", "race", "relig", propensity_ingredient_vars)

# Filtrar para quedarse solo con los casos que tienen datos completos para TODOS los atributos
gss_df_complete <- gss_df_cleaned[complete.cases(gss_df_cleaned[, attribute_vars_full]), ]
N_gss_complete <- nrow(gss_df_complete)

cat(paste("Número de casos completos en GSS 2004:", N_gss_complete, "\n"))

# Submuestreo de 1000 casos para la simulación de redes
set.seed(987)
gss_df_1000 <- gss_df_complete[sample(N_gss_complete, 1000), ]
N_gss_1000 <- nrow(gss_df_1000)

# --- 2. Creación del Objeto Network Base (Nodos + Atributos) ---

gss_base_network_1000 <- network.initialize(N_gss_1000, directed = FALSE)

set.vertex.attribute(gss_base_network_1000, "age", gss_df_1000$age)
set.vertex.attribute(gss_base_network_1000, "sex", as.character(gss_df_1000$sex))
set.vertex.attribute(gss_base_network_1000, "educ_num", gss_df_1000$educ_num)
set.vertex.attribute(gss_base_network_1000, "race", as.character(gss_df_1000$race))
set.vertex.attribute(gss_base_network_1000, "relig", as.character(gss_df_1000$relig))

# Este bucle ahora funcionará porque las columnas ya son numéricas simples
for (p_var in propensity_ingredient_vars) {
  set.vertex.attribute(gss_base_network_1000, p_var, gss_df_1000[[p_var]])
}

# --- 3. Definición del Modelo ERGM y Coeficientes ---

formula_homofilia_only <- ~ nodematch("race") +
  nodematch("sex") +
  absdiff("age") +
  absdiff("educ_num") +
  nodematch("relig")

beta_s2014_raw <- c(Different_Race = -1.959, Different_Religion = -1.270, Different_Sex = -0.373,
                    Age_Difference = -0.047, Education_Difference = -0.157, Year_2004 = -0.052,
                    Diff_Race_x_Year = 0.264, Diff_Relig_x_Year = -0.215, Diff_Sex_x_Year = 0.144,
                    Age_Diff_x_Year = -0.005, Educ_Diff_x_Year = -0.044)

coef_eff_diff_race  <- beta_s2014_raw["Different_Race"] + beta_s2014_raw["Diff_Race_x_Year"]
coef_eff_diff_relig <- beta_s2014_raw["Different_Religion"] + beta_s2014_raw["Diff_Relig_x_Year"]
coef_eff_diff_sex   <- beta_s2014_raw["Different_Sex"] + beta_s2014_raw["Diff_Sex_x_Year"]
coef_eff_absdiff_age  <- beta_s2014_raw["Age_Difference"] + beta_s2014_raw["Age_Diff_x_Year"]
coef_eff_absdiff_educ <- beta_s2014_raw["Education_Difference"] + beta_s2014_raw["Educ_Diff_x_Year"]

coef_homofilia_fijos <- setNames(
  c(-coef_eff_diff_race,
    -coef_eff_diff_sex,
    coef_eff_absdiff_age,
    coef_eff_absdiff_educ,
    -coef_eff_diff_relig),
  c("nodematch.race",
    "nodematch.sex",
    "absdiff.age",
    "absdiff.educ_num",
    "nodematch.relig")
)

# --- 4. Calibración del Coeficiente `edges` ---

calibrate_edges_coefficient <- function(
    atp_base_network_input, target_density_input, formula_homofilia_terms, 
    fixed_homophily_coefs, initial_lower_bound_edges = -12.0, initial_upper_bound_edges = -4.0, 
    max_calib_iterations = 25, density_conv_tolerance = 0.0005, num_sim_networks = 5,
    control_simulate_ergm_options, verbose_calibration = TRUE) {
  
  N_nodes <- network.size(atp_base_network_input)
  lower_bound <- initial_lower_bound_edges
  upper_bound <- initial_upper_bound_edges
  
  calibrated_edges_val <- NA
  final_avg_density <- NA
  
  full_ergm_formula <- update.formula(formula_homofilia_terms, paste("~ edges + ."))
  
  if (verbose_calibration) {
    cat(paste("Iniciando calibración de 'edges' para N =", N_nodes, "y densidad objetivo =", target_density_input, "\n"))
    cat("-----------------------------------------------------------------\n")
  }
  
  iter <- 0
  for (i in 1:max_calib_iterations) {
    iter <- i
    current_edges_try <- (lower_bound + upper_bound) / 2
    current_full_coefs <- c(edges = current_edges_try, fixed_homophily_coefs)
    
    if (verbose_calibration) {
      cat(paste("Iteración", i, ": Probando coef_edges =", round(current_edges_try, 5), "\n"))
    }
    
    sim_networks_list_iter <- tryCatch({
      simulate(full_ergm_formula, basis = atp_base_network_input, nsim = num_sim_networks,
               coef = current_full_coefs, control = control_simulate_ergm_options, verbose = FALSE)
    }, error = function(e) {
      if (verbose_calibration) cat("   Error en simulación:", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if (is.null(sim_networks_list_iter)) {
      upper_bound <- current_edges_try
      next
    }
    
    avg_sim_density_iter <- mean(sapply(sim_networks_list_iter, network.density), na.rm = TRUE)
    final_avg_density <- avg_sim_density_iter
    
    if(is.nan(avg_sim_density_iter) || is.na(avg_sim_density_iter)){
      upper_bound <- current_edges_try
      next
    }
    
    if (verbose_calibration) cat(paste("   Densidad promedio simulada:", round(avg_sim_density_iter, 5), "\n"))
    
    if (abs(avg_sim_density_iter - target_density_input) < density_conv_tolerance) {
      calibrated_edges_val <- current_edges_try
      if (verbose_calibration) cat(paste("Convergencia alcanzada. Coef_edges calibrado =", round(calibrated_edges_val, 5), "\n"))
      break
    }
    
    if (avg_sim_density_iter < target_density_input) {
      lower_bound <- current_edges_try
    } else {
      upper_bound <- current_edges_try
    }
    
    if (i == max_calib_iterations) {
      calibrated_edges_val <- current_edges_try
      if (verbose_calibration) cat(paste("Máximo de iteraciones. Coef_edges final (aprox) =", round(calibrated_edges_val, 5), "\n"))
    }
  }
  
  return(list(calibrated_coef_edges = calibrated_edges_val, achieved_density = final_avg_density, iterations_run = iter))
}

TARGET_DENSITY_GSS <- 0.029
control_sim_settings <- control.simulate.formula(
  MCMC.burnin = 1000 * N_gss_1000,
  MCMC.interval = 100 * N_gss_1000
)

edges_calibration_result <- calibrate_edges_coefficient(
  atp_base_network_input = gss_base_network_1000,
  target_density_input = TARGET_DENSITY_GSS,
  formula_homofilia_terms = formula_homofilia_only,
  fixed_homophily_coefs = coef_homofilia_fijos,
  control_simulate_ergm_options = control_sim_settings
)

print(edges_calibration_result) #$calibrated_coef_edges -> [1] -4.34375 , $achieved_density -> [1] 0.02916196


# --- 5. Simulación Final de 100 Redes ---

final_gss_coefs <- c(edges = edges_calibration_result$calibrated_coef_edges, coef_homofilia_fijos)
full_formula_gss <- update.formula(formula_homofilia_only, paste("~ edges + ."))
N_SIMULACIONES <- 100

output_dir <- "data/02_GSS_network_ergm"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cl <- makeCluster(8, type = "FORK")
registerDoParallel(cl)

invisible(
  foreach(i = 1:N_SIMULACIONES, .packages = c("network", "ergm")) %dopar% {
    set.seed(2024 + i)
    
    gss_network_simulated <- simulate(
      full_formula_gss,
      basis = gss_base_network_1000,
      nsim = 1,
      coef = final_gss_coefs,
      control = control_sim_settings,
      verbose = FALSE
    )
    
    saveRDS(gss_network_simulated, file.path(output_dir, sprintf("GSS_net_sim_1000_%03d.rds", i)))
  }
)

stopCluster(cl)

cat(paste("\nSimulación completada. Se guardaron", N_SIMULACIONES, "redes en la carpeta '", output_dir, "'.\n"))


# --- 6. Verificación Rápida de una Red Simulada ---

first_simulated_net <- readRDS(file.path(output_dir, "GSS_net_sim_1000_001.rds"))

cat("\n--- Resumen de la primera red simulada ---\n")
print(summary(first_simulated_net, print.adj = FALSE))
cat("\nAtributos de los nodos:\n")
print(list.vertex.attributes(first_simulated_net))
cat("\nDensidad de la red simulada:", network.density(first_simulated_net), "\n")
