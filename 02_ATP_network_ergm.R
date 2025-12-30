# ------------------------------------------------------------------------------
# Genera redes simuladas 'ATP' mediante ERGM calibrados para reproducir la 
# densidad y patrones de homofilia observados en los datos ATP.
# Las redes (N=1000, 100 realizaciones) contienen atributos sociodemográficos.
# Gráficos comparativos y análisis de ajuste (GOF). 
# Genera:
#   trabajo_1_files/ATP_network_ergm/ATP_network_simulated_1000_XXX.rds
# ------------------------------------------------------------------------------
library(ergm) # Para simulate.ergm y network
library(dplyr)
library(haven)
library(doParallel)

# --- 1. Preparación Datos ---

ATP_W3_W4 <- readRDS("data/01_ATP_GSS_imputation/ATP_W3_W4_input.rds")
N_atp <- nrow(ATP_W3_W4)

gss_egor <- readRDS("data/01_ATP_GSS_imputation/gss_egor.rds")
gss_egos <- gss_egor$ego
gss_alters <- gss_egor$alter

# Nos aseguramos que las categorías sean factores
if (!is.factor(ATP_W3_W4$sex)) ATP_W3_W4$sex <- factor(ATP_W3_W4$sex, labels = c("Male", "Female")) # Ajusta si es necesario
if (!is.factor(ATP_W3_W4$race)) ATP_W3_W4$race <- factor(ATP_W3_W4$race)
if (!is.factor(ATP_W3_W4$relig)) ATP_W3_W4$relig <- factor(ATP_W3_W4$relig)

# Variables de atributos para ergm
demographic_vars <- c("age", "sex", "educ_num", "race", "relig")
metech_vars <- c("METECH_A", "METECH_B", "METECH_C", 
                 "METECH_D", "METECH_E", "METECH_F")
attribute_vars <- c(demographic_vars, metech_vars)

for (m_var in metech_vars) {
  if (m_var %in% names(ATP_W3_W4)) {
    # Obtenemos valores numéricos
    numeric_values <- haven::zap_labels(ATP_W3_W4[[m_var]])
    
    ATP_W3_W4[[m_var]] <- case_when(
      numeric_values == 0 ~ 0,
      numeric_values == 1 ~ 1,
      TRUE ~ NA_real_ # NA's y 99
    )
  }
}

# Seleccionamos casos completos
ATP_W3_W4 <- ATP_W3_W4[complete.cases(ATP_W3_W4[, attribute_vars]), ] # Quitamos NA's
N_atp <- nrow(ATP_W3_W4) # Número de individuos en ATP

# Submuestreo para red 1000 casos.
set.seed(987)
ATP_W3_W4_1000 <- ATP_W3_W4[sample(nrow(ATP_W3_W4), 1000), ]
N_atp_1000 <- nrow(ATP_W3_W4_1000)

# Creamos el objeto Network (sin lazos, solo nodos y atributos)
atp_base_network_1000 <- network.initialize(N_atp_1000, directed = FALSE)
set.vertex.attribute(atp_base_network_1000, "age", ATP_W3_W4_1000$age)
set.vertex.attribute(atp_base_network_1000, "sex", as.character(ATP_W3_W4_1000$sex))
set.vertex.attribute(atp_base_network_1000, "educ_num", ATP_W3_W4_1000$educ_num)
set.vertex.attribute(atp_base_network_1000, "race", as.character(ATP_W3_W4_1000$race))
set.vertex.attribute(atp_base_network_1000, "relig", as.character(ATP_W3_W4_1000$relig))
set.vertex.attribute(atp_base_network_1000, "metech_a", as.numeric(ATP_W3_W4_1000$METECH_A))
set.vertex.attribute(atp_base_network_1000, "metech_b", as.numeric(ATP_W3_W4_1000$METECH_B))
set.vertex.attribute(atp_base_network_1000, "metech_c", as.numeric(ATP_W3_W4_1000$METECH_C))
set.vertex.attribute(atp_base_network_1000, "metech_d", as.numeric(ATP_W3_W4_1000$METECH_D))
set.vertex.attribute(atp_base_network_1000, "metech_e", as.numeric(ATP_W3_W4_1000$METECH_E))
set.vertex.attribute(atp_base_network_1000, "metech_f", as.numeric(ATP_W3_W4_1000$METECH_F))

# Fórmula ERGM (el término 'edge' se iterará)
formula_homofilia_only <- ~ nodematch("race") +
                            nodematch("sex") +
                            absdiff("age") +
                            absdiff("educ_num") +
                            nodematch("relig")

# Smith et al. modelan P(lazo | Diferencia), no P(lazo | Coincidencia).
# El signo de los coeficientes en ergm para 'nodematch' será POSITIVO si la similitud aumenta la probabilidad.
# Los coeficientes de Smith et al. son para 'Different_X', entonces para 'nodematch.X' el signo se invierte.

# Coeficientes de Smith (2014), Tabla 3, Modelo 2 "All Ties" - AJUSTADOS PARA 'nodematch'
# Year_ij_val = 1 (para ATP, análogo a 2004 GSS)
beta_s2014_raw <- c(Different_Race = -1.959, Different_Religion = -1.270, Different_Sex = -0.373,
                    Age_Difference = -0.047, Education_Difference = -0.157, Year_2004 = -0.052,
                    Diff_Race_x_Year = 0.264, Diff_Relig_x_Year = -0.215, Diff_Sex_x_Year = 0.144,
                    Age_Diff_x_Year = -0.005, Educ_Diff_x_Year = -0.044) # AGREGAR TAMBIÉN AQUÍ 'YEAR' ??

# Coeficientes efectivos para ATP (asumimos que 2004 es lo más cercano)
coef_eff_diff_race  <- beta_s2014_raw["Different_Race"] + beta_s2014_raw["Diff_Race_x_Year"]
coef_eff_diff_relig <- beta_s2014_raw["Different_Religion"] + beta_s2014_raw["Diff_Relig_x_Year"]
coef_eff_diff_sex   <- beta_s2014_raw["Different_Sex"] + beta_s2014_raw["Diff_Sex_x_Year"]
coef_eff_absdiff_age  <- beta_s2014_raw["Age_Difference"] + beta_s2014_raw["Age_Diff_x_Year"]
coef_eff_absdiff_educ <- beta_s2014_raw["Education_Difference"] + beta_s2014_raw["Educ_Diff_x_Year"]

# El coef de Smith et al. (-1.695 para raza efectiva en 2004) es la log-odds de lazar
# si eres de DIFERENTE raza comparado con la misma raza (si el intercepto captura la misma raza).
# Entonces, para nodematch("race") en ergm, el coeficiente debería ser POSITIVO si la misma raza es más probable.
# Es decir, -1 * (coef_eff_diff_race)
# Los términos `absdiff` ya tienen el signo correcto (negativo indica que mayor diferencia disminuye lazos)

coef_homofilia_fijos <- setNames(
  c(-coef_eff_diff_race, 
    -coef_eff_diff_sex,
    coef_eff_absdiff_age, 
    coef_eff_absdiff_educ,
    -coef_eff_diff_relig), 
  c("nodematch.race",   # Para nodematch("race")
    "nodematch.sex",    # Para nodematch("sex") # Aquí podría ser nodematch.sex.FEMALE si Male es ref.
    "absdiff.age",      # Para absdiff("age")
    "absdiff.educ_num", # Para absdiff("educ_num")
    "nodematch.relig")  # Para nodematch("relig")
)


# --- 1. función de optimización para 'edges' ---

calibrate_edges_coefficient <- function(
    atp_base_network_input,
    target_density_input,
    formula_homofilia_terms, # Fórmula solo con términos de homofilia (ej. ~ nodematch("race") + ...)
    fixed_homophily_coefs,   # Vector nombrado con coeficientes para esos términos
    initial_lower_bound_edges = -10.0,
    initial_upper_bound_edges = -2.0,
    max_calib_iterations = 20,
    density_conv_tolerance = 0.001,
    num_sim_networks = 5,
    control_simulate_ergm_options, # Objeto de control para simulate()
    verbose_calibration = TRUE) {
  
  # Número de nodos y bordes
  N_nodes <- network.size(atp_base_network_input)
  lower_bound <- initial_lower_bound_edges
  upper_bound <- initial_upper_bound_edges
  
  # Objeto para guardar resultados
  calibrated_edges_val <- NA
  final_avg_density <- NA
  
  # Fórmula ERGM completa que incluye 'edges'
  full_ergm_formula <- update.formula(formula_homofilia_terms, paste("~ edges + ."))
  
  if (verbose_calibration) {
    cat(paste("Iniciando calibración de 'edges' para N =", N_nodes, "y densidad objetivo =", target_density_input, "\n"))
    cat("-----------------------------------------------------------------\n")
  }
  
  # Ciclo principal
  for (iter in 1:max_calib_iterations) {
    current_edges_try <- (lower_bound + upper_bound) / 2
    
    # Combinar el coeficiente de edges actual con los coeficientes de homofilia fijos
    current_full_coefs <- c(edges = current_edges_try, fixed_homophily_coefs)
    
    if (verbose_calibration) {
      cat(paste("Iteración", iter, "de", max_calib_iterations, ": Probando coef_edges =", round(current_edges_try, 5), "\n"))
    }
    
    # simulación de num_sim_networks redes
    sim_networks_list_iter <- tryCatch({
      simulate(
        full_ergm_formula,
        basis = atp_base_network_input,
        nsim = num_sim_networks,
        coef = current_full_coefs,
        control = control_simulate_ergm_options,
        verbose = FALSE # Evitar el verboso de la simulación interna aquí
      )
    }, error = function(e) {
      if (verbose_calibration) {
        cat("   Error durante la simulación:", conditionMessage(e), "\n")
        cat("   Coeficientes probados:", paste(names(current_full_coefs), round(current_full_coefs,3), collapse=", "), "\n")
      }
      return(NULL)
    })
    
    if (is.null(sim_networks_list_iter)) {
      if (verbose_calibration) cat("   Fallo en la simulación. Ajustando límites de búsqueda...\n")
      if (abs(current_edges_try - lower_bound) < 1e-6 || abs(current_edges_try - upper_bound) < 1e-6) {
        warning("Límites de búsqueda muy juntos y la simulación sigue fallando. Deteniendo calibración.")
        calibrated_edges_val <- (lower_bound + upper_bound) / 2 # Mejor intento
        break
      }
      upper_bound <- current_edges_try # Asumir que falló por ser demasiado positivo/denso
      next
    }
    
    # Cálculo densidades
    sim_densities_iter <- sapply(sim_networks_list_iter, network.density)
    avg_sim_density_iter <- mean(sim_densities_iter, na.rm = TRUE)
    final_avg_density <- avg_sim_density_iter # Guardar la última densidad promedio calculada
    
    if(is.nan(avg_sim_density_iter) || is.na(avg_sim_density_iter)){
      if (verbose_calibration) cat("   Densidad promedio NaN/NA. Ajustando límites de búsqueda...\n")
      if (abs(current_edges_try - lower_bound) < 1e-6 || abs(current_edges_try - upper_bound) < 1e-6) {
        warning("Límites de búsqueda muy juntos y la simulación sigue produciendo NaN/NA. Deteniendo calibración.")
        calibrated_edges_val <- (lower_bound + upper_bound) / 2
        break
      }
      upper_bound <- current_edges_try
      next
    }
    
    if (verbose_calibration) {
      cat(paste("   Densidad promedio simulada:", round(avg_sim_density_iter, 5), "\n"))
    }
    
    # Revisión convergencia dentro de tolerancia
    if (abs(avg_sim_density_iter - target_density_input) < density_conv_tolerance) {
      calibrated_edges_val <- current_edges_try
      if (verbose_calibration) {
        cat(paste("Convergencia alcanzada en iteración", iter, ". Coef_edges calibrado =", round(calibrated_edges_val, 5), "\n"))
      }
      break
    }
    
    # Recalcular bordes para próxima iteración
    if (avg_sim_density_iter < target_density_input) {
      lower_bound <- current_edges_try
    } else {
      upper_bound <- current_edges_try
    }
    
    # Máximo iteraciones
    if (iter == max_calib_iterations) {
      calibrated_edges_val <- current_edges_try
      if (verbose_calibration) {
        cat(paste("Máximo de iteraciones alcanzado. Coef_edges final (aproximado) =", round(calibrated_edges_val, 5), "\n"))
        cat(paste("   Densidad obtenida con este coeficiente:", round(avg_sim_density_iter, 5), "\n"))
      }
    }
  }
  
  if (verbose_calibration) cat("-----------------------------------------------------------------\n")
  
  return(list(calibrated_coef_edges = calibrated_edges_val, 
              achieved_density = final_avg_density,
              iterations_run = iter,
              N_nodes = N_nodes))
}


# --- 2. Ejcución función optimización ---

# REVISAR convergencia
control_sim_formula <- control.simulate.formula( 
  MCMC.burnin = 1000 * N_atp,
  MCMC.interval = 100 * N_atp
)

# Para ATP submuestra N=1000 como base
edges_var_info_1000 <- calibrate_edges_coefficient(
  atp_base_network_input = atp_base_network_1000,   # base clase network (N=1000)
  formula_homofilia_terms = formula_homofilia_only, # Fórmula solo con términos de homofilia (ej. ~ nodematch("race") + ...) 
  fixed_homophily_coefs = coef_homofilia_fijos,  # Vector nombrado, con coeficientes para esos términos
  target_density_input = 0.029,           # Densidad target
  initial_lower_bound_edges = -10.0,      # Límite inferior para el coeficiente de edges
  initial_upper_bound_edges = -2.0,       # Límite superior
  max_calib_iterations = 20,              # Número máximo de iteraciones
  density_conv_tolerance = 0.001,         # tolerancia al target_density
  num_sim_networks = 5,             # Número de redes a simular en cada paso para promediar densidad
  control_simulate_ergm_options = control_sim_formula # Objeto de control para simulate()
)

print(edges_var_info_1000) # $calibrated_coef_edges -> [1] -4.3125 , $achieved_density -> [1] 0.02851892

# Las densidades de ambas bases son prácticamente iguales para 'edges' = -4.3125
# Si solo tuviera 'edges': 
# p = exp(θ_edges) / (1 + exp(θ_edges)) --> coef_edges_teo <- log(0.029 / (1 - 0.029))
# La densidad es controlada principalmente por 'edges' y los efectos relativos de homofilia se mantienen.


# --- 3. Simulación nueva red ---

library(network)
library(sna)

# Fórmula completa
full_formula_ergm <- update.formula(formula_homofilia_only, paste("~ edges + ."))
final_ergm_coefs <- c(edges = edges_var_info_1000$calibrated_coef_edges, coef_homofilia_fijos)

n_simulaciones <- 100

cl <- makeCluster(8, type = "FORK") 
registerDoParallel(cl)

# --- Loop paralelo ---
invisible(
  foreach(i = 1:n_simulaciones, 
          .packages = c("network", "ergm"), 
          .errorhandling = "pass") %dopar% {
            
            # Fijar semilla única por iteración
            set.seed(123 + i)
            
            # Simular red
            ATP_network_simulated_1000 <- simulate(
              full_formula_ergm,
              basis = atp_base_network_1000,
              nsim = 1,
              coef = final_ergm_coefs,
              control = control_sim_formula,
              verbose = FALSE
            )
            
            # Guardar
            saveRDS(ATP_network_simulated_1000, sprintf("data/02_ATP_network_ergm/ATP_net_sim_1000_%03d.rds", i))
          }
)

stopCluster(cl)

# --- Comparación demográficos ATP original vs. ERGM simulada ---

# Asegúrate que los nombres de los atributos son los mismos que en el dataframe original
ATP_network_simulated_1000_atr <- data.frame(
  age = get.vertex.attribute(ATP_network_simulated_1000, "age"),
  sex = get.vertex.attribute(ATP_network_simulated_1000, "sex"),
  educ_num = get.vertex.attribute(ATP_network_simulated_1000, "educ_num"),
  race = get.vertex.attribute(ATP_network_simulated_1000, "race"),
  relig = get.vertex.attribute(ATP_network_simulated_1000, "relig")
)
# Convertir a factores si es necesario para la comparación (ej. si en la red son character)
ATP_network_simulated_1000_atr$sex <- factor(ATP_network_simulated_1000_atr$sex, levels = c("Male", "Female"))
ATP_network_simulated_1000_atr$race <- factor(ATP_network_simulated_1000_atr$race, levels = levels(ATP_W3_W4$race))
ATP_network_simulated_1000_atr$relig <- factor(ATP_network_simulated_1000_atr$relig, levels = levels(ATP_W3_W4$relig))

library(ggplot2)

plots_comparacion <- list()

for (attr_name in attribute_vars) {
  
  df_original <- data.frame( # dataframe original ATP
    valor = ATP_W3_W4[[attr_name]],
    fuente = "ATP Original (Base N=1000)"
  )
  df_simulado <- data.frame( # Datos de la red simulada ERGM
    valor = ATP_network_simulated_1000_atr[[attr_name]],
    fuente = "Red ERGM Simulada (N=1000)"
  )
  df_combinado <- rbind(df_original, df_simulado)
  
  # Para CATEGÓRICOS
  if (is.factor(df_combinado$valor) || is.character(df_combinado$valor)) {
    cat("Prop. en original ATP:\n")
    print(prop.table(table(df_original$valor)))
    cat("Prop. en ERGM simulado:\n")
    print(prop.table(table(df_simulado$valor)))
    
    # Barras comparativo
    p <- ggplot(df_combinado, aes(x = valor, fill = fuente)) +
      geom_bar(position = "dodge", aes(y = ..prop.., group = fuente)) + # ..prop.. --> proporciones
      labs(title = paste("Distribución de", attr_name), x = attr_name, y = "Proporción") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots_comparacion[[paste0(attr_name, "_bar")]] <- p
    
  } else if (is.numeric(df_combinado$valor)) { # Para NUMÉRICOS
    cat("Resumen original ATP:\n"); print(summary(df_original$valor))
    cat("Resumen ERGM simulado:\n"); print(summary(df_simulado$valor))
    
    # Densidad comparativa
    p <- ggplot(df_combinado, aes(x = valor, fill = fuente, color = fuente)) +
      geom_density(alpha = 0.5, adjust=1.5) + # adjust para suavizar más o menos
      labs(title = paste("Distribución de", attr_name), x = attr_name, y = "Densidad") +
      theme_minimal()
    plots_comparacion[[paste0(attr_name, "_dens")]] <- p
  }
  print(p) # Mostrar el gráfico actual
  cat("--------------------------------\n")
}

# Histograma (gmode="graph" para no dirigida)
hist(degree(ATP_network_simulated_1000, gmode="graph"), main="Distribución de Grado (ATP Simulada)",
     xlab="Grado", ylab="Frecuencia", breaks=50)

# Goodness of Fit  - GOF -
gof_simulado <- gof(
  full_formula_ergm,
  coef = final_ergm_coefs,
  basis = ATP_network_simulated_1000,
  control = control.gof.formula(nsim=100, MCMC.burnin=100000, MCMC.interval=10000), # Ajustar según sea necesario
  verbose = TRUE
)
plot(gof_simulado)
print(gof_simulado) # Los p-value < 0.05 indican problema: diferencia significativa entre realizaciones.
