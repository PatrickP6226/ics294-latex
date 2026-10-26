# =====================================================================
# Proyecto Econometría ICS-294 (USM, 2026-2)
# Salarios de CEOs y desempeño de las empresas — base ceosal1 (wooldridge)
# Integrantes: Jorge Figueroa, Patrick Peñaloza, Joaquín Rubio,
#              Felipe Hormachea, Felipe Vidal, Fernanda Nuñez
#
# Script con todo el código del informe. Es el mismo código del
# cuaderno de Colab proyecto_ceosal1.ipynb, sin el texto explicativo.
# Cómo usarlo en RStudio: abrir el archivo, seleccionar todo (Ctrl+A)
# y ejecutar (Ctrl+Enter). Los resultados aparecen en la consola y los
# gráficos en el panel "Plots".
# =====================================================================

# #####################################################################
# Salarios de CEOs y desempeño de las empresas (ceosal1)
# #####################################################################

# ============================================================
# 0. Dependencias
# ============================================================

# Paquetes que usa el cuaderno:
#  - wooldridge: contiene la base ceosal1
#  - dplyr / tidyr: manipulación de datos
#  - ggplot2: gráficos
#  - corrplot: gráfico de la matriz de correlaciones
#  - lmtest: pruebas de Breusch-Pagan, RESET y coeftest()
#  - sandwich: errores estándar robustos (vcovHC)
#  - car: VIF y pruebas F de restricciones lineales
#  - tseries: prueba de normalidad de Jarque-Bera
paquetes <- c("wooldridge", "dplyr", "tidyr", "ggplot2", "corrplot",
              "lmtest", "sandwich", "car", "tseries")

# Se instalan solo los paquetes que no estén disponibles
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar)) install.packages(instalar)

# Se cargan todos los paquetes
invisible(lapply(paquetes, library, character.only = TRUE))

# #####################################################################
# 1. Descripción de la base de datos
# #####################################################################

# --- 1.1 Origen y tipo de datos ---

# --- 1.2 Diccionario de variables ---

# ============================================================
# 1. Carga de datos
# ============================================================

data("ceosal1")          # carga la base desde el paquete wooldridge
ceo <- ceosal1           # copia de trabajo

dim(ceo)                 # número de observaciones y de variables
str(ceo)                 # tipo de cada variable
head(ceo)                # primeras filas

# --- 1.3 Revisión de datos nulos y consistencia ---

# Valores perdidos por columna y en total
colSums(is.na(ceo))
sum(is.na(ceo))

# Cada empresa debe pertenecer a un solo sector
# (la suma de las cuatro dummies debe ser 1)
table(ceo$indus + ceo$finance + ceo$consprod + ceo$utility)

# lsalary y lsales deben ser el logaritmo de salary y sales
all.equal(ceo$lsalary, log(ceo$salary), tolerance = 1e-4)
all.equal(ceo$lsales,  log(ceo$sales),  tolerance = 1e-4)

# Variable categórica de sector, útil para tablas y gráficos
ceo$sector <- factor(
  case_when(ceo$indus == 1    ~ "Industrial",
            ceo$finance == 1  ~ "Financiero",
            ceo$consprod == 1 ~ "Consumo",
            ceo$utility == 1  ~ "Utilities/transporte"),
  levels = c("Industrial", "Financiero", "Consumo",
             "Utilities/transporte"))
table(ceo$sector)

# #####################################################################
# 2. Estadística descriptiva
# #####################################################################

# --- 2.1 Medidas de tendencia central y dispersión ---

# ============================================================
# 2. Estadística descriptiva
# ============================================================

# Asimetría y curtosis calculadas con momentos muestrales
asimetria <- function(x) {
  m <- mean(x)
  mean((x - m)^3) / mean((x - m)^2)^(3/2)
}
curtosis <- function(x) {
  m <- mean(x)
  mean((x - m)^4) / mean((x - m)^2)^2
}

# Función que resume una variable numérica
resumen <- function(x) {
  c(Media     = mean(x),
    Mediana   = median(x),
    DE        = sd(x),
    CV        = sd(x) / mean(x),
    Min       = min(x),
    Q1        = unname(quantile(x, 0.25)),
    Q3        = unname(quantile(x, 0.75)),
    Max       = max(x),
    RIC       = IQR(x),
    Asimetria = asimetria(x),
    Curtosis  = curtosis(x))
}

vars_continuas <- c("salary", "lsalary", "sales", "lsales",
                    "roe", "ros", "pcsalary", "pcroe")

tabla_desc <- t(sapply(ceo[, vars_continuas], resumen))

# Tendencia central y dispersión
round(tabla_desc[, c("Media", "Mediana", "DE", "CV", "Min", "Max")], 3)

# Posición y forma de la distribución
round(tabla_desc[, c("Q1", "Q3", "RIC", "Asimetria", "Curtosis")], 3)

# --- 2.2 Variables binarias: composición por sector ---

# Proporción de empresas en cada sector
dummies <- c("indus", "finance", "consprod", "utility")
round(sapply(ceo[, dummies], mean), 3)

# Salario del CEO por sector
ceo %>%
  group_by(sector) %>%
  summarise(n               = n(),
            salario_medio   = mean(salary),
            salario_mediana = median(salary),
            lsalary_medio   = mean(lsalary)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Tamaño y rentabilidad promedio de las empresas por sector
ceo %>%
  group_by(sector) %>%
  summarise(ventas_medias = mean(sales),
            roe_medio     = mean(roe),
            ros_medio     = mean(ros)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# --- 2.3 Correlaciones ---

# Matriz de correlaciones de las variables del modelo
vars_modelo <- ceo[, c("lsalary", "lsales", "roe", "ros",
                       "finance", "consprod", "utility")]
round(cor(vars_modelo), 3)

# Pruebas de significancia de la correlación con lsalary
p_valores <- sapply(c("lsales", "roe", "ros"),
                    function(v) cor.test(ceo$lsalary, ceo[[v]])$p.value)
signif(p_valores, 3)

# Visualización
corrplot(cor(vars_modelo), method = "number", type = "upper",
         tl.col = "black", number.cex = 0.8)

# #####################################################################
# 3. Gráficos descriptivos
# #####################################################################

# --- 3.1 Distribución del salario: niveles y logaritmo ---

# Histogramas del salario en niveles y en logaritmo, uno al lado del otro
ceo %>%
  select(salary, lsalary) %>%
  pivot_longer(everything(),
               names_to = "variable", values_to = "valor") %>%
  mutate(variable = dplyr::recode(variable,
                                  salary  = "Salario (miles de US$)",
                                  lsalary = "Logaritmo del salario"),
         variable = factor(variable,
                           levels = c("Salario (miles de US$)",
                                      "Logaritmo del salario"))) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribución del salario de los CEOs (1990)",
       x = NULL, y = "Frecuencia") +
  theme_minimal()

# --- 3.2 Distribución de las ventas: niveles y logaritmo ---

ceo %>%
  select(sales, lsales) %>%
  pivot_longer(everything(),
               names_to = "variable", values_to = "valor") %>%
  mutate(variable = dplyr::recode(variable,
                                  sales  = "Ventas (millones de US$)",
                                  lsales = "Logaritmo de las ventas"),
         variable = factor(variable,
                           levels = c("Ventas (millones de US$)",
                                      "Logaritmo de las ventas"))) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribución de las ventas de las empresas (1990)",
       x = NULL, y = "Frecuencia") +
  theme_minimal()

# --- 3.3 Salario y tamaño de la empresa ---

ggplot(ceo, aes(x = lsales, y = lsalary)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "firebrick") +
  labs(title = "Logaritmo del salario vs logaritmo de las ventas",
       x = "log(ventas)", y = "log(salario)") +
  theme_minimal()

# --- 3.4 Salario y desempeño contable (ROE) ---

ggplot(ceo, aes(x = roe, y = lsalary)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "firebrick") +
  labs(title = "Logaritmo del salario vs ROE",
       x = "ROE promedio 1988-1990 (%)", y = "log(salario)") +
  theme_minimal()

# --- 3.5 Salario y desempeño bursátil (rentabilidad de la acción) ---

ggplot(ceo, aes(x = ros, y = lsalary)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "firebrick") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Logaritmo del salario vs rentabilidad de la acción",
       x = "Rentabilidad de la acción 1988-1990 (%)",
       y = "log(salario)") +
  theme_minimal()

# --- 3.6 Salario según sector ---

ggplot(ceo, aes(x = sector, y = lsalary, fill = sector)) +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
               show.legend = FALSE) +
  labs(title = "Logaritmo del salario según sector de la empresa",
       subtitle = "El rombo indica la media de cada sector",
       x = "Sector", y = "log(salario)") +
  theme_minimal()

# --- 3.7 Valores atípicos ---

# Las cinco remuneraciones más altas de la muestra
top5 <- ceo[order(-ceo$salary),
            c("salary", "sales", "roe", "ros", "sector")]
head(top5, 5)

# Número de observaciones fuera de 1,5 RIC en cada variable
fuera_ric <- function(x) {
  q <- quantile(x, c(0.25, 0.75)); r <- IQR(x)
  sum(x < q[1] - 1.5 * r | x > q[2] + 1.5 * r)
}
sapply(ceo[, c("salary", "lsalary", "sales", "lsales", "roe", "ros")],
       fuera_ric)

# #####################################################################
# 4. Planteamiento del modelo e hipótesis de investigación
# #####################################################################

# --- 4.1 Pregunta de investigación ---

# --- 4.2 Variable dependiente ---

# --- 4.3 Variables explicativas ---

# --- 4.4 Modelo de regresión poblacional ---

# --- 4.5 Hipótesis de investigación ---

# #####################################################################
# 5. Estimación del modelo
# #####################################################################

# ============================================================
# 5. Estimación del modelo por MCO
# ============================================================

# lm() estima el modelo por MCO; summary() muestra coeficientes,
# errores estándar, estadísticos t, p-valores, R2 y prueba F global
modelo <- lm(lsalary ~ lsales + roe + ros + finance + consprod + utility,
             data = ceo)
summary(modelo)

# Tabla ordenada de coeficientes
coefs <- summary(modelo)$coefficients
tabla_mco <- data.frame(Coeficiente    = coefs[, 1],
                        Error_estandar = coefs[, 2],
                        t              = coefs[, 3],
                        p_valor        = coefs[, 4])
round(tabla_mco, 5)

# Efecto porcentual exacto de las dummies de sector: 100 * (exp(b) - 1)
dummies_sector <- c("finance", "consprod", "utility")
round(100 * (exp(coef(modelo)[dummies_sector]) - 1), 1)

# --- 5.1 Interpretación de los coeficientes ---

# --- 5.2 Bondad de ajuste ---

# #####################################################################
# 6. Verificación de los supuestos del modelo
# #####################################################################

# --- 6.1 Exogeneidad ---

# ============================================================
# 6.1 Exogeneidad
# ============================================================

# Por construcción, los residuos de MCO no están correlacionados con
# los regresores. Esto NO prueba la exogeneidad; solo se muestra
# para dejar claro que el supuesto no puede verificarse así.
round(cor(residuals(modelo), ceo[, c("lsales", "roe", "ros")]), 4)

# Prueba RESET de Ramsey (H0: la forma funcional es correcta).
# Agrega los valores ajustados al cuadrado y al cubo al modelo.
resettest(modelo, power = 2:3, type = "fitted")

# --- 6.2 Homocedasticidad ---

# ============================================================
# 6.2 Homocedasticidad
# ============================================================

# Gráfico de residuos versus valores ajustados
diag_df <- data.frame(ajustados = fitted(modelo),
                      residuos  = residuals(modelo))
ggplot(diag_df, aes(x = ajustados, y = residuos)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "firebrick", linetype = "dashed") +
  labs(title = "Residuos versus valores ajustados",
       x = "Valores ajustados de log(salario)", y = "Residuos") +
  theme_minimal()

# Prueba de Breusch-Pagan
bptest(modelo)

# Prueba de White: regresores, sus cuadrados y sus interacciones
# (las interacciones entre dummies de sector se omiten porque son
# siempre cero: una empresa pertenece a un solo sector)
white <- bptest(modelo,
  ~ lsales + roe + ros + finance + consprod + utility +
    I(lsales^2) + I(roe^2) + I(ros^2) +
    lsales:roe + lsales:ros + roe:ros +
    lsales:finance + lsales:consprod + lsales:utility +
    roe:finance + roe:consprod + roe:utility +
    ros:finance + ros:consprod + ros:utility,
  data = ceo)
white

# --- 6.3 Normalidad de los errores ---

# ============================================================
# 6.3 Normalidad
# ============================================================

jarque.bera.test(residuals(modelo))
shapiro.test(residuals(modelo))

# Gráfico QQ de los residuos
ggplot(data.frame(residuos = residuals(modelo)), aes(sample = residuos)) +
  stat_qq(alpha = 0.6) +
  stat_qq_line(color = "firebrick") +
  labs(title = "Gráfico QQ de los residuos",
       x = "Cuantiles teóricos (normal)",
       y = "Cuantiles de los residuos") +
  theme_minimal()

# Observaciones con residuo estandarizado mayor que 3 en valor absoluto
res_est <- rstandard(modelo)
cbind(ceo[abs(res_est) > 3, c("salary", "sales", "roe", "ros", "sector")],
      residuo_est = round(res_est[abs(res_est) > 3], 2))

# --- 6.4 Multicolinealidad ---

# ============================================================
# 6.4 Multicolinealidad
# ============================================================

round(vif(modelo), 3)

# #####################################################################
# 7. Correcciones
# #####################################################################

# ============================================================
# 7. Corrección: errores estándar robustos HC1
# ============================================================

# vcovHC() calcula la matriz de varianzas robusta y coeftest()
# recalcula los estadísticos t y los p-valores con ella
vcov_hc1 <- vcovHC(modelo, type = "HC1")
robusto <- coeftest(modelo, vcov = vcov_hc1)
robusto

# Comparación de errores estándar y p-valores: MCO vs robustos
comparacion <- data.frame(Coeficiente = coef(modelo),
                          EE_MCO      = sqrt(diag(vcov(modelo))),
                          EE_robusto  = sqrt(diag(vcov_hc1)),
                          p_MCO       = coefs[, 4],
                          p_robusto   = robusto[, 4])
round(comparacion, 5)

# #####################################################################
# 8. Testeo de las hipótesis de investigación
# #####################################################################

# ============================================================
# 8. Testeo de hipótesis con el modelo corregido (HC1)
# ============================================================

gl <- modelo$df.residual          # grados de libertad: n - k - 1 = 202
t_rob <- robusto[, "t value"]     # estadísticos t robustos

pruebas <- data.frame(
  Hipotesis  = c("H1: b_lsales > 0", "H2: b_roe > 0",
                 "H3: b_ros > 0", "H5: b_utility < 0"),
  t_robusto  = round(unname(t_rob[c("lsales", "roe", "ros",
                                    "utility")]), 3),
  p_una_cola = signif(c(pt(t_rob["lsales"], gl, lower.tail = FALSE),
                        pt(t_rob["roe"], gl, lower.tail = FALSE),
                        pt(t_rob["ros"], gl, lower.tail = FALSE),
                        pt(t_rob["utility"], gl, lower.tail = TRUE)), 3),
  row.names = NULL)
pruebas$Decision_5 <- ifelse(pruebas$p_una_cola < 0.05,
                             "Se rechaza H0", "No se rechaza H0")
pruebas

# H4: prueba F robusta de que roe y ros son conjuntamente cero
linearHypothesis(modelo, c("roe = 0", "ros = 0"), white.adjust = "hc1")

# #####################################################################
# 9. Análisis de robustez
# #####################################################################

# --- 9.1 Modelo sin valores atípicos ---

# ============================================================
# 9.1 Robustez: modelo sin valores atípicos
# ============================================================

ceo_sin <- ceo[abs(res_est) <= 3, ]
nrow(ceo_sin)

modelo_sin <- lm(lsalary ~ lsales + roe + ros +
                   finance + consprod + utility, data = ceo_sin)
coeftest(modelo_sin, vcov = vcovHC(modelo_sin, type = "HC1"))
summary(modelo_sin)$r.squared

# La normalidad mejora mucho al excluir los valores atípicos
jarque.bera.test(residuals(modelo_sin))

# --- 9.2 Forma funcional: tamaño con efecto cuadrático ---

# ============================================================
# 9.2 Robustez: término cuadrático del tamaño
# ============================================================

modelo_cuad <- lm(lsalary ~ lsales + I(lsales^2) + roe + ros +
                    finance + consprod + utility, data = ceo)
coeftest(modelo_cuad, vcov = vcovHC(modelo_cuad, type = "HC1"))
summary(modelo_cuad)$adj.r.squared

# ¿Mejora la forma funcional?
resettest(modelo_cuad, power = 2:3, type = "fitted")

# Elasticidad salario-ventas evaluada en el promedio de lsales
b <- coef(modelo_cuad)
b["lsales"] + 2 * b["I(lsales^2)"] * mean(ceo$lsales)

# Nivel de ventas (millones de USD) en que la elasticidad se hace cero
punto_max <- exp(-b["lsales"] / (2 * b["I(lsales^2)"]))
punto_max
sum(ceo$sales > punto_max)        # empresas sobre ese nivel

# H4 en el modelo cuadrático
linearHypothesis(modelo_cuad, c("roe = 0", "ros = 0"),
                 white.adjust = "hc1")

# --- 9.3 Comparación de los tres modelos ---

# ============================================================
# 9.3 Comparación de coeficientes entre modelos (p-valores robustos)
# ============================================================

fila <- function(m, v) {
  r <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
  p <- ifelse(r[v, 4] < 0.001, "p < 0.001", sprintf("p = %.3f", r[v, 4]))
  sprintf("%.4f (%s)", r[v, 1], p)
}
vars_interes <- c("roe", "ros", "finance", "consprod", "utility")

data.frame(
  Variable     = vars_interes,
  Principal    = sapply(vars_interes, function(v) fila(modelo, v)),
  Sin_atipicos = sapply(vars_interes, function(v) fila(modelo_sin, v)),
  Cuadratico   = sapply(vars_interes, function(v) fila(modelo_cuad, v)),
  row.names = NULL)

# #####################################################################
# 10. Conclusiones, limitaciones y recomendaciones
# #####################################################################

# --- 10.1 Principales hallazgos ---

# --- 10.2 Limitaciones ---

# --- 10.3 Recomendaciones y mejoras ---

# #####################################################################
# Referencias
# #####################################################################
