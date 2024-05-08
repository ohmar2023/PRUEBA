
library(readxl)
library(tidyverse)
library(gmodels) 
library(dplyr)
library(janitor)

base <- read_excel("DATA/PRUEBA COORDINADOR DM.XLSX",sheet = "Base 1") %>% 
  clean_names() %>% 
  mutate(codigo_id = as.character(codigo_id),
         marca_mora_tarjeta = as.numeric(marca_mora_tarjeta)
         ) %>% 
  filter(!is.na(codigo_id))
base_entrenamiento$marca_mora_tarjeta <- as.integer(base_entrenamiento$marca_mora_tarjeta)

summary(base)

base %>% View()

CrossTable(base$INSTRUCCION)
CrossTable(base$SUCURSAL)
CrossTable(base$INSTRUCCION)
CrossTable(base$saldo_utiliz_prom_cliente, 
           base$segmento_riesgo, 
           prop.r = TRUE,            
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)

CrossTable(base$segmento_riesgo)

summary(base)


# Conjunto de entrenamiento -----------------------------------------------

set.seed(502) 
f <- dim(base)[1]
i_train <- sample(1:f, 2/3*f ) 
base_entrenamiento <- base[i_train, ] 
base_test <- base[-i_train, ]


table(base$loan_status,model_pred)


# Modelo ------------------------------------------------------------------


modelos_a <- glm(marca_mora_tarjeta~edad + 
      saldo_utiliz_prom_cliente + 
      antiguedad_tarjeta_anios + 
      maximo_num_dias_vencido +
      numero_operaciones_titular + 
      riesgo_cliente_total_gfp +
      segmento_riesgo +
      forma_pago + 
      saldo_total_tarjeta +
      cupo_promedio_tarjeta +
      promedio_mensual_consumos_locales +
      marca_cuenta_corriente + 
      marca_cuenta_ahorros +  
      sucursal +
      #valor_deposito_a_plazo +
      instruccion,
    data = base_entrenamiento)

predicciones_a <- predict(modelos_a,newdata = base_test, type = "response")
range(predicciones_a)

str(base_entrenamiento)

predicciones_a[]
predicciones_a <- predicciones_a %>% data.frame()












