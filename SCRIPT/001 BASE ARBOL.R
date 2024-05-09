install.packages("tree",dependencies = TRUE)
install.packages("rpart",dependencies = TRUE)
install.packages("rpar.plot",dependencies = TRUE)

library(rpart)
library(rpart.plot)
library(dplyr)
library(tidyverse)

base <- read_excel("DATA/PRUEBA COORDINADOR DM.XLSX",sheet = "Base 1") %>% 
  clean_names() %>% 
  mutate(codigo_id = as.character(codigo_id)) %>% 
  filter(!is.na(codigo_id))

modelo_arbol<-rpart(marca_mora_tarjeta~edad + 
                      saldo_utiliz_prom_cliente , 
                      #antiguedad_tarjeta_anios + 
                      #maximo_num_dias_vencido +
                      #numero_operaciones_titular ,
                      # riesgo_cliente_total_gfp +
                      # segmento_riesgo +
                      # forma_pago + 
                      # saldo_total_tarjeta +
                      # cupo_promedio_tarjeta +
                      # promedio_mensual_consumos_locales +
                      # marca_cuenta_corriente + 
                      # marca_cuenta_ahorros +  
                      # sucursal +
                      # #valor_deposito_a_plazo +
                      # instruccion,
                    data = base_entrenamiento)


predict(modelo_arbol,
        base_test,
        type = "class")

rpart.plot(modelo_arbol)

# OTRO ARBOL --------------------------------------------------------------

str(base_entrenamiento)
base_entrenamiento$marca_mora_tarjeta <- factor(base_entrenamiento$marca_mora_tarjeta)

modelo_arbol_2 <- rpart(marca_mora_tarjeta ~ .,
                        data = base_entrenamiento %>% 
                          select(- codigo_id,-fecha,-num_tc_sist_fim))

rpart.plot(modelo_arbol_2)

modelo_arbol_2

base_test <- base_test %>% 
  select(- codigo_id,-fecha,-num_tc_sist_fim)

prediccion_2 <- predict(modelo_arbol_2,
                        newdata = base_test,
                        type = "class")






