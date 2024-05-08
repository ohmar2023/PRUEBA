install.packages("tree",dependencies = TRUE)
install.packages("rpart",dependencies = TRUE)
install.packages("rpar.plot",dependencies = TRUE)

library(rpart)
library(rpart.plot)

modelo_arbol<-rpart(marca_mora_tarjeta~edad + 
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
                      instruccion,, 
                    data = base_entrenamiento)


plot(modelo_arbol)
text(modelo_arbol)

predict(modelo_arbol,
        base_test,
        type = "class")




