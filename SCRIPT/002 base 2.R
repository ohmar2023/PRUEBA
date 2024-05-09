

# LECTURA DE BASE ---------------------------------------------------------

base_2 <- read_excel("DATA/PRUEBA COORDINADOR DM.XLSX",sheet = "Base 2") %>% 
  clean_names() 

names(base_2)
str(base_2)


# 1y 2 En que mes se da la mejor recuperación de cartera. ------------------

base_2 %>% group_by(mes_corte) %>% 
  summarise(Valor_recaudado_mes = sum(cuotas_pagadas)) %>% 
  arrange() %>% 
  View()


# 3, En el mes de Mayo, cual es la probalilidad que un cliente que --------
# se encuentra en etapa preventiva, termine a primera etapa

base_mayo <- base_2 %>% filter(mes_corte==5)
base_mayo %>% filter(etapa_normal=="02. PRIMERA" &
                       etapa_asignacion=="01. PREVENTIVA") %>% dim() -> ab
base_mayo %>% filter(etapa_asignacion=="01. PREVENTIVA") %>% dim() -> b

p_a_b <-  ab[1]/dim(base_mayo)[1] 
p_b <- b[1]/dim(base_mayo)[1]

p_a_b/p_b


#En el mes de Junio, cual es la probalilidad que un cliente que se encuentra 
#en primera etapa, termine en tercera etapa.

base_jun <- base_2 %>% filter(mes_corte==6)
base_jun %>% filter(etapa_normal=="03. TERCERA" &
                       etapa_asignacion=="02. PRIMERA") %>% dim() -> ab2
base_mayo %>% filter(etapa_asignacion=="02. PRIMERA") %>% dim() -> b2

p_a_b_2 <-  ab2[1]/dim(base_jun)[1] 
p_b_2<- b2[1]/dim(base_jun)[1]

p_a_b_2/p_b_2

#, Considere la fecha de corte septiembre, que empresa de cobranza tiene 
# mayor efectividad de recaudo.

base_2 %>% filter(mes_corte==9) %>% 
  group_by(empresa_cob) %>% 
  summarise(MontoCobrado = sum(cuotas_pagadas)) %>% 
  arrange(MontoCobrado) %>% 
  View()

# Que relación tiene el score de cobranza con la etapa de normal 
# de la cartera..?

CrossTable(base_2$descripcion_score_cobranza,
           base_2$etapa_normal)

base_2 %>% group_by(descripcion_score_cobranza,
                    etapa_normal) %>% 
  summarise(n=n()) %>% 
  View()

base_2 %>%
  ggplot(aes(x = descripcion_score_cobranza, 
             fill = etapa_normal)) +
  geom_bar(position = "fill", width = 0.5) + 
  scale_fill_brewer(type = "qual", palette = 3) +
  theme_minimal()

library(DescTools)

CramerV(base_2$descripcion_score_cobranza,
        base_2$etapa_normal)*100


#Cual es la región con mayor efectividad en recaudo y por qué?

base_2 %>% group_by(region) %>% 
  summarise(MontoCobrado = sum(cuotas_pagadas),
            n = n()) %>% 
  View()
  



