#### TAREA INDICES

# ¿Agradable o apropiada para vivir?
# Construir un índice de “....” para las ciudades

base <- readxl::read_excel("latino.xlsx")

#Densidad -  Hab/km2
#Congestion - nivel de congestión
#Seguridad - Percepción de seguridad (valor ponderado de acuerdo a la inversión en seguridad)
#HorasSol - horas promedio de sol al año
#PolRuido - nivel de contaminación acustica
#PolLuz - nivel de contaminacion luminosa
#Desempleo - tasa de desempleo
#Deuda - nivel promedio de deuda global
#Ssocial - nivel de seguridad social (laboral, salud, jubilación)
#SaludMen - tasa promedio de suicidios
#SaludFis - nivel de salud fisica (gasto en salud, nacimientos, etc)
#IgualGen - tasa de participacion laboral según género
#IgualRaz - tasa de participación laboral según raza
#Transporte - nivel de satisfacción del transporte público
#PoderCompra - costo de vida (ingreso / gasto familiar)
#PolAire - nivel de contaminacion ambiental
#AreaVerde - metros cuadrados de área verde / km

# Usted, junto a su grupo, deben discutir y decidir:
# Dimensiones <- qué variables (al menos dos)
# Como mezclar las dimensiones (al menos dos)

# Obtener el indicador y analizar el orden.
# Obs: Los valores ya han sido “normalizados” de acuerdo al criterio IDH y un valor cercano al 10 es bueno, y cercano al 1 es malo
