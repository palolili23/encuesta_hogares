library(here)
library(tidyverse)
library(rio)
library(magrittr)
source(here("01_R", "funciones.R"))

# Datos de base: Persona 2019 ---------------------------------------------

## Importar base de datos personas

persona2019_crudo <- import(here("002_eh", "Base EH2019", "EH2019_Persona.sav"))

## Generar fecha de nacimiento

persona2019 <- persona2019_crudo %>%
  unite(fecha_nac, c(s02a_04a, s02a_04b, s02a_04c), sep = "/")

## Cambiar los nombres de las variables

persona2019 %<>% 
  rename(sexo = s02a_02,
         edad = s02a_03,
         lengua_materna = s02a_08,
         migro_cinco_anos = s03a_01a,
         etnia_pertenece = s03a_04,
         tipo_etnia = s03a_04npioc,
         enfermedad_cronica_1 = s04a_01a,
         enfermedad_cronican_2 = s04a_01b,
         enfermedad_cronican_e = s04a_01e,
         enfermedad_infecciosa_1 = s04a_02a,
         enfermedad_infecciosa_2 = s04a_02b,
         enfermedad_infecciosa_e = s04a_02e,
         acudio_cs_1 = s04a_03a,
         acudio_cs_2 = s04a_03b,
         acudio_cs_3 = s04a_03c,
         acudio_cs_4 = s04a_03d,
         acudio_cs_5 = s04a_03e,
         acudio_cs_6 = s04a_03f,
         acudio_cs_7 = s04a_03g,
         filiacion_seguro_1 = s04a_04a,
         filiacion_seguro_2 = s04a_04b,
         filiacion_seguro_e = s04a_04e,
         costo_consulta = s04a_05a,
         costo_aparato = s04a_05b,
         costo_internacion = s04a_05c,
         costo_examenes = s04a_05d,
         costo_medicina = s04a_05e,
         embarazo_previo = s04b_11a,
         embarazo_numero = s04b_11b,
         embarazo_vivos = s04b_12,
         embarazo_vivos_actual = s04b_13,
         embarazo_ultimo_mes = s04b_14a,
         embarazo_ultimo_anio = s04b_14b,
         parto_atencion = s04b_15,
         parto_atencion_e = s04b_15e,
         parto_no_medico = s04b_16,
         parto_lugar = s04b_17,
         parto_lugar_e = s04b_17e,
         parto_seguro = s04b_18,
         parto_seguro_e = s04b_18,
         parto_bono_inscripcion = s04b_19,
         parto_bono_cobro_pren = s04b_20a1,
         parto_bono_cobro_pren_num = s04b_20a2,
         parto_bono_cobro_post = s04b_20b,
         parto_sup_emb = s04b_21a,
         parto_sup_meses = s04b_21b,
         parto_sup_meses_num = s04b_21b2,
         asistio_centro_infantil = s04c_22 ,
         asistio_centro_infantil_tipo = s04c_23,
         tuvo_diarrea_2sem = s04d_24,
         tuvo_ira = s04d_25,
         inscripcion_bono_juana = s04d_26,
         cobro_bono_juana = s04d_27a,
         cobro_bono_juana_num = s04d_27b,
         tabaquismo = s04e_32a,
         tabaquismo_frec = s04e_32b,
         alcohol = s04e_33a,
         alcohol_frec = s04e_33b,
         seguridad_noche = s04f_34,
         violencia_1 = s04f_35a,
         violencia_2 = s04f_35b,
         violencia_3 = s04f_35c,
         violencia_e = s04f_35e,
         alfabetismo = s05a_01,
         matematica = s05a_01a,
         curso_max_instruccion = s05a_02a,
         curso_aprobado = s05a_03a,
         curso_matricula = s05a_04,
         curso_desertado = s05a_05,
         curso_inscripcion = s05a_06a,
         curso_establecimiento = s05a_09,
         curso_asistencia = s05b_10,
         curso_asistencia_motivo = s05b_11,
         curso_bullying = s05c_14a,
         curso_bullying_e = s05c_14b,
         curso_bullying_motivo = s05c_15a,
         curso_bullying_motivo_e = s05c_15b,
         trabajo_una_hora = s06a_01,
         trabajo_hora_actividad = s06a_02,
         trabajo_previo = s06a_07,
         trabajo_previo_tiempo_a = s06a_08a,
         trabajo_previo_tiempo_b = s06a_08b,
         trabajo_condicion = s06a_09,
         trabajo_condicion_e = s06a_09e, 
         trabajo_motivo = s06a_10,
         trabajo_motivo_e = s06a_10e,
         trabajo_ocupacion = s06b_11a,
         trabajo_ocupacion_cod = s06b_11a_cod,
         trabajo_tareas = s06b_11b,
         trabajo_establecimiento = s06b_12a)

## Seleccionar sólo las variables que renombramos

persona2019 %<>% 
  select(-starts_with("s0"))

## Utiliza función para reemplazar etiquetas de SPPS y generar factores

persona2019 %<>%
  map(relabel_spss_variable) %>% as_tibble()

## Limpieza de categorías generadas

persona2019 %<>% 
  mutate(across(where(is.factor), ~str_replace(.x, "[:digit:]*[.]", ""))) %>% 
  mutate(across(where(is.character), ~str_remove(.x, "[?]$"))) %>% 
  mutate(across(where(is.character), ~str_remove(.x, "^[¿]"))) %>% 
  mutate(across(where(is.character), ~str_trim(.x, side = "both"))) %>% 
  mutate(across(where(is.character), ~str_to_sentence(.x))) 


# Datos de base: Vivienda ----------------------------------------------------------------

vivienda2019_crudo <- import(here("002_eh", "Base EH2019", "EH2019_Vivienda.sav"))

vivienda2019 <- vivienda2019_crudo %>% 
  map(relabel_spss_variable) %>% as_tibble()

vivienda2019 %<>% 
  rename(
    vivienda_tipo = s01a_01,
    vivienda_propiedad = s01a_02,
    vivienda_revoque = s01a_07,
    vivienda_techo = s01a_08,
    vivienda_piso = s01a_09, 
    fuente_agua = s01a_10,
    fuente_agua_e = s01a_10e,
    agua_lavado_manos = s01a_13,
    productos_higiene = s01a_14_1,
    servicio_sanitario = s01a_15,
    desague = s01a_16,
    servicio_sanitario_propio = s01a_17)

vivienda2019 %<>% 
  select(-starts_with("s01"))

vivienda2019 %<>% 
  mutate(across(where(is.factor), ~str_replace(.x, "[:digit:]*([.]|[.¿])", ""))) %>% 
  mutate(across(where(is.character), ~str_remove(.x, "[?]$"))) %>% 
  mutate(across(where(is.character), ~str_remove(.x, "^[¿]"))) %>% 
  mutate(across(where(is.character), ~str_trim(.x, side = "both"))) %>% 
  mutate(across(where(is.character), ~str_to_sentence(.x))) 


# Unir bases --------------------------------------------------------------

base_completa <- persona2019 %>% 
  left_join(vivienda2019)

export(base_completa, here("002_eh", "base_analisis", "base_vivienda_persona.xlsx"))
export(base_completa, here("002_eh", "base_analisis", "base_vivienda_persona.RData"))
