library(dplyr)
library(fastDummies)
library(tidyr)

dir_temporal  <- setwd("C:/Users/a.aybarf/Downloads/pm_algoritmo_r/R_PM_2024")

pobreza24 <-read.csv(file.path(dir_temporal,"Base final/pobreza_24.csv"))
pob24 <-read.csv(file.path(dir_temporal,"Bases de datos/poblacion.csv"))
tra24 <-read.csv(file.path(dir_temporal,"Bases de datos/trabajos.csv"))
hog24 <-read.csv(file.path(dir_temporal,"Bases de datos/hogares.csv"))
viv24 <-read.csv(file.path(dir_temporal,"Bases de datos/viviendas.csv"))

# Variables a conservar de la base de pobreza
vars <- c("folioviv", "foliohog", "numren", "est_dis", "upm", "factor", 
          "tam_loc", "rururb", "ent", "edad", "sexo", "parentesco",
          "pobreza", "pobreza_e", "pobreza_m", "plp_e", "plp", 
          "ic_rezedu", "ic_asalud", "ic_segsoc", "ic_cv", "ic_sbv", 
          "ic_ali_nc", "ic_ali", "i_privacion", "anac_e", "inas_esc", 
          "pea", "jub", "pam", "id_men", "tot_iaad", "tot_iamen", 
          "ins_ali", "tamhogesc", "ictpc", "ict", "ing_mon", "ing_lab", 
          "ing_ren", "ing_tra", "nomon", "pago_esp", "reg_esp", "hli")

nombres <- c("pobreza24")

for (nombre in nombres) {
  df <- get(nombre)
  df <- df[, vars]
  df$tam_loc <- as.factor(df$tam_loc)
  df$ent <- as.factor(df$ent)
  df$parentesco <- factor(substr(as.character(df$parentesco), 1, 1))
  df$mujer <- ifelse(df$sexo == 2, 1, 0)
  assign(nombre, df)
}

# Variables adicionales de cada ENIGH:

## Poblacion

pob24 <- pob24 %>%
  mutate(asispriv = factor(case_when(tipoesc == 2 ~ 1,tipoesc %in% c(1, 3) ~ 0))) %>%
  mutate(inst_sal = case_when(inst_1 == 1 ~ 1,inst_2 == 2 | inst_3 == 3 | inst_4 == 4 ~ 2,
                              inst_5 == 5 ~ 3,inst_5 == 6 ~ 4,TRUE ~ NA_real_)) %>%
  mutate(raz_s_sal = case_when(inscr_1 == 1 ~ 1,inscr_2 == 2 ~ 2,
                               inscr_3 == 3 | inscr_4 == 4 | inscr_7 == 7 ~ 3,
                               inscr_5 == 5 ~ 4,inscr_6 == 6 ~ 5,inscr_8 == 8 ~ NA_real_,TRUE ~ NA_real_)) %>%
  mutate(
    d_disca = if_else(act_pnea1 == 5 | act_pnea2 == 5, 1L, 0L, missing = 0L),
    d_hogar = if_else(act_pnea1 == 3 | act_pnea2 == 3, 1L, 0L, missing = 0L),
    d_jubil = if_else(act_pnea1 == 2 | act_pnea2 == 2, 1L, 0L, missing = 0L),
    d_estud = if_else(act_pnea1 == 4 | act_pnea2 == 4, 1L, 0L, missing = 0L)) %>%
  mutate(
    alfabetism = factor(case_when(alfabetism == 1 ~ 1,alfabetism == 2 ~ 0,TRUE ~ NA_real_)),
    tiene_b = factor(case_when(tiene_b == 1 ~ 1,tiene_b == 2 ~ 0,TRUE ~ NA_real_)),
    segsoc = factor(case_when(segsoc == 1 ~ 1,segsoc == 2 ~ 0,TRUE ~ NA_real_)),
    atemed = factor(case_when(atemed == 1 ~ 1,atemed == 2 ~ 0,TRUE ~ NA_real_)),
    diabetes = factor(case_when(diabetes == 1 ~ 1,diabetes == 2 ~ 0,TRUE ~ NA_real_)),
    pres_alta = factor(case_when(pres_alta == 1 ~ 1,pres_alta == 2 ~ 0,TRUE ~ NA_real_)),
    peso = factor(case_when(peso == 1 ~ 1,peso == 2 ~ 0,TRUE ~ NA_real_)),
    trabajo_mp = factor(case_when(trabajo_mp == 1 ~ 1,trabajo_mp == 2 ~ 0,
                                  motivo_aus %in% 1:6 ~ 1,TRUE ~ NA_real_)),
    t_trab = case_when(usotiempo1 == 8 ~ NA_real_,
                       usotiempo1 == 9 ~ 0,TRUE ~ hor_1),
    trab_comun = case_when(hor_3 >= 0 & hor_3 <= 99 ~ 1,
                           usotiempo3 == 9 ~ 0,TRUE ~ 0),
    t_cuid = case_when(usotiempo4 == 8 ~ NA_real_,
                       usotiempo4 == 9 ~ 0,TRUE ~ hor_4),
    t_qhac = case_when(usotiempo6 == 8 ~ NA_real_,
                       usotiempo6 == 9 ~ 0,TRUE ~ hor_6),
    t_acarr = case_when(usotiempo7 == 8 ~ NA_real_,
                        usotiempo7 == 9 ~ 0,TRUE ~ hor_7)) %>%
  select( folioviv, foliohog, numren, 
          parentesco,sexo,edad, 
          alfabetism,nivel,grado,tiene_b,nivelaprob,gradoaprob,antec_esc,segsoc,
          t_trab, trab_comun, t_cuid, t_qhac, t_acarr,
          atemed, inst_sal, raz_s_sal,
          diabetes, pres_alta, peso, trabajo_mp,
          d_disca, d_hogar, d_jubil, d_estud)

hog24 <- hog24 %>%
  mutate(
    tiemp_hosp = tsalud1_h + tsalud1_m / 60,
    comp_alim_m = if_else(habito_1 == 1, 1L, 0L, missing = 0L),
    comp_alim_q = if_else(habito_2 == 2, 1L, 0L, missing = 0L),
    comp_alim_s = if_else(habito_3 == 3, 1L, 0L, missing = 0L),
    comp_alim_t = if_else(habito_4 == 4, 1L, 0L, missing = 0L),
    comp_alim_d = if_else(habito_5 == 5, 1L, 0L, missing = 0L),
    acc_alim1 = if_else(acc_alim1 == 1, 1L, 0L, missing = 0L),
    acc_alim2 = if_else(acc_alim2 == 1, 1L, 0L, missing = 0L),
    acc_alim3 = if_else(acc_alim3 == 1, 1L, 0L, missing = 0L),
    acc_alim4 = if_else(acc_alim4 == 1, 1L, 0L, missing = 0L),
    acc_alim5 = if_else(acc_alim5 == 1, 1L, 0L, missing = 0L),
    acc_alim6 = if_else(acc_alim6 == 1, 1L, 0L, missing = 0L),
    acc_alim7 = if_else(acc_alim7 == 1, 1L, 0L, missing = 0L),
    acc_alim8 = if_else(acc_alim8 == 1, 1L, 0L, missing = 0L),
    acc_alim9 = if_else(acc_alim9 == 1, 1L, 0L, missing = 0L),
    acc_alim10 = if_else(acc_alim10 == 1, 1L, 0L, missing = 0L),
    acc_alim11 = if_else(acc_alim11 == 1, 1L, 0L, missing = 0L),
    acc_alim12 = if_else(acc_alim12 == 1, 1L, 0L, missing = 0L),
    acc_alim13 = if_else(acc_alim13 == 1, 1L, 0L, missing = 0L),
    acc_alim14 = if_else(acc_alim14 == 1, 1L, 0L, missing = 0L),
    acc_alim15 = if_else(acc_alim15 == 1, 1L, 0L, missing = 0L),
    acc_alim16 = if_else(acc_alim16 == 1, 1L, 0L, missing = 0L),
    dias_cereales = alim17_1,
    dias_tuberculos = alim17_2,
    dias_verduras = alim17_3,
    dias_frutas = alim17_4,
    dias_carne = alim17_5,
    dias_huevo = alim17_6,
    dias_pescado = alim17_7,
    dias_leguminosas = alim17_8,
    dias_lacteos = alim17_9,
    dias_aceites = alim17_10,
    dias_azucar = alim17_11,
    dias_condimentos = alim17_12,
    telefono = factor(if_else(telefono == 1, 1, 0)),
    celular = factor(if_else(celular == 1, 1, 0)),
    tv_paga = factor(if_else(tv_paga == 1, 1, 0)),
    conex_inte = factor(if_else(conex_inte == 1, 1, 0)),
    dauto = if_else(num_auto == 0, 0, 1),
    dvan = if_else(num_van == 0, 0, 1),
    dmoto = if_else(num_moto == 0, 0, 1),
    dbici = if_else(num_bici == 0, 0, 1),
    dtrici = if_else(num_trici == 0, 0, 1),
    dcanoa = if_else(num_canoa == 0, 0, 1),
    dotro = if_else(num_otro == 0, 0, 1),
    dester = if_else(num_ester == 0, 0, 1),
    dradio = if_else(num_radio == 0, 0, 1),
    dtva = if_else(num_tva == 0, 0, 1),
    dtvd = if_else(num_tvd == 0, 0, 1),
    ddvd = if_else(num_dvd == 0, 0, 1),
    dlicua = if_else(num_licua == 0, 0, 1),
    dtosta = if_else(num_tosta == 0, 0, 1),
    dmicro = if_else(num_micro == 0, 0, 1),
    drefri = if_else(num_refri == 0, 0, 1),
    destuf = if_else(num_estuf == 0, 0, 1),
    dlavad = if_else(num_lavad == 0, 0, 1),
    dplanc = if_else(num_planc == 0, 0, 1),
    dmaqui = if_else(num_maqui == 0, 0, 1),
    dventi = if_else(num_venti == 0, 0, 1),
    daspir = if_else(num_aspir == 0, 0, 1),
    dcompu = if_else(num_compu == 0, 0, 1),
    dimpre = if_else(num_impre == 0, 0, 1),
    djuego = if_else(num_juego == 0, 0, 1),
    tarjeta = if_else(tarjeta == 1, 1, 0),
    pagotarjet = if_else(tarjeta == 0 & is.na(pagotarjet), 0, if_else(pagotarjet == 1, 1, 0)),
    autocons = if_else(autocons == 1, 1, 0),
    regalos = if_else(regalos == 1, 1, 0),
    remunera = if_else(remunera == 1, 1, 0),
    transferen = if_else(transferen == 1, 1, 0),
    negcua = if_else(negcua == 1, 1, 0),
    gas_alim_e = est_alim,
    gas_tran_e = est_trans,
    lgasalime = asinh(est_alim),
    lgastrans = asinh(est_trans),
    bene_licon = if_else(bene_licon == 1, 1, 0),
    diconsa = if_else(diconsa == 1, 1, 0)) %>%
  select( folioviv, foliohog, 
          tiemp_hosp, 
          acc_alim1,acc_alim2,acc_alim3,acc_alim4, acc_alim5, acc_alim6, 
          acc_alim7,acc_alim8,acc_alim9,acc_alim10, acc_alim11, acc_alim12,
          acc_alim13, acc_alim14, acc_alim15, acc_alim16,
          dias_cereales, dias_tuberculos, dias_verduras, dias_frutas, 
          dias_carne, dias_huevo, dias_pescado,dias_leguminosas, 
          dias_lacteos, dias_aceites, dias_azucar, dias_condimentos,
          telefono,celular,tv_paga,conex_inte,
          num_auto,dauto,num_van,dvan,num_moto,dmoto, 
          num_bici,dbici,num_trici,dtrici,num_canoa,dcanoa,
          num_ester,dester,num_radio,dradio,num_tva,dtva,
          num_tvd,dtvd,num_dvd,ddvd, 
          num_licua,dlicua,num_tosta,dtosta,num_micro,dmicro,num_refri,drefri, 
          num_estuf,destuf,num_lavad,dlavad,num_planc,dplanc,num_maqui,dmaqui, 
          num_venti,dventi,num_aspir,daspir,num_compu,dcompu,num_impre,dimpre,
          num_juego,djuego,
          tarjeta,pagotarjet,autocons,regalos,remunera,transferen,negcua,
          comp_alim_m,comp_alim_q,comp_alim_s,comp_alim_t,comp_alim_d,
          gas_alim_e,gas_tran_e,lgasalime,lgastrans,bene_licon,diconsa )

library(dplyr)

viv24 <- viv24 %>%
  mutate(
    agua_diaria = ifelse(ab_agua %in% c(1, 2) & dotac_agua == 1, 1, 0),
    agua_2sem = ifelse(ab_agua %in% c(1, 2) & dotac_agua %in% c(2, 3), 1, 0),
    tipo_viv = as.factor(replace(tipo_viv, grepl("[^0-9]", tipo_viv), NA)),
    tipo_viv = as.factor(ifelse(tipo_viv %in% c(4,5), 3, tipo_viv)),
    mat_pared = as.factor(case_when(mat_pared %in% 1:5 ~ 1,
                                    mat_pared == 6 ~ 2,mat_pared == 7 ~ 3,mat_pared == 8 ~ 4,TRUE ~ NA_real_)),
    mat_techos = as.factor(case_when(mat_techos %in% 1:6 ~ 1,
                                     mat_techos == 7 ~ 2,mat_techos %in% 8:9 ~ 3,
                                     mat_techos == 10 ~ 4,TRUE ~ NA_real_)),
    mat_pisos = as.factor(replace(mat_pisos, grepl("[^0-9]", mat_pisos), NA)),
    cocina = as.factor(ifelse(cocina == 1, 1, 0)),
    cocina_dor = ifelse(cocina == 0 & is.na(cocina_dor), 0, cocina_dor),
    cocina_dor = as.factor(ifelse(cocina_dor == 1, 1, 0)),
    disp_agua = as.factor(case_when(ab_agua %in% c(3, 4, 5) ~ 3,
                                    ab_agua %in% c(6, 7) ~ 4,TRUE ~ ab_agua)),
    excusado = case_when(excusado == 1 & sanit_agua == 1 ~ 1,
                         excusado == 1 & sanit_agua == 2 ~ 2,excusado == 1 & sanit_agua == 3 ~ 3,
                         excusado == 2 ~ 4,TRUE ~ NA_real_) %>% 
      as.factor(),
    bano_comp = ifelse(is.na(bano_comp), 0, bano_comp),
    d_bano_comp = as.factor(ifelse(bano_comp != 0, 1, 0)),
    bano_excus = ifelse(is.na(bano_excus), 0, bano_excus),
    d_bano_excus = as.factor(ifelse(bano_excus != 0, 1, 0)),
    bano_regad = ifelse(is.na(bano_regad), 0, bano_regad),
    d_bano_regad = as.factor(ifelse(bano_regad != 0, 1, 0)),
    drenaje = as.factor(case_when(drenaje %in% c(3, 4) ~ 3,
                                  drenaje == 5 ~ 4,TRUE ~ drenaje)),
    disp_elect = as.factor(case_when(disp_elect %in% c(2, 3, 4) ~ 2,
                                     disp_elect == 5 ~ 3,TRUE ~ disp_elect)),
    focos_ahor = replace(focos_ahor, is.na(focos_ahor), 0),
    eli_basura = as.factor(case_when(eli_basura %in% c(2,3) ~ 2,
                                     eli_basura %in% 4:8 ~ 3,TRUE ~ eli_basura)),
    tenencia = as.factor(ifelse(tenencia == 6, 5, tenencia)),
    renta = ifelse(renta %in% c(-1,1), NA, renta),
    estim_pago = ifelse(estim_pago %in% c(-1,1), NA, estim_pago),
    pago_viv = ifelse(pago_viv %in% c(-1,1), NA, pago_viv),
    across(c(lavadero, fregadero, regadera, tinaco_azo, cisterna, pileta,
             calent_sol, calent_gas, bomba_agua, tanque_gas,
             aire_acond, calefacc),
           ~ as.factor(ifelse(.x == 1, 1, 0)),
           .names = "{.col}" )) %>%
  mutate(renta_imp1 = pmin(renta, estim_pago, pago_viv, na.rm = TRUE),
         renta_imp2 = pmax(renta, estim_pago, pago_viv, na.rm = TRUE)) %>%
  select(folioviv, tipo_viv, mat_pared, mat_techos, mat_pisos, antiguedad, cocina, cocina_dor, cuart_dorm, num_cuarto,
         ab_agua, disp_agua, agua_diaria, agua_2sem, excusado, bano_comp, d_bano_comp, bano_excus, d_bano_excus,
         bano_regad, d_bano_regad, drenaje, disp_elect, focos_ahor,
         eli_basura, tenencia, renta, estim_pago, pago_viv, renta_imp1, renta_imp2,
         lavadero, fregadero, regadera, tinaco_azo, cisterna, pileta,
         calent_sol, calent_gas, bomba_agua, tanque_gas,
         aire_acond, calefacc,tot_resid, tot_hom, tot_muj, tot_hog, ubica_geo, est_socio)
viv24 <- viv24 %>%
  mutate(renta_imp1 = ifelse(is.infinite(renta_imp1), NA, renta_imp1),
         renta_imp2 = ifelse(is.infinite(renta_imp2), NA, renta_imp2))

## Trabajo

tra24 <- tra24 %>%
  filter(id_trabajo == 1) %>%
  mutate(
    pres_ninguna = case_when(pres_20==20~1,is.na(pres_20)~0,TRUE~0),
    trapais = ifelse(trapais == 2 | is.na(trapais), 0, trapais),
    subor = ifelse(subor == 2 | is.na(subor), 0, subor),
    indep = ifelse(indep == 2 | is.na(indep), 0, indep),
    personal = ifelse(personal == 2 | is.na(personal), 0, personal),
    pago = as.factor(ifelse(is.na(pago), 0, pago)),
    contrato = ifelse(contrato == 2 | is.na(contrato), 0, contrato),
    tipocontr = ifelse(tipocontr == 1 | is.na(tipocontr), 0, 1),
    sinco = as.factor(ifelse(sinco >= 1000 & sinco <= 9999, substr(sinco, 1, 1), 0)),
    scian = as.factor(ifelse(scian >= 1000 & scian <= 9999, substr(scian, 1, 1), 0)),
    clas_emp = as.factor(ifelse(is.na(clas_emp), 0, clas_emp)),
    tam_emp = as.factor(ifelse(tam_emp == 12, NA, tam_emp))
  ) %>%
  select(folioviv, foliohog, numren, trapais, subor, indep, personal, pago,
         contrato, tipocontr, pres_ninguna, htrab, sinco, scian,
         clas_emp, tam_emp)

# Integración de bases

pobreza24 <- merge(pobreza24,pob24,by=c("folioviv","foliohog","numren"))
pobreza24 <- merge(pobreza24,tra24, by = c("folioviv", "foliohog", "numren"), all = TRUE)
pobreza24 <- merge(pobreza24,hog24, by = c("folioviv", "foliohog"), all = TRUE)
pobreza24 <- merge(pobreza24,viv24, by = c("folioviv"), all = TRUE)

rm(list = setdiff(ls(), c("pobreza24")))

################
# Ajustes 2024 #
################

pobreza24 <- pobreza24 %>% 
  select(-edad.y, -sexo.y) %>% 
  rename( edad = edad.x,sexo = sexo.x,
          paren = parentesco.x,
          parentesco = parentesco.y )

# Variables a nivel hogar
pobreza24 <- pobreza24 %>%
  group_by(folioviv, foliohog) %>%
  mutate(
    n_noasis       = sum(inas_esc == 1, na.rm = TRUE),
    d_noasis       = as.integer(any(inas_esc == 1, na.rm = TRUE)),
    n_noasis_017   = sum(inas_esc == 1 & edad >= 0 & edad <= 17, na.rm = TRUE),
    d_noasis_017   = as.integer(any(inas_esc == 1 & edad >= 0 & edad <= 17, na.rm = TRUE)),
    n_noasis_35    = sum(inas_esc == 1 & edad >= 3 & edad <= 5, na.rm = TRUE),
    d_noasis_35    = as.integer(any(inas_esc == 1 & edad >= 3 & edad <= 5, na.rm = TRUE)),
    n_noasis_611   = sum(inas_esc == 1 & edad >= 6 & edad <= 11, na.rm = TRUE),
    d_noasis_611   = as.integer(any(inas_esc == 1 & edad >= 6 & edad <= 11, na.rm = TRUE)),
    n_noasis_1217  = sum(inas_esc == 1 & edad >= 12 & edad <= 17, na.rm = TRUE),
    d_noasis_1217  = as.integer(any(inas_esc == 1 & edad >= 12 & edad <= 17, na.rm = TRUE))
  ) %>%
  ungroup()

pobreza24 <- pobreza24 %>%
  mutate(nivel= case_when(nivel>=1 & nivel<=5~1,nivel==6~ 2,nivel==7~3,
                          nivel>=8 & nivel<=10~4,nivel>=11 & nivel<=13~5,
                          TRUE~ NA_real_))

pobreza24 <- pobreza24 %>%
  mutate(
    # Analfabetismo (1 si alfabetism ≠ 1 o es NA)
    analf = if_else(alfabetism == 1, 0L, 1L, missing = 1L),
    # Nivel educativo
    apree = if_else(nivel == 1, 1L, 0L, missing = 0L),
    aprim = if_else(nivel == 2, 1L, 0L, missing = 0L),
    asecu = if_else(nivel == 3, 1L, 0L, missing = 0L),
    aprep = if_else(nivel == 4, 1L, 0L, missing = 0L),
    asupe = if_else(nivel == 5, 1L, 0L, missing = 0L),
    # Institución de salud
    is_imss = if_else(inst_sal == 1, 1L, 0L, missing = 0L),
    is_iest = if_else(inst_sal == 2, 1L, 0L, missing = 0L),
    is_priv = if_else(inst_sal == 3, 1L, 0L, missing = 0L),
    is_noss = if_else(is.na(inst_sal), 1L, 0L),  # aquí se mantiene explícito
    # Razón de seguridad social
    rss_trab = if_else(raz_s_sal == 1, 1L, 0L, missing = 0L),
    rss_jubi = if_else(raz_s_sal == 2, 1L, 0L, missing = 0L),
    rss_fami = if_else(raz_s_sal == 3, 1L, 0L, missing = 0L),
    rss_estu = if_else(raz_s_sal == 4, 1L, 0L, missing = 0L),
    rss_cpro = if_else(raz_s_sal == 5, 1L, 0L, missing = 0L)
  )

# Años de escolaridad
pobreza24 <- pobreza24 %>%
  mutate(
    aesc = case_when(
      # Sin educación o preescolar
      nivelaprob %in% c(0, 1) ~ 0,
      # Primaria
      nivelaprob == 2 & gradoaprob == 1 ~ 1,
      nivelaprob == 2 & gradoaprob == 2 ~ 2,
      nivelaprob == 2 & gradoaprob == 3 ~ 3,
      nivelaprob == 2 & gradoaprob == 4 ~ 4,
      nivelaprob == 2 & gradoaprob == 5 ~ 5,
      nivelaprob == 2 & gradoaprob == 6 ~ 6,
      # Secundaria
      nivelaprob == 3 & gradoaprob == 1 ~ 7,
      nivelaprob == 3 & gradoaprob == 2 ~ 8,
      nivelaprob == 3 & gradoaprob == 3 ~ 9,
      # Preparatoria
      nivelaprob == 4 & gradoaprob == 1 ~ 10,
      nivelaprob == 4 & gradoaprob == 2 ~ 11,
      nivelaprob == 4 & gradoaprob == 3 ~ 12,
      # Superior
      nivelaprob == 7 & gradoaprob == 1 ~ 13,
      nivelaprob == 7 & gradoaprob == 2 ~ 14,
      nivelaprob == 7 & gradoaprob == 3 ~ 15,
      nivelaprob == 7 & gradoaprob %in% c(4, 5) ~ 16,
      # Maestría
      nivelaprob == 8 & gradoaprob == 1 ~ 17,
      nivelaprob == 8 & gradoaprob >= 2 & !is.na(gradoaprob) ~ 18,
      # Doctorado
      nivelaprob == 9 & gradoaprob == 1 ~ 19,
      nivelaprob == 9 & gradoaprob == 2 ~ 20,
      nivelaprob == 9 & gradoaprob == 3 ~ 21,
      nivelaprob == 9 & gradoaprob >= 4 & !is.na(gradoaprob) ~ 22,
      # Normal o técnica: antecedente primaria
      nivelaprob %in% c(5, 6) & antec_esc == 1 & gradoaprob == 1 ~ 7,
      nivelaprob %in% c(5, 6) & antec_esc == 1 & gradoaprob == 2 ~ 8,
      nivelaprob %in% c(5, 6) & antec_esc == 1 & gradoaprob == 3 ~ 9,
      nivelaprob %in% c(5, 6) & antec_esc == 1 & gradoaprob == 4 ~ 10,
      nivelaprob %in% c(5, 6) & antec_esc == 1 & gradoaprob == 5 ~ 11,
      # Normal o técnica: antecedente secundaria
      nivelaprob %in% c(5, 6) & antec_esc == 2 & gradoaprob == 1 ~ 10,
      nivelaprob %in% c(5, 6) & antec_esc == 2 & gradoaprob == 2 ~ 11,
      nivelaprob %in% c(5, 6) & antec_esc == 2 & gradoaprob == 3 ~ 12,
      nivelaprob %in% c(5, 6) & antec_esc == 2 & gradoaprob == 4 ~ 13,
      nivelaprob %in% c(5, 6) & antec_esc == 2 & gradoaprob == 5 ~ 14,
      # Normal o técnica: antecedente preparatoria
      nivelaprob %in% c(5, 6) & antec_esc == 3 & gradoaprob == 1 ~ 13,
      nivelaprob %in% c(5, 6) & antec_esc == 3 & gradoaprob == 2 ~ 14,
      nivelaprob %in% c(5, 6) & antec_esc == 3 & gradoaprob == 3 ~ 15,
      nivelaprob %in% c(5, 6) & antec_esc == 3 & gradoaprob %in% c(4, 5) ~ 16,
      # Valor por omisión si no se cumple ninguna condición
      TRUE ~ NA_real_ ))

pobreza24 <- pobreza24 %>%
  group_by(folioviv, foliohog) %>%
  mutate(
    # Máximo de escolaridad en el hogar
    esc_max_h = max(aesc, na.rm = TRUE),
    # Escolaridad de la jefa(e) del hogar (numren == 1)
    esc_jh = aesc[numren == 1][1]  # [1] por si hay duplicados o NA
  ) %>%
  ungroup() %>%
  mutate(
    # Indicadores para esc_max_h
    d_emax_supe = as.integer(esc_max_h >= 13),
    d_emax_prep = as.integer(esc_max_h >= 10 & esc_max_h <= 12),
    d_emax_secu = as.integer(esc_max_h >= 7 & esc_max_h <= 9),
    d_emax_prim = as.integer(esc_max_h >= 1 & esc_max_h <= 6),
    d_emax_sine = as.integer(esc_max_h == 0),
    # Indicadores para esc_jh
    d_ejh_supe = as.integer(esc_jh >= 13),
    d_ejh_prep = as.integer(esc_jh >= 10 & esc_jh <= 12),
    d_ejh_secu = as.integer(esc_jh >= 7 & esc_jh <= 9),
    d_ejh_prim = as.integer(esc_jh >= 1 & esc_jh <= 6),
    d_ejh_sine = as.integer(esc_jh == 0))

pobreza24 <- pobreza24 %>%
  group_by(folioviv, foliohog) %>%
  mutate(
    # 1. Trabajo de menores de 12 a 14
    trab_men1214 = as.integer(any(edad >= 12 & edad <= 14 & t_trab >= 1, na.rm = TRUE)),
    trab2_men1214 = as.integer(any(edad >= 12 & edad <= 14 & htrab >= 1, na.rm = TRUE)),
    # 2. Trabajo de menores de 15 a 17
    trab_men1517 = as.integer(any(edad >= 15 & edad <= 17 & t_trab >= 1, na.rm = TRUE)),
    trab2_men1517 = as.integer(any(edad >= 15 & edad <= 17 & htrab >= 1, na.rm = TRUE)),
    # 3. Número de personas adultas (18+) con t_trab >= 35 horas
    n_ttiemcomp = sum(edad >= 18 & t_trab >= 35, na.rm = TRUE),
    n2_ttiemcomp = sum(edad >= 18 & htrab >= 35, na.rm = TRUE),
    # 4. Número de personas adultas (18+) con t_trab entre 1 y 34 horas
    n_ttiemparc = sum(edad >= 18 & t_trab >= 1 & t_trab < 35, na.rm = TRUE),
    n2_ttiemparc = sum(edad >= 18 & htrab >= 1 & htrab < 35, na.rm = TRUE),
    # 5. Indicador: nadie trabaja jornada completa (>=35)
    d_nttiemcomp = as.integer(n_ttiemcomp == 0),
    d2_nttiemcomp = as.integer(n2_ttiemcomp == 0),
    # 6. Horas de trabajo del jefe/a del hogar, NA se convierte en 0
    t_trab_jh = coalesce(t_trab[numren == 1][1], 0),
    t2_trab_jh = coalesce(htrab[numren == 1][1], 0),
    # 7. Indicador: jefe(a) trabaja jornada completa (0 si t_trab_jh es NA o <35)
    d_jhtiemcomp = as.integer(t_trab_jh >= 35),
    d2_jhtiemcomp = as.integer(t2_trab_jh >= 35)) %>%
  ungroup()

pobreza24 <- pobreza24 %>%
  group_by(folioviv, foliohog) %>%
  mutate(
    # Variables para t_cuid
    n_cuid = sum(t_cuid >= 1, na.rm = TRUE),
    n_cuid_1214 = sum(edad >= 12 & edad <= 14 & t_cuid >= 1, na.rm = TRUE),
    n_cuid_nasis_1214 = sum(edad >= 12 & edad <= 14 & inas_esc == 1 & t_cuid >= 1, na.rm = TRUE),
    n_cuid_1517 = sum(edad >= 15 & edad <= 17 & t_cuid >= 1, na.rm = TRUE),
    n_cuid_nasis_1517 = sum(edad >= 15 & edad <= 17 & inas_esc == 1 & t_cuid >= 1, na.rm = TRUE),
    # Variables para t_qhac
    n_qhac = sum(t_qhac >= 1, na.rm = TRUE),
    n_qhac_1214 = sum(edad >= 12 & edad <= 14 & t_qhac >= 1, na.rm = TRUE),
    n_qhac_nasis_1214 = sum(edad >= 12 & edad <= 14 & inas_esc == 1 & t_qhac >= 1, na.rm = TRUE),
    n_qhac_1517 = sum(edad >= 15 & edad <= 17 & t_qhac >= 1, na.rm = TRUE),
    n_qhac_nasis_1517 = sum(edad >= 15 & edad <= 17 & inas_esc == 1 & t_qhac >= 1, na.rm = TRUE),
    # Variables para t_acarr
    n_acarr = sum(t_acarr >= 1, na.rm = TRUE),
    d_acarr = ifelse(n_acarr==0,0,1)) %>%
  ungroup()

pobreza24 <- pobreza24 %>%
  group_by(folioviv, foliohog) %>%
  mutate(
    # Diabetes
    n_ndiabetes_65m   = sum(edad >= 65 & diabetes == 0, na.rm = TRUE),
    n_ndiabetes_5064  = sum(edad >= 50 & edad <= 64 & diabetes == 0, na.rm = TRUE),
    n_diabetes_65m    = sum(edad >= 65 & diabetes == 1, na.rm = TRUE),
    n_diabetes_5064   = sum(edad >= 50 & edad <= 64 & diabetes == 1, na.rm = TRUE),
    # Presión alta
    n_npresion_65m    = sum(edad >= 65 & pres_alta == 0, na.rm = TRUE),
    n_npresion_5064   = sum(edad >= 50 & edad <= 64 & pres_alta == 0, na.rm = TRUE),
    n_presion_65m     = sum(edad >= 65 & pres_alta == 1, na.rm = TRUE),
    n_presion_5064    = sum(edad >= 50 & edad <= 64 & pres_alta == 1, na.rm = TRUE),
    # Peso
    n_npeso_65m       = sum(edad >= 65 & peso == 0, na.rm = TRUE),
    n_npeso_5064      = sum(edad >= 50 & edad <= 64 & peso == 0, na.rm = TRUE),
    n_npeso_3049      = sum(edad >= 30 & edad <= 49 & peso == 0, na.rm = TRUE),
    n_npeso_1229      = sum(edad >= 12 & edad <= 29 & peso == 0, na.rm = TRUE),
    n_peso_65m        = sum(edad >= 65 & peso == 1, na.rm = TRUE),
    n_peso_5064       = sum(edad >= 50 & edad <= 64 & peso == 1, na.rm = TRUE),
    n_peso_3049       = sum(edad >= 30 & edad <= 49 & peso == 1, na.rm = TRUE),
    n_peso_1229       = sum(edad >= 12 & edad <= 29 & peso == 1, na.rm = TRUE)) %>%
  ungroup()

pobreza24 <- pobreza24 %>%
  group_by(folioviv, foliohog) %>%
  mutate(
    # Suma de trabajo_mp (personas con trabajo_mp == 1)
    n_trab = sum(trabajo_mp == 1, na.rm = TRUE),
    # Personas con trabajo_mp == 0 o NA (sin trabajo o sin información)
    n_ntrab = sum(trabajo_mp == 0 | is.na(trabajo_mp)),
    # Personas de 18 a 64 años sin trabajo (trabajo_mp == 0 o NA)
    n_ntrab_1864 = sum(edad >= 18 & edad <= 64 & (trabajo_mp == 0 | is.na(trabajo_mp))),
    # Personas de 12 a 17 años con trabajo_mp == 1
    n_trab_1217 = sum(edad >= 12 & edad <= 17 & trabajo_mp == 1, na.rm = TRUE)) %>%
  ungroup()

pobreza24 <- pobreza24 %>%
  mutate(
    traext = if_else(trapais == 0, 1L, 0L),
    pago_asal = if_else(pago == 1, 1L, 0L),
    pago_sphog = if_else(pago == 2, 1L, 0L),
    pago_spnhog = if_else(pago == 3, 1L, 0L)
  ) %>%
  fastDummies::dummy_cols(select_columns = c("sinco", "scian", "clas_emp"),
                          remove_selected_columns = FALSE,
                          remove_first_dummy = FALSE,
                          ignore_na = TRUE)

vars_na0 <- c("gas_tran_e", "lgastrans", "tot_iaad", "tot_iamen", "inas_esc", "hli",
              "alfabetism", "aesc", "trabajo_mp")

pobreza24 <- pobreza24 %>%
  mutate(across(all_of(vars_na0), ~ replace_na(as.numeric(as.character(.)), 0)))

pobreza24 <- pobreza24 %>%
  mutate(tam_emp = case_when(tam_emp==1~1,tam_emp %in% c(2,3)~2,
                             tam_emp %in% c(4,5,6,7)~3,tam_emp %in% c(8, 9)~4,
                             tam_emp %in% c(10, 11)~5,TRUE~NA_real_),
         te_1 = as.integer(tam_emp == 1),
         te_2 = as.integer(tam_emp == 2),
         te_3 = as.integer(tam_emp == 3),
         te_4 = as.integer(tam_emp == 4),
         te_5 = as.integer(tam_emp == 5))

agregar_indicadores_hogar <- function(df, vars, id_vars = c("folioviv", "foliohog")) {
  df %>%
    group_by(across(all_of(id_vars))) %>%
    mutate(across(all_of(vars), 
                  list(n = ~ sum(. == 1, na.rm = TRUE),
                       d = ~ as.integer(any(. == 1, na.rm = TRUE))),
                  .names = "{fn}_{.col}")) %>%
    ungroup()
}

pobreza24 <- agregar_indicadores_hogar(pobreza24, vars = c("pea","jub","pam","hli","analf",
                                                           "apree","aprim","asecu","aprep","asupe",
                                                           "tiene_b","segsoc","trab_comun",
                                                           "atemed","is_imss","is_iest","is_priv","is_noss",
                                                           "rss_trab","rss_jubi","rss_fami","rss_estu","rss_cpro",
                                                           "d_disca","d_hogar","d_jubil","d_estud",
                                                           "traext","subor","indep","personal",
                                                           "pago_asal","pago_sphog","pago_spnhog",
                                                           "contrato","tipocontr","pres_ninguna",
                                                           "sinco_1","scian_1","sinco_4","scian_4","sinco_7","scian_7",
                                                           "sinco_2","scian_2","sinco_5","scian_5","sinco_8","scian_8",
                                                           "sinco_3","scian_3","sinco_6","scian_6","sinco_9","scian_9",
                                                           "clas_emp_1","clas_emp_2","clas_emp_3","clas_emp_4",
                                                           "te_1","te_2","te_3","te_4","te_5",
                                                           "pago_asal","pago_sphog","pago_spnhog"))

excepciones <- c("tipo_viv", "antiguedad","disp_agua","atemed","d_ejh_sine","d_ejh_prim","d_ejh_secu","d_ejh_prep","d_ejh_supe",
                 "sinco_1","scian_1","sinco_4","scian_4","sinco_7","scian_7",
                 "sinco_2","scian_2","sinco_5","scian_5","sinco_8","scian_8",
                 "sinco_3","scian_3","sinco_6","scian_6","sinco_9","scian_9")
na_counts <- sapply(pobreza24, function(x) sum(is.na(x)))
variables_a_conservar <- names(na_counts)[na_counts <= 15 | names(na_counts) %in% excepciones]
pobreza24 <- pobreza24 %>% select(all_of(variables_a_conservar))

# Se guardan como enteros los factores para ahorrar memoria

pobreza24 <- pobreza24 %>%
  mutate(across(where(is.factor), ~ as.integer(.)))

# Últimos ajustes:

pobreza24$ins_ali <- as.factor(pobreza24$ins_ali) 
pobreza24$est_socio <- as.factor(pobreza24$est_socio) 
pobreza24$parentesco <- as.factor(pobreza24$parentesco) 
pobreza24$sexo <- as.factor(pobreza24$sexo) 
pobreza24$bano_comp <- if_else(pobreza24$bano_comp==0,0L,1L)
pobreza24$bano_excus <- if_else(pobreza24$bano_excus==0,0L,1L)
pobreza24$bano_regad <- if_else(pobreza24$bano_regad==0,0L,1L)

# Se guardan las bases en formato csv gz

write.csv(pobreza24, gzfile("pobreza24.csv.gz"), row.names = FALSE)