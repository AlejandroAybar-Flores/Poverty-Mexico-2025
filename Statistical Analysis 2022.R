library(readxl)

df22 <- read_excel("C:/Users/a.aybarf/Downloads/BBDDPobrezaIII/dfregresion2022.xlsx")

df22$plp <- as.factor(df22$plp)
df22$rururb <- as.factor(df22$rururb)
df22$sexo <- as.factor(df22$sexo)
df22$alfabetism <- as.factor(df22$alfabetism)
df22$trabajo_mp <- as.factor(df22$trabajo_mp)
df22$ins_ali <- as.factor(df22$ins_ali)
df22$hli <- as.factor(df22$hli)
df22$segpop <- as.factor(df22$segpop)
df22$atemed <- as.factor(df22$atemed)
df22$tarjeta <- as.factor(df22$tarjeta)
df22$disp_elect <- as.factor(df22$disp_elect)
df22$región <- as.factor(df22$región)
df22$conectividad <- as.factor(df22$conectividad)
df22$agua_drenaje <- as.factor(df22$agua_drenaje)
df22$neducativojefe <- as.factor(df22$neducativojefe)
df22$ocupacion_hogar <- as.factor(df22$ocupacion_hogar)
df22$trabajomenores <- as.factor(df22$trabajomenores)
df22$housing_tenure <- as.factor(df22$housing_tenure)
df22$basic_energy <- as.factor(df22$basic_energy)

library(survey)

#Establecimiento del diseño muestral dentro de las BD

diseño22 <- svydesign(id =~ upm, strata =~ est_dis, weights=~ factor, data=df22) 
options(survey.lonely.psu="certainty")

library(jtools)
library(car)
library(margins)

##########
## 2022 ##
##########

# Realizar la prueba t de medias para las variables numéricas
svyttest(edad ~ plp, diseño22)
svyttest(tot_resid ~ plp, diseño22)
svyttest(niños ~ plp, diseño22)
svyttest(log_gasto_total_e ~ plp, diseño22)

# Realizar las pruebas de chi-cuadrado por cada variable categórica
svychisq(~rururb + plp, diseño22, statistic = c("Chisq"))
svychisq(~sexo + plp, diseño22, statistic = c("Chisq"))
svychisq(~alfabetism + plp, diseño22, statistic = c("Chisq"))
svychisq(~trabajo_mp + plp, diseño22, statistic = c("Chisq"))
svychisq(~ins_ali + plp, diseño22, statistic = c("Chisq"))
svychisq(~hli + plp, diseño22, statistic = c("Chisq"))
svychisq(~segpop + plp, diseño22, statistic = c("Chisq"))
svychisq(~atemed + plp, diseño22, statistic = c("Chisq"))
svychisq(~tarjeta + plp, diseño22, statistic = c("Chisq"))
svychisq(~disp_elect + plp, diseño22, statistic = c("Chisq"))
svychisq(~región + plp, diseño22, statistic = c("Chisq"))
svychisq(~conectividad + plp, diseño22, statistic = c("Chisq"))
svychisq(~agua_drenaje + plp, diseño22, statistic = c("Chisq"))
svychisq(~neducativojefe + plp, diseño22, statistic = c("Chisq"))
svychisq(~ocupacion_hogar + plp, diseño22, statistic = c("Chisq"))
svychisq(~trabajomenores + plp, diseño22, statistic = c("Chisq"))
svychisq(~housing_tenure + plp, diseño22, statistic = c("Chisq"))
svychisq(~basic_energy + plp, diseño22, statistic = c("Chisq"))

#################
## REGRESIONES ##
#################

##########
## 2022 ##
##########

#Binomial Logit
Modelo22 <- svyglm(formula = plp ~ edad + tot_resid + niños + log_gasto_total_e
                   + rururb + sexo + alfabetism
                   + ins_ali + hli + segpop + atemed + tarjeta 
                   + disp_elect + región + conectividad + agua_drenaje
                   + neducativojefe + trabajomenores + ocupacion_hogar
                   + housing_tenure + basic_energy, design = diseño22, family = quasibinomial(link="logit"))
summary(Modelo22)
summ(Modelo22,confint = getOption("summ-confint", TRUE),model.info = getOption("summ-model.info", TRUE),model.fit = getOption("summ-model.fit", TRUE),exp=TRUE,vifs = getOption("summ-vifs", TRUE))

# Calcular los efectos marginales
efectos_marginales <- margins(Modelo22, design = diseño22)
# Mostrar los resultados de los efectos marginales
summary(efectos_marginales)
#VIF
vif(Modelo22)


