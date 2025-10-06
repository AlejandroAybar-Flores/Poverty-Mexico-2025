library(readxl)

df24 <- read_excel("C:/Users/a.aybarf/Downloads/dfregresion24.xlsx")

df24$plp <- as.factor(df24$plp)
df24$rururb <- as.factor(df24$rururb)
df24$sexo <- as.factor(df24$sexo)
df24$alfabetism <- as.factor(df24$alfabetism)
df24$trabajo_mp <- as.factor(df24$trabajo_mp)
df24$ins_ali <- as.factor(df24$ins_ali)
df24$hli <- as.factor(df24$hli)
df24$segsoc <- as.factor(df24$segsoc)
df24$atemed <- as.factor(df24$atemed)
df24$tarjeta <- as.factor(df24$tarjeta)
df24$disp_elect <- as.factor(df24$disp_elect)
df24$región <- as.factor(df24$región)
df24$conectividad <- as.factor(df24$conectividad)
df24$agua_drenaje <- as.factor(df24$agua_drenaje)
df24$neducativojefe <- as.factor(df24$neducativojefe)
df24$ocupacion_hogar <- as.factor(df24$ocupacion_hogar)
df24$trabajomenores <- as.factor(df24$trabajomenores)
df24$housing_tenure <- as.factor(df24$housing_tenure)
df24$basic_energy <- as.factor(df24$basic_energy)

library(survey)

#Establecimiento del diseño muestral dentro de las BD

diseño24 <- svydesign(id =~ upm, strata =~ est_dis, weights=~ factor, data=df24) 
options(survey.lonely.psu="certainty")

library(jtools)
library(car)
library(margins)

##########
## 2024 ##
##########

# Realizar la prueba t de medias para las variables numéricas
svyttest(edad ~ plp, diseño24)
svyttest(tot_resid ~ plp, diseño24)
svyttest(niños ~ plp, diseño24)
svyttest(consumption_expenditure ~ plp, diseño24)

# Realizar las pruebas de chi-cuadrado por cada variable categórica
svychisq(~rururb + plp, diseño24, statistic = c("Chisq"))
svychisq(~sexo + plp, diseño24, statistic = c("Chisq"))
svychisq(~alfabetism + plp, diseño24, statistic = c("Chisq"))
svychisq(~trabajo_mp + plp, diseño24, statistic = c("Chisq"))
svychisq(~ins_ali + plp, diseño24, statistic = c("Chisq"))
svychisq(~hli + plp, diseño24, statistic = c("Chisq"))
svychisq(~segsoc + plp, diseño24, statistic = c("Chisq"))
svychisq(~atemed + plp, diseño24, statistic = c("Chisq"))
svychisq(~tarjeta + plp, diseño24, statistic = c("Chisq"))
svychisq(~disp_elect + plp, diseño24, statistic = c("Chisq"))
svychisq(~región + plp, diseño24, statistic = c("Chisq"))
svychisq(~conectividad + plp, diseño24, statistic = c("Chisq"))
svychisq(~agua_drenaje + plp, diseño24, statistic = c("Chisq"))
svychisq(~neducativojefe + plp, diseño24, statistic = c("Chisq"))
svychisq(~ocupacion_hogar + plp, diseño24, statistic = c("Chisq"))
svychisq(~trabajomenores + plp, diseño24, statistic = c("Chisq"))
svychisq(~housing_tenure + plp, diseño24, statistic = c("Chisq"))
svychisq(~basic_energy + plp, diseño24, statistic = c("Chisq"))

#################
## REGRESIONES ##
#################

##########
## 2024 ##
##########

#Binomial Logit
Modelo24 <- svyglm(formula = plp ~ edad + tot_resid + niños + consumption_expenditure
                   + rururb + sexo + alfabetism
                   + ins_ali + hli + segsoc + atemed + tarjeta 
                   + disp_elect + región + conectividad + agua_drenaje
                   + neducativojefe + trabajomenores + ocupacion_hogar
                   + housing_tenure + basic_energy, design = diseño24, family = quasibinomial(link="logit"))
summary(Modelo24)
summ(Modelo24,confint = getOption("summ-confint", TRUE),model.info = getOption("summ-model.info", TRUE),model.fit = getOption("summ-model.fit", TRUE),exp=TRUE,vifs = getOption("summ-vifs", TRUE))

# Calcular los efectos marginales
efectos_marginales <- margins(Modelo24, design = diseño24)
# Mostrar los resultados de los efectos marginales
summary(efectos_marginales)
#VIF
vif(Modelo24)


