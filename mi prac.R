library(ppcor)
library(readxl)

# a)

# se puede arreglar una relacion monotona con una transformacion
# prueba de mulitcolineialidad con variables con alto corr


head(Housing)
head(Atletas)

housing <- subset(Housing, select = -c(prefarea, airconditioning,hotwaterheating, basement,guestroom,mainroad, furnishingstatus) )

cor(housing)

dim(Housing)

class(Atletas)

class(housing)
dim(Atletas)
pcor(housing)

# TODO 
library(leaps)
regfit.full <- regsubsets(price ~ ., Housing) #Todos los criterios de informaci?n coinciden en la selecci?n para 
#un mismo tama?o (AIC, BIC, R^2 ajustado)
summary(regfit.full)
reg.summary <- summary(regfit.full)
###
names(reg.summary)
###
reg.summary$rsq
###
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "N?mero de variables",
     ylab = "SCE", type = "l")
plot(reg.summary$adjr2, xlab = "N?mero de variables",
     ylab = " R2 Ajustado", type = "l")
###
which.max(reg.summary$adjr2)

points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
       pch = 20)
###
which.max(regfit.summary$adjr2)


fit_todo <- lm(price~., data = Housing)
fit <- lm(price~.,data=housing)
summary(fit)
summary(fit_todo)

confint(fit, "ht",level = 0.99)
vcov(fit)
Mu = fit$fitted.values
plot(Mu)

resid <- fit_mejor$residuals

mean(resid)

res <- fit_todo$residuals
plot(resid)
# MEJOR MODELO

housing_mejor <- subset(Housing, select = c(price,bathrooms) )
area_ <-log(housing$area)
plot(housing$area)
plot(housing$area,housing$price)
housing_mejor <-subset(Housing, select = c(price,area, bathrooms, stories, basement, airconditioning, parking, prefarea, furnishingstatus) ) 
#subset(Housing, select = c(price,area, bathrooms) )
#subset(Housing, select = c(price,area) )



# subset(Housing, select = c(price,area, bathrooms, stories, airconditioning) )
# modelo 8 subset(Housing, select = c(price,area, bathrooms, stories, basement, airconditioning, parking, prefarea, furnishingstatus) )

fit_mejor <- lm(price ~ . ,housing_mejor)
#abline(fit_mejor)

summary(fit_mejor)
res <- fit_mejor$residuals
hist(res)
mean(res)
plot(housing$area,res)
summary(fit)

#la hipotesis nula del i es si b1=0, b2=0 o b3=0 
# sie el modelo se puede expresar como uno mas pequeno se puede

#es mas grabe quitar algo no significativo que si podria ser, a dejarlo
#los p valors no son los unicos determinantes para dejar al modelo
#sele debe preguntar al modelo con la hipotesis lineal generla, no independientemente
# se deja una variable con base al experto, talvez se quiere dejar por motivos explicativos

