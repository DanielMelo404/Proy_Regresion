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

fit <- lm(lbm~.,data=housing)
summary(fit)
confint(fit, "ht",level = 0.99)
vcov(fit)
Mu = fit$fitted.values
plot(Mu)

resid <- fit$residuals
mean(resid)

summary(fit)
#la hipotesis nula del i es si b1=0, b2=0 o b3=0 
# sie el modelo se puede expresar como uno mas pequeno se puede

#es mas grabe quitar algo no significativo que si podria ser, a dejarlo
#los p valors no son los unicos determinantes para dejar al modelo
#sele debe preguntar al modelo con la hipotesis lineal generla, no independientemente
# se deja una variable con base al experto, talvez se quiere dejar por motivos explicativos

