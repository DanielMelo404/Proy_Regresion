library(ppcor)
library(readxl)

###################### a) Haga un análisis descriptivo del conjunto de datos ###################
########### Lectura de los datos ########
setwd("~/UNAL 2022_II/Docencia/Regresión/Material módulos/Módulo 3")
Atletas <- read_excel("Atletas.xlsx")

cor(Atletas) #este análisis es necesario mas no suficiente para descartar multicolinealidad.

cor(Atletas,method="kendall")

plot(Atletas)

pcor(Atletas) #solo miren la relación de cada variable explicativa con la variable dependiente

help(pcor)

###################### c) Estime e INTERPRETE los parámetros  ###################

fit<-lm(lbm~1+ht+wt+rcc, data=Atletas)
summary(fit)
confint(fit)

###################### d) Compruebe, haciendo uso de la forma matricial del modelo, 
#####que efectivamente las estimaciones de los parámetros corresponden con las proporcionadas
###por R  ###################
n<-dim(Atletas)[1]

Y<-as.matrix(Atletas[,1])
X<-as.matrix(Atletas[,2:4])
X<-cbind(rep(1,n),X)
colnames(X)[1]<-"interc"

betahat<-solve(t(X)%*%X)%*%t(X)%*%Y

all.equal(as.numeric(betahat),as.numeric(fit$coefficients))

###################### e) Calcule la matriz de varianzas y covarianzas estimada 
#de los parámetros de localización y compare con la que proporciona el modelo.

#Matriz de varianzas y covarianzas
vcov(fit) #Resultado de R

varhat<-(1/(n-4))*sum((Y-X%*%betahat)^2)

sdhat<-sqrt(varhat)
sdhat
summary(fit)

vcovbetahat<-varhat*solve(t(X)%*%X)

all.equal(as.numeric(vcovbetahat),as.numeric(vcov(fit)))

###################### f) Calcule la matriz de proyección, H, y verifique que es 
#simétrica e idempotente.

H<-X%*%solve(t(X)%*%X)%*%t(X)

all.equal(t(H),H) #verifica simetría

all.equal(H%*%H,H) #verifica idempotencia

###################### g) Calcule las estimativas del valor medio estimado para 
#todos los individuos de la muestra.

Yhat<-X%*%betahat

Mu<-H%*%Y #predicciones como una proyección

all.equal(Yhat,Mu)

all.equal(as.numeric(Mu),as.numeric(fit$fitted.values))

plot(Mu,Y)
lines(Mu,Mu,col="red")

###Cuál es el valor predicho para el individuo 1

Y1hat<-X[1,]%*%betahat

Y[1] #verdadero valor

Yhats<-X[c(1,4,6,45),]%*%betahat

Y[c(1,4,6,45)] #verdadero valor

###################### h) Calcule el vector de residuales del modelo y verifique 
#que al proyectarlos con la matriz H se obtiene el vector 0. ¿Qué significa ese resultado? 
#¿Cuál es la media de los residuales? 

resid<-Y-Mu

resid2<-fit$residuals

all.equal(as.numeric(resid),as.numeric(resid2))

all.equal(as.numeric(H%*%resid),rep(0,n))

mean(resid)

###################### i) ¿Es el modelo de regresión significativo para explicar
#la variabilidad presentada en lbm?

summary(fit)

#Usando la hipótesis lineal general

C<-matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),ncol=4,nrow=3,byrow=T)  #H0: CB=0   vs H1: CB<>0

f_C<-t(C%*%betahat)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%betahat)/(3*varhat)

pval_i<-1-pf(f_C,3,n-4)

#usando anova

fit0<-lm(lbm~1, data=Atletas)

f_C2<-(sum(fit0$residuals^2)-sum(fit$residuals^2))/(3*varhat)

anova(fit0,fit,test="F")

###################### m) ¿Son necesarias las variables de estatura y de conteo 
#de glóbulos rojos para explicar el comportamiento de la masa corporal magra?

C3<-matrix(c(0,1,0,0,0,0,0,1),ncol=4,nrow=2,byrow=T)  #H0: CB=0   vs H1: CB<>0

f_C3<-t(C3%*%as.matrix(fit$coeff))%*%solve(C3%*%as.matrix(vcov(fit))%*%t(C3))%*%(C3%*%as.matrix(fit$coeff))/2

pval_m<-1-pf(f_C3,2,n-4)

fit1<-lm(lbm~1+wt, data=Atletas)
anova(fit1,fit,test="F")

###################### n) ¿Se podría decir que el efecto del aumento de 1kg en la
#masa del atleta es igual al efecto del aumento de 1cm en la altura?

a<-c(0,1,-1,0)

t_C<-t(a)%*%as.matrix(fit$coeff)/sqrt((t(a)%*%as.matrix(vcov(fit))%*%a))

pval_n<-2*(1-pt(abs(t_C),n-4))

###################### o) Calcule un intervalo del 95% de confianza para el valor 
#medio de TODOS los individuos con una estatura de 170cm, 70kg y con un conteo de 4.5 glóbulos rojos.

x<-c(1,170,70,4.5)

mu<-t(x)%*%as.matrix(fit$coeff)

ee<-sqrt((t(x)%*%as.matrix(vcov(fit))%*%x))

perct<-qt(0.975,df=n-4)

LI<-mu-perct*ee

LS<-mu+perct*ee

#con la función predict

x2<-as.data.frame(rbind(x[-1])) #los nuevos datos deben estar como un data.frame
colnames(x2)<-colnames(Atletas[,-1]) #además, deben tener los nombres de las variables originales
predict(fit,x2,se.fit=TRUE,interval="confidence", level=0.95) #se puede cambiar por prediction para un int. de predicción

####construir varios intervalos de confianza para varios valores medios

x3<-rbind(x2,c(180,90,5),c(160,65,2)); x3
predict(fit,x3,se.fit=TRUE,interval="confidence", level=0.95) #se puede cambiar por prediction para un int. de predicción

####Construir varios intervalos de predicción para el valor de individuos que no estaban en la muestra
predict(fit,x3,se.fit=TRUE,interval="prediction", level=0.95) #se puede cambiar por prediction para un int. de predicción

###################### p) Calcule e interprete el coeficiente de determinación y
#el coeficiente de determinación ajustado.

summary(fit)

###modelo inventado

cod<-1:98
cod2<-cod^2
fit_im<-lm(lbm~1+ht+wt+rcc+cod+cod2, data=Atletas)
summary(fit_im)

###################### q) Modelo con potencias.

fitnl<-lm(lbm~1+ht+I(ht^2)+wt+rcc, data=Atletas)
summary(fitnl)

fitnl2<-lm(lbm~1+poly(ht,2,raw=TRUE)+wt+rcc, data=Atletas)
summary(fitnl2)
eigen(vcov(fitnl2))
eigen(vcov(fit))
cor(model.matrix(fitnl))

#########################PROBLEMA 2##########################################
#############################################################################
#############################################################################

#######Lectura de datos
ipb_data <- read_excel("Varios_2022_I/U_Narino/Docencia/Semana 2/4. RLM_1/Practica 2/ipb.xlsx")
head(ipb_data)
str(ipb_data)

###################### a) Calcule y comente las estadísticas de resumen más importantes
#(min, Q1, mediana, media, Q3, max, desviación estándar) para el Índice de Placa Bacteriana en cada combinación de grupo y sexo.

library(spam)
library(fields)

stats(ipb_data$ipb, by=ipb_data$grupo)
stats(ipb_data$ipb, by=ipb_data$sexo)

####Resumen gráfico

#Boxplots
boxplot(ipb ~ grupo*sexo, data = ipb_data,
        xlab = "Grupo y sexo", ylab = "IPB",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))

#Diagrama de dispersión

ipb<-ipb_data$ipb
sexo<-as.factor(ipb_data$sexo)
grupo<-as.factor(ipb_data$grupo)
n2<-length(ipb)

mini <- min(ipb)
maxi <- max(ipb)
par(xaxt="n")
plot(as.integer((grupo)[sexo=="F"]), ipb[sexo=="F"], xlab=" ", ylim=c(mini,maxi), xlim=c(0.5,length(levels(grupo))+0.5), ylab=" ", cex=0.2, lwd=3, col="blue")
par(new=T)
plot(as.integer((grupo)[sexo=="M"]), ipb[sexo=="M"], xlab=" ", ylim=c(mini,maxi), xlim=c(0.5,length(levels(grupo))+0.5), ylab="Índice de Placa Bacteriana ", cex=0.2, lwd=3, col="red")
lab <- c("Grupo 1","Grupo 2","Grupo 3")
par(xaxt="s")
axis(side=1, at=seq(1,length(levels(grupo)),by=1), label=lab)
legend(0.5,5,c("Femenino","Masculino"), bty="n", col=c("blue","red"), lty=c(1,1))

###################### c) Estime e INTERPRETE los parámetros (localización y escala) del modelo propuesto.

fit <- lm(ipb ~ 1+grupo+sexo)
summary(fit)

grupo2<-relevel(factor(grupo),"Grupo2") #recorderis de cómo cambiar la categoria de referencia


library(fastDummies)
ipb_data2<-dummy_cols(ipb_data,c("grupo","sexo"),remove_first_dummy = TRUE,remove_selected_columns = TRUE)

head(ipb_data2)

fit1a <- lm(ipb ~ ., data=ipb_data2)
summary(fit1a)

#solo como ejemplo
fit1b <- lm(ipb ~ .-grupo_Grupo2, data=ipb_data2)
summary(fit1b)

###################### d) ¿Cree usted que la muestra proporciona evidencia 
#estadísticamente significativa de que el Índice de Placa Bacteriana de los 
#estudiantes depende del grupo poblacional? Justifique su respuesta.

fit2 <- lm(ipb ~ 1+sexo) #modelo contenido en el original

anova(fit2, fit, test="F")

###################### e) ¿Cree usted que la muestra proporciona evidencia 
#estadísticamente significativa de que el Índice de Placa Bacteriana de los 
#estudiantes depende del sexo? Justifique su respuesta.

fit3 <- lm(ipb ~ 1+grupo)

anova(fit3, fit, test="F")

###################### f) ¿Cree usted que la muestra proporciona evidencia estadísticamente
#significativa de que el efecto del grupo poblacional sobre el Índice de Placa Bacteriana 
#de los estudiantes depende del sexo? Justifique su respuesta.

fit4 <- lm(ipb ~ 1+grupo+sexo +grupo*sexo)
summary(fit4)

anova(fit, fit4, test="F")

###################### g) ¿Cree usted que la muestra proporciona evidencia estadísticamente 
#significativa de que el efecto del grupo poblacional sobre el Índice de Placa Bacteriana 
#es igual para los grupos 2 y 3? Justifique su respuesta.

a<-c(0,2,-1,0)

t_C<-t(a)%*%as.matrix(fit1a$coeff)/sqrt((t(a)%*%as.matrix(vcov(fit1a))%*%a))

pval_g<-2*(1-pt(t_C,n2-4))
