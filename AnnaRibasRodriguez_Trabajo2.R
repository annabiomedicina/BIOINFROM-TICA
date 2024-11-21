#PREGUNTA 1
getwd()
datos <- read.table(file = "datos-trabajoR.txt", header = TRUE)
head(datos)
summary (datos)
dim (datos)
#hay 4 variables y 55 tratamientos
str (datos)
#PREGUNTA 2
boxplot(datos$Wildtype,col=c('blue'),ylab='Valores',main='Boxplot para Wildtype')
boxplot(datos$Sequia,col=c('red'),ylab='Valores',main='Boxplot para Sequia')
boxplot(datos$ExcesoRiego,col=c('yellow'),ylab='Valores',main='Boxplot para Exceso de Riego')

#PREGUNTA 3 y PREGUNTA 4
plot (datos$Wildtype,datos$Sequia,col=datos$Tratamiento,xlab='Wildtype',ylab='Sequia',main='Sequía vs Wildtype')
legend(x='bottomright',legend=c('Tto1','Tto2','Tto3','Tto4','Tto5','Tto6'), fill=c('black','red','green','lightblue','darkblue','pink'), title='Tratamientos')
plot (datos$Wildtype,datos$ExcesoRiego,col=datos$Tratamiento, xlab='Wildtype', ylab='ExcesoRiego',main='Exceso de Riego vs Wildtype')
legend(x='bottomright',legend=c('Tto1','Tto2','Tto3','Tto4','Tto5','Tto6'), fill=c('black','red','green','lightblue','darkblue','pink'), title='Tratamientos')
# PREGUNTA 5
hist(datos$Wildtype, main = 'Histograma de Wildtype', xlab = 'Wildtype', col = 'blue')
hist(datos$Sequia, main = 'Histograma de Sequia', xlab = 'Sequia', col = 'red')
hist(datos$ExcesoRiego, main = 'Histograma de ExcesoRiego', xlab = 'ExcesoRiego', col = 'yellow')
# PREGUNTA 6
tratamiento_factor <- factor(datos$Tratamiento)
# PREGUNTA 7
#Calculo de la media y la desviación estándar para Wildtype
media_wildtype <- tapply(datos$Wildtype, tratamiento_factor, mean)
sd_wildtype <- tapply(datos$Wildtype, tratamiento_factor, sd)

# Calculo de la media y la desviación estándar para Sequia
media_sequia <- tapply(datos$Sequia, tratamiento_factor, mean)
sd_sequia <- tapply(datos$Sequia, tratamiento_factor, sd)

# Calculo la media y la desviación estándar para ExcesoRiego
media_excesoriego <- tapply(datos$ExcesoRiego, tratamiento_factor, mean)
sd_excesoriego <- tapply(datos$ExcesoRiego, tratamiento_factor, sd)

print(media_wildtype)
print(sd_wildtype)
print(media_sequia)
print(sd_sequia)
print(media_excesoriego)
print(sd_excesoriego)
# PREGUNTA 8
conteo_tratamientos <- table(tratamiento_factor)
print(conteo_tratamientos)
#PREGUNTA 9
tratamiento1<-subset (datos,Tratamiento==1)
tratamiento4<-subset (datos,Tratamiento==4)
head(tratamiento1)
head (tratamiento4)
#PREGUNTA 10
#Primero comprobamos si siguen una distribución normal con el shapiro test. Para comprobar si sigue una distribución normal o no, miraremos en los resultados el p-value. Si p-value>0,05 siguen una normalidad, si p-value<0,05 no siguen una normalidad
shapiro.test(datos$	Wildtype [datos$Tratamiento==1]) #Sigue una distribución normal
shapiro.test(datos$Sequia[datos$Tratamiento==1]) #Sigue una distribución normal
shapiro.test(datos$ExcesoRiego[datos$Tratamiento==1]) #Sigue una distribución normal
shapiro.test(datos$Sequia[datos$Tratamiento==5]) #No sigue una dsitribuión normal
shapiro.test(datos$Wildtype[datos$Tratamiento==5]) #No sigue una distribución normal
shapiro.test(datos$ExcesoRiego[datos$Tratamiento==5]) #Sigue una distribución normal
#Una vez hemos comprobado si siguen una normalidad o no utilizamos los tests para comparar las condiciónes.Para los que no siguen una distribución normal, utilizamos un test no paramétrico como es el wilcox test, que compara las medianas de máximo 2 condiciones. Antes de hacerlo primero vamos a ver si las varianzas de cada condición son diferentes o iguales para poder indicarlo en el test. Si en el test de varianza el p-value<0,05 se rechaza la hipótesis nula de que no hay diferencia entre las dos varianzas. 
var.test(datos$Wildtype,datos$Sequia[datos$Tratamiento==5]) #Las varianzas son diferentes p<0,05
wilcox.test(datos$Wildtype,datos$Sequia[datos$Tratamiento==5],var.equal=FALSE) #indicamos que son diferentes
var.test(datos$Wildtype,datos$ExcesoRiego[datos$Tratamiento==5]) #las varianzas son diferentes p<0,05
wilcox.test(datos$Wildtype,datos$ExcesoRiego[datos$Tratamiento==5],var.equal=FALSE)
#Para los que siguen una distribución normal, utilizamos un test paramétrico como un t-test para la comparación de las medias de máximo 2 condicionees, pero primero tenemos que ver si las varianzas de cada condición son diferentes o iguales para poder indicarlo en el test.
var.test(datos$Wildtype,datos$Sequia[datos$Tratamiento==1]) #sus varianzas no son iguales
t.test(datos$Wildtype,datos$Sequia [datos$Tratamiento==1], var.equal=FALSE) #indicamos que son diferentes
var.test(datos$Wildtype,datos$ExcesoRiego[datos$Tratamiento==1]) #sus varianzas no son iguales
t.test(datos$Wildtype,datos$ExcesoRiego [datos$Tratamiento==1], var.equal=FALSE)
var.test(datos$Sequia,datos$ExcesoRiego[datos$Tratamiento==1]) #sus varianzas no son iguales
t.test(datos$Sequia,datos$ExcesoRiego [datos$Tratamiento==1], var.equal=FALSE)
#No se puede comparar sequía con exceso de riego en el tratamiento 5 porque sequia sigue una distribución normal y exceso de riego no
#Podemos ver que hay diferencias significativas en todas nuestras condiciones del tratamiento 5 con el p-value. En todos nuestros casos el p-value<0.05 por lo que podemos rechazar la hipótesis nula que en nuestro caso sería afirmar que no hay diferencia significativa entre las condiciones, por lo tanto si que existen diferencias signficativas en todas nuestras condiciones en el tratamiento 5
#En el caso del tratamiento 1, existen diferencias significativas entre Wildtype y Sequia y entre Wildtype y Exceso de Riego ya que el p<0,05 y rechazamos la hipotesis nula que afrima que no hay diferencias. En el caso de sequia con exceso de riego el p>0,05 por lo que aceptaríamos la hipótesis nula, sin diferencia entre las dos condiciones
#PREGUNTA 12
#creamos una variable para cada condición del tratamiento 1 y la guardamos
wildtype_tto1=subset(datos,Tratamiento==1)$Wildtype
head(wildtype_tto1)
sequia_tto1=subset(datos,Tratamiento==1)$Sequia
head(sequia_tto1)
excesoriego_tto1=subset(datos,Tratamiento==1)$ExcesoRiego
head(excesoriego_tto1)
#creamos una tabla normal
tabla<-data.frame(Wildtype=wildtype_tto1,Sequia=sequia_tto1,ExcesoRiego=excesoriego_tto1)
head(tabla)
#creamos una tabla larga para poder hacer un anova como en el archivo datos-anova
tabla_anova<-stack(tabla)
#ponemos la condición de que las columnas se llamen valores y condición
colnames(tabla_anova)<-c('Valores','Condición')
head(tabla_anova)
test_anova<-aov(Valores~Condición,data=tabla_anova)
summary(test_anova)
#El test anova es un método paramétrico que compara la media de 3 o mas condiciones de distribución normal. En nuestros resultados existen diferencias entre las 3 condiciones porque podemos ver que el p-value<0,05, por lo que podemos rechazar la hipótesis nula que sería afirmar que no hay diferencia significativa entre las 3 condiciones. 







