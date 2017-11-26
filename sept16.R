#EXAMEN 14 SEPTIEMBRE 2016

#1 DADO n ENTERO POSITIVO, SUMAR ENTEROS DE 1 A n Y VERIFICAR SI LA SUMA ES MULTIPLO DE n.

n<-12
sc<-ifelse(sum((1:n)^2)%%n==0,paste(sum((1:n)^2),' es múltiplo de ',n),paste(sum((1:n)^2),' no es múltiplo de',n))
sc


##############################################################################
##############################################################################
##############################################################################
##############################################################################

#2 SIMULE EL LANZAMIENDO DE UN DADO 1000000 DE VECES. MUESTRE LA CANTIDAD DE VECES QUE SALE CADA NÚMERO

#set.seed(1234) #ESTA LÍNEA NO ES OBLIGATORIA, ES PARA QUE SIEMPRE SALGA EL MISMO RESULTADO (FIJO LA SEMILLA ALEATORIA)

# a. CON for
simulacion1<-NULL
t.inicio1<-proc.time()
for(i in 1:1000000)
  simulacion1<-append(simulacion1,sample(1:6,1))
table(simulacion1)
t.final1<-proc.time()

# b. VECTORIZANDO
t.inicio2<-proc.time()
simulacion2<-sample(1:6,1000000,replace=TRUE)
table(simulacion2)
t.final2<-proc.time()

# Comparamos los tiempos de ejecución
tiempo1<-t.final1-t.inicio1
tiempo2<-t.final2-t.inicio2

tiempo1
tiempo2

#otra forma:

system.time({
  simulacion1<-NULL
  for(i in 1:1000000)
    simulacion1<-append(simulacion1,sample(1:6,1))
  table(simulacion1)
})

system.time({
  simulacion2<-sample(1:6,1000000,replace=TRUE)
  table(simulacion2)
})


##############################################################################
##############################################################################
##############################################################################
##############################################################################

#3. DADO EL SIGUIENTE VECTOR:
x<-c(8,23,15,NA,21,7,NA,NA,6,91,NA,12)

# Escribe un programa que devuelva las posiciones de los valores perdidos de x
which(is.na(x))

# Escribe un programa que de el número de valores no perdidos en x
sum(!is.na(x))

# Escribe un programa que cambie los valores peridos de x por la mediana de los valores no perdidos.
ifelse(is.na(x),median(x,na.rm=T),x)

# Escribe una función con argumento de entrada un vector x y que devuelva un vector con los valores 
# reemplazados por la media de los valores no perdidos de x

reemplazo<-function(x) ifelse(is.na(x),mean(x,na.rm=TRUE),x)
reemplazo(x)


##############################################################################
##############################################################################
##############################################################################
##############################################################################

#4. DADO EL DATA.FRAME mtcars 

# a. PROGRAMA QUE DIBUJE UN GRÁFICO DE DISPERSIÓN DE LA VARIABLE hp FRENTE A mpg, UTILIZANDO UN COLOR DIFERENTE PARA CADA 
#    NIVEL DE LA VARIABLE carb.

data(mtcars)
str(mtcars)
plot(mtcars$mpg~mtcars$hp,
     col=mtcars$carb,
     main='DIAGRAMA DE DISPERSIÓN',
     xlab='hp',ylab='mpg')

#b. PROGRAMA QUE ORDENE LAS FILAS DEL data.frame EN FUNCIÓN DE LA COLUMNA hp.

orden<-order(mtcars$hp)
mtcars[orden,]

# CON dplyr
library(dplyr)
mtcars %>% arrange(hp)


#c. PROGRAMA QUE MUESTRE EL NÚMER ODE LA OBSERVACIÓN DONDE SE ALCANZA EL MÁXIMO DE LA RAZÓN ENTRE mpg Y wt.
razon<-mtcars$mpg/mtcars$wt
which.max(razon)


##############################################################################
##############################################################################
##############################################################################
##############################################################################

#5. DADO EL DATA.FRAME mtcars 

#a. ESCRIBE UN PROGRAMA QUE CREE EL data.frame wine CON LOS NOMBRES DE LAS VARIABLES: ...

# guardamos los nombres
nombres<-c('Type','Alcohol','MalicAcid','Ash','Alcalinity.of.ash',
           'Magnesium','Total.phenols','Flavanoids','Nonflavanoid.phenols',
           'Proanthocyaninis','Color.intensity','Hue','Diluted.wines','Proline')

# leemos el fichero externo y lo guardamos en el data.frame wine
setwd('C:/Rproy/soft_estad_ii_upm')
wine<-read.table(paste(getwd(),'/wine.txt',sep=''),sep=',')
str(wine)

# asignamos los nombres deseados, y comprobamos la estructura
colnames(wine)<-nombres
str(wine)

# b. ESCRIBE UN PROGAMA QUE CALCULE LA MEDIANA DE Alcohol y Malic.Acid PARA CADA TIPO DE VINO (Type).

tapply(wine$Alcohol,wine$Type,median)
tapply(wine$MalicAcid,wine$Type,median)

# c. ESCRIBE UN PROGRAMA UE CUENTE EL NÚMERO DE OBSERVACIONES CON Alcohol MAYOR QUE 13 Y Proline MENOR QUE 650.

sum(wine$Alcohol>13 & wine$Proline<650)

#con dplyr
wine %>% filter(Alcohol>13,Proline<650) %>% summarise(n())

# d. ESCRIBE UN PROGRAMA QUE REALICE UN GRÁFICO DE BARRAS MOSTRANDO EL NÚMERO DE VINOS DE CADA TIPO QUE HAY EN EL 
# data.frame.

wine$Type<-factor(wine$Type,levels=1:3,labels=c('Tipo 1','Tipo 2','Tipo 3'))
barplot(table(wine$Type),
        main='Gráfico de barras',
        las=2)
#el parámetro 'las = 0,1,2,3' rota los valores de los ejes


##############################################################################
##############################################################################
##############################################################################
##############################################################################

#6. DADO LA FUNCIÓN DEL ENUNCIADO

# a. DIBUJA LA FUNCIÓN f(x) UTILIZANDO EL VALOR 1 EN LA OPCIÓN type.
