
#se carga librerías necesarias
install.packages("kohonen")
library(kohonen)
require(RColorBrewer)


# Definición de paleta de color para clustering-
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2','#7f7f7f','#17becf', '#aec7e8', '#ffbb78', '#98df8a', '#ff9896')

source('coolBlueHotRed.R') #llamada a script externo guardado en el directorio de trabajo.


#carga de datos desde ubicación de la PC se guarda en variable dataHSDPA

dataHSDPA <- read.csv(choose.files()) #seleccionamos dataset del archivo de trabajo DatasetTFM_CSV.



#Exploración de data set nombres de columnas

head(dataHSDPA,2) #exploramos las dos primeras líneas del dataset.

summary(dataHSDPA)

# Se guarda nombre de coulumnas para análisis en variable vector dataHSDPA_meas1

dataHSDPA_meas1 <- c("VS.MeanRTWP.dBm.",
                     "HSUPA.User.Num",
                     "DCH.User.Num", 
                     "HSDPA.User.Num",
                     "FACH.User.Num",
                     "PD85",
                     "FR_DROP_DL",
                     "FR_DROP_UL",
                     "HSUPA.User.Throughput..kbit.s.",
                     "HSDPA.User.Throughput..kbit.s."
                     )

summary(dataHSDPA[dataHSDPA_meas1])

#----------------------------Escalamiento de datos------------------------

sc_dataHSDPA_meas1<-scale(dataHSDPA[dataHSDPA_meas1])

head(sc_dataHSDPA_meas1,2) #Mostramos las dos primera filas de datos escalados.

summary(sc_dataHSDPA_meas1) #resumen de datos escalados

#comprobación de equivalencia con el valor mínimo de la columna RTWP.
sc_valor<-((min(dataHSDPA$VS.MeanRTWP.dBm.))-(mean(dataHSDPA$VS.MeanRTWP.dBm.)))/(sd(dataHSDPA$VS.MeanRTWP.dBm.))
sc_valor



#-------------------------- APLICACION ALGORITMO SOM------------------------

#aplicación de SOM y escalamiento de datos
#se usan número de iteraciones por default rlen=100
#se define grid de 6 X 6.

set.seed(100)  # Fijamos una semilla.
HSDPA.SOM1 <- som(sc_dataHSDPA_meas1, grid = somgrid(6, 6, "hexagonal"))


#-------------------------- VISUALIZACIONES ----------------------------------

#proceso de aprendizaje,permite apreciar la convergencia del algorithmo vs número de iteraciones..
#muestra la evolución de la distancia promedio la la celda más cercana en el mapa.
par(mfrow = c(1, 1)) #define matriz de gráficas en presentación / 1 X 1 una gráfica por presentación.

plot(HSDPA.SOM1, type = "changes", main = "Proceso de Entrenamiento") #variable "changes" muestra la suman de distanciaa media al nodo más cercano vs iteración.

plot(HSDPA.SOM1, type ="codes", main = "Vectores del libro de Códigos")  #visualización por defecto de algoritmo.
                  #ubicación de nodos cada una con gráfica de pétalos con peso de atributo.


#"counts"==cuenta de elementos dentro de cada nodo.
plot(HSDPA.SOM1, type = "counts", main="Cuenta de Elementos por Nodo", palette.name=coolBlueHotRed)

#"quality"== muestra la calidad del mapa, 
# muestra la distancia media de cada objeto asignado en cada nodos hacia el vector 
#del libro de códigos de ese nodo.
#Cuanto más pequeñas son las distancias, 
#mejor están representados los objetos por los vectores del libro de códigos.

plot(HSDPA.SOM1, type = "quality", main="Nodo Calidad/Distancia", palette.name=coolBlueHotRed)

#"dist.neighbours"=== muestra la suma de las distancias a todos los vecinos inmediatos. 
#Este tipo de visualización también se conoce como diagrama de matriz U. 
#Se puede esperar que las unidades cercanas a un límite de clase
#tienen distancias medias más altas a sus vecinos. 

plot(HSDPA.SOM1, type="dist.neighbours", main = "SOM - distancia entre vecinos", palette.name=grey.colors)

#"codes"==gráfica por defecto de SOM, muestra ubicación de nodos y gráfica
#de pétalos de los atributos.
plot(HSDPA.SOM1, type = "codes")

#"mapping"== muestra como estan distribuidos los elementos en los nodos.

plot(HSDPA.SOM1, type = "mapping", pchs = 20, main = "Distribución de elementos por nodo")



#para revisar lacorrespondecia de nodos en el mapa.
print(HSDPA.SOM1$grid)


# resumen de elemto resultante.
summary(HSDPA.SOM1)

# Plot de variables escaladas /normalizadas, se asigna a la caliable "var" el númeor de columna de interes.

var <- 10 #define the variable to plot
plot(HSDPA.SOM1, type = "property", 
     property = getCodes(HSDPA.SOM1)[,var], 
     main=colnames(getCodes(HSDPA.SOM1))[var], 
     palette.name=coolBlueHotRed)


# Plot de una variable en escala original de los datos de entrenamiento.

var <- 1 #define the variable to plot

#se sumariza infomración de variable especificada, "var" calculando su media, de acuerdo a clasificación.
#se guarda resultado en "var_unscaled", la columna seleccionada [,2]
var_unscaled <- aggregate(as.numeric(sc_dataHSDPA_meas1 [,var]), 
                by=list(HSDPA.SOM1$unit.classif), 
                FUN=mean, 
                simplify=TRUE)[,2]


plot(HSDPA.SOM1, 
     type = "property", 
     property=var_unscaled, 
     main=names(sc_dataHSDPA_meas1)[var], 
     palette.name=coolBlueHotRed)

rm(var_unscaled)

par(mfrow = c(1, 1))
#------------------------------vizualización de cualquier variable desde un menu---------------
#función que permite plotear cualquier variable del elemento SOM desde un menu de selección.
source('plotHeatMap.R')#llamada a script externo guardado en el directorio de trabajo.

plotHeatMap(HSDPA.SOM1, dataHSDPA, variable=0)


#------------ploteamos una matriz de gráficas de toda las variables---------

#creamos una matriz grafica 
par(mfrow=c(4,3)) 

#Usamos una función de bucle para generar las grpaficas en secuencia.
#seleccionamos cada una de las columnas y hacemos una sumarización (aggregate) calculando el promedio (FUN=mean)
#por cada elemento clasificado usando la consulta $unit.classif.

for (j in 1:ncol(dataHSDPA[dataHSDPA_meas1])){
        plot(HSDPA.SOM1,type="property",
             property= aggregate(as.numeric(dataHSDPA[dataHSDPA_meas1][,j]), 
                                 by=list(HSDPA.SOM1$unit.classif), 
                                 FUN=mean, 
                                 simplify=TRUE)[,2],
             palette.name=coolBlueHotRed,
             main=colnames(sc_dataHSDPA_meas1)[j],cex=1) 
        } 



#----------------Agrupamiento de  LIBRO DE CODIGOS --------------------------

#-------------evaluamos el número de clusters mas adecuado con el metodo del codo.-------------------

code_book <- getCodes(HSDPA.SOM1) #se extrae el libro de códigos de elemento SOM y se guarda en la variable "code_book".

head(code_book,3)
code

# aplicamos algoritmo Kmeans para calular el resto de valores desde 2 al 35 y guardamos como vector wss[]
set.seed(100)

wss <- sum(kmeans(data.frame(code_book), centers=1)$withinss) #creamos la variable wss.
for (i in 2:35) {
                wss[i] <- sum(kmeans(data.frame(code_book), centers=i)$withinss)
                }

# graficamos número de clusters vs distancias medias entre códigos.
plot(1:35, 
     wss, 
     type="b", 
     xlab="Número de Clústeres",
     ylab="Suma de cuadrados dentro del clúster", 
     main="Suma de cuadrados dentro del clúster (WCSS)")


# -------fin de la evaluación  se observa que a partir de la iteración 10 la suma de cuadrados 
#dentro de los grupos se estabiliza, no se reduce significativamente, esto sugiere un npumero de clusters de 10.

#------------------------CLUSTERING DE NODOS ---------------------------


## usamos agrupameint aglomerativojerarquico para agrupar libro de códigos.

som_cluster <- cutree(hclust(dist(code_book)),10)

# Mostrar el mapa con diferente color para cada cluster.

plot(HSDPA.SOM1, 
     type="mapping", 
     bgcol = pretty_palette[som_cluster], 
     main = "Clusteres",
     shape = "straight")

som_cluster

# se añaden contornos a clusters formados.
add.cluster.boundaries(HSDPA.SOM1, som_cluster)

#plot 1 y 2 en la misma hoja

par(mfrow = c(1, 2))

#plot1
plot(HSDPA.SOM1, 
     type="mapping", 
     bgcol = pretty_palette[som_cluster], 
     main = "Clusters",
     shape = "straight")
        
add.cluster.boundaries(HSDPA.SOM1, som_cluster)

#plot2

plot(HSDPA.SOM1,shape = "straight" )


#mostrar el mismo plot con los codigos en lugar de solo colores.
par(mfrow = c(1, 1))
plot(HSDPA.SOM1, type="codes", bgcol = pretty_palette[som_cluster], main = "Clústeres",shape = "straight")
add.cluster.boundaries(HSDPA.SOM1, som_cluster)


# -------------------- FIN DE LA VIZUALISACION ---------------------------------


# ----------------------EXTRACCION Y EXPORTACIÓN  DE DATA ------------------------------------

#añadimos columna con la correspondencia de nodos y guardamos como nueva tabla "HSDPA_nodos"

HSDPA_nodos <- HSDPA.SOM1$unit.classif #extraemos correspondencia de nodos del elemento SOM.

HSDPA_nodosx <- cbind(dataHSDPA, HSDPA_nodos) #unimos tabla de clasificación por nodo con la tabla de datos originales.

som_cluster_dataframe <- cbind(c(1:36),as.data.frame(som_cluster)) # obtenemos correspondencia de nodos con cluster.


names(som_cluster_dataframe) <- c("HSDPA_nodos","som_cluster")  #cambiamos los nombres de las columnas.

HSDPA_node_cluster <- merge(som_cluster_dataframe,HSDPA_nodosx)

head(HSDPA_node_cluster,8)

write.csv(HSDPA_node_cluster,file="HSDPA_Nodo_Cluster.csv")     #guardamos archivo con correspondencia de nodos y cluster.

Aggregate_HSDPA <- aggregate(HSDPA_node_cluster,by = list(HSDPA_node_cluster$som_cluster),FUN = mean) #generamos vectores a escala original que caracteriza a cada cluster.

write.csv(Aggregate_HSDPA,file="HSDPA_Aggregado_CLuster.csv")                                         #guardamos tabla generada de promedios por cluster.



# Para guardar datos en archivos separados por cluster.

HSDPA_split <- split(HSDPA_node_cluster, HSDPA_node_cluster$som_cluster)
lapply(names(HSDPA_split), function(x){ write.csv(HSDPA_split[[x]], file = paste(x, ".csv", sep = ""))})




#-----------------Análisis de componentes principales ACP-----------------------


#definimos función para cáculo de correlación ponderada.
weighted.correlation <- function(v,w,grille){
  x <- grille$grid$pts[,"x"]
  y <- grille$grid$pts[,"y"] 
  mx <- weighted.mean(x,w) 
  my <- weighted.mean(y,w) 
  mv <- weighted.mean(v,w) 
  numx <- sum(w*(x-mx)*(v-mv))
  denomx <- sqrt(sum(w*(x-mx)^2))*sqrt(sum(w*(v-mv)^2)) 
  numy <- sum(w*(y-my)*(v-mv))      
  denomy <- sqrt(sum(w*(y-my)^2))*sqrt(sum(w*(v-mv)^2)) 
  #correlación pra dos ejes.
  res <- c(numx/denomx,numy/denomy) 
  return(res)
} 


#correlación para todas las columnas del libro de códigos.

#------------------------ANALISIS DE COMPONENTES PRINCIPALES--------------------

# Generamos una matriz de correlación entre variables.

cor(dataHSDPA[dataHSDPA_meas1]) 

# Calculamos las componentes principales y guardamos en la variable "acp".

acp<-prcomp(sc_dataHSDPA_meas1)

# Mostramos las componentes principales generadas.
acp 

# Revisamos cual es la proporción de la varianza explicada por cada componente principal.
summary(acp)

#criterio de kayser para determinar el numero de componentes principales.
desv_stand<-acp[[1]]

desv_stand

varianza1<-desv_stand^2

varianza1  # se observa que variabilidad la podremos describir con las tres primeras componentes principales.

#Extraemos las tres primeras componentes principales del objeto "acp" y las guardamos en tres variables.

CP1<-acp[[2]][,1]
CP2<-acp[[2]][,2]
CP3<-acp[[2]][,3]

# Unimos las componentes principales en una sola tabla.
comp_princi<-cbind(CP1,CP2,CP3)
comp_princi

#Extraesmos y guardamos  en una variable los individuos extraidos del objeto acp.
individuos<-acp$x[,1:3]
individuos

#circulos de correlaciones

#Instalamos el paquete "ade4" y llamamos la librería.
install.packages("ade4")
library(ade4)

x11() #Abrimos una ventana emergente.

#Generamos la grafica de correlación entre CP1 y CP2
s.corcircle(comp_princi[,-3], sub= "CP1,CP2")

x11() #Abrimos otra ventana emergente.

#Extraesmos y guardamos  en una variable los individuos extraidos del objeto acp.
individuos<-acp$x[,1:3]

#Grafica de coordenadas de individuos.
s.label(individuos[,-3], label=row.names(dataHSDPA[dataHSDPA_meas1]),sub= "coord de los individuos")

# Se generan las mismas grpaficas para el resto de componentes.

#CP1 y CP3
x11()
s.corcircle(comp_princi[,-2], sub= "CP1,CP3")

x11()
s.label(individuos[,-2], label=row.names(dataHSDPA[dataHSDPA_meas1]),sub= "coord de los individuos")

#CP2 y CP3
x11()
s.corcircle(comp_princi[,-1], sub= "CP2 y CP3")

x11()
s.label(individuos[,-1], label=row.names(dataHSDPA[dataHSDPA_meas1]), sub = "coord. de los individuos")


#__________ fin de ACP PRUEBA

