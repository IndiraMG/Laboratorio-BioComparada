### EJERCICIO 1:
library("ape")   #cargar la libreria
data("bird.orders")   #cargar datos
plot(bird.orders, no.margin = T)  #graficar arbol
#crear o cargar vector con valores de bootstrap
bs.ml <- c(NA, 88, 76, 73, 71, 100, 45, 81, 72, 67, 63, 100, 100, 88, 76, 73, 71, 100, 45, 72, 67, 100, 100) 
color<-character(length(bs.ml))  #crear vector de colores con la longitus de bs.ml
color[bs.ml >= 90] <- "red"   #asignar rojo cuando soporte es mayor o igual a 90
color[bs.ml < 90 & bs.ml >= 70] <- "orange"  #asignar naranja cuando soporte es menor a 90 y mayor o igual a 70
color[bs.ml < 70] <- "yellow"  #asignar amarillo cuando soporte es menor a 70
nodelabels(node = 25:45, pch = 21, cex = 2, bg = color[-1])  #agregar etiquetas a los nodos
legend("topleft", legend = c("90 <= BP", "70 <= BP < 90", "BP < 70"), pch = 21, pt.bg = c("red","orange","yellow"), 
       pt.cex = 2, bty = "n") #agregar leyenda en esquina superior izquierda

### EJERCICIO 2:
relaciones <- which.edge(bird.orders, 1:5) #Guardar en vector las relaciones que involucran los nodos 1 al 5
color2 <- rep("black", Nedge(bird.orders)) #Crear vector con color negro y longitud igual a nodos totales
color2[relaciones] <- "blue"  #cambiar las posiciones de color2, que estan guardadas en relaciones, por "blue"
#Dibujar filograma usando color2 para las ramas seleccionadas:
plot.phylo(bird.orders, "phylogram", T, edge.color = color2, edge.width = 2, no.margin = F, show.tip.label = T)  

info_plot<-plot(bird.orders) #guardar información del gráfico del arbol en un vector
plot(bird.orders,show.tip.label = F, x.lim = info_plot$x.lim ) #Graficar sin nombres
# agregar nombres con los colores deseados:
tiplabels(bird.orders$tip.label, adj = -0.1, col = c(rep("blue",5), rep("black", 18)), frame="n")


### EJERCICIO 3: Función que recibe árbol y vector con estados de caracter y devuelve filograma con ramas coloreadas dependiendo 
#del tipo de cambio:

col.cambio <- function(vector, arbol) {
  
  if (length(vector)==sum(length(arbol$tip.label), arbol$Nnode)) # verificar que la longitud del vector corresponda a los nodos totales del árbol
    {
    
    colores<-character(nrow(arbol$edge))   #crear vector de caracteres con longitud igual al número de filas en edge
    
    for (i in 1:nrow(arbol$edge)) { #crear loop que haga tantos ciclos como filas hay en edge
      if(vector[arbol$edge[i,1]]==vector[arbol$edge[i,2]]) {colores[i]<-"black"} # asignar color negro cuando no hay cambio
      if(vector[arbol$edge[i,1]] < vector[arbol$edge[i,2]]) {colores[i]<-"red"}  # asignar color rojo cuando el cambio va de un valor menor a uno mayor
      if(vector[arbol$edge[i,1]] > vector[arbol$edge[i,2]]) {colores[i]<-"blue"} # asignar color azul cuando el cambio va de un valor mayor a uno menor
    }
   plot.phylo(arbol, "phylogram", T, edge.color = colores, edge.width = 2, no.margin = F, show.tip.label = T)
   # Graficar filograma con ramas con los colores asignados anteriormente
  } 

  else {print("Error: El número de nodos no coincide con la longitud del vector")}
  #mensaje de error en caso de que no se cumpla la primera condición
}


### EJERCICIO 4 (Adicional):
ml<- read.tree("ET_LikeLihood.tre") #leer arbol de MrBayes
mb<- read.tree("mrbayes.tre")       #leer arbol de Maximun likelihood
layout(matrix(1:2, 1, 2))           #dividir la pantalla en dos partes iguales verticalmente
par(mar = c(4, 0, 0, 0))            #Asignar margenes a las graficas
plot.phylo(ml, type="phylogram", use.edge.length = F, x.lim = c(0,30)) #graficar arbol de ml sin long de ramas
nodelabels(ml$node.label, bg="white", frame = "none", adj = c(-0.1,0.5), font=1, cex=0.8)  #agregar valores de soporte a los nodos
plot.phylo(mb, type="phylogram", direction = "l", use.edge.length=F, x.lim = c(0,30))   # graficar arbol de mb hacia la izquierda
nodelabels(mb$node.label, bg="white", frame = "none", adj = c(1.2,0.5), font=1, cex=0.8) #agregar soporte a los nodos
#lo anterior asume topologías distintas y por eso se muestran los tiplabels de ambos arboles, en caso de que las topologías
#sean iguales, se divide la pantalla en tres, para graficar a cada extremo las topologías sin tiplabels y graficar los tiplabels
# en el centro, así:
par( mfrow= c(1,3)) #dividir la pantalla en una fila y tres columnas
# graficar filograma, usando long de ramas, limites ajustados en eje x, sin tiplabels y sin margen: 
plot.phylo(ml, type="phylogram", use.edge.length = T, x.lim = c(0,0.35), show.tip.label = F, no.margin = T)
#agregar soportes a los nodos, sin marco, ajustar posición de las etiquetas, asignar fuente y tamaño:
nodelabels(ml$node.label, frame = "none", adj = c(-0.1,0.5), font=1, cex=0.8)
#graficar 22 lineas punteadas color gris, para ubicar mejor el taxon correspondiente a cada rama:
abline(h=1:22, lty=2, col="gray")
#graficar la topología con edge en blanco, para mostrar solo los tiplabels y ajustar limites para centrar:
plot(ml,type= "phylogram", edge.color = "white", show.tip.label = T, use.edge.length = F, x.lim = c(20,40), adj= 0.5, cex=1.5)
# graficar segunda topología tipo filograma, limites ajustados en eje x, sin tiplabels, sin margen y hacia la izquierda:
plot.phylo(mb, type="phylogram", use.edge.length=T, x.lim = c(0.04,0.3), show.tip.label = F, no.margin = T, direction = "l" )
#agregar soportes a los nodos, sin marco, ajustar posición de las etiquetas, asignar fuente y tamaño:
nodelabels(mb$node.label, frame = "none", adj = c(1.2,0.5), font=1, cex=0.8)
#graficar 22 lineas punteadas color gris, para ubicar mejor el taxon correspondiente a cada rama:
abline(h=1:22, lty=2, col="gray")




















