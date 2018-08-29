### Ejercicio 1. ###
library("ape")   #cargar la libreria
data("bird.orders")   #cargar datos
plot(bird.orders, no.margin = T)  #graficar arbol
bs.ml <- c(NA, 88, 76, 73, 71, 100, 45, 81, 72, 67, 63, 100, 100, 88, 76, 73, 71,
           100, 45, 72, 67, 100, 100) #crear o cargar vector con valores de bootstrap
color<-character(length(bs.ml))  #crear vector de colores con la longitus de bs.ml
color[bs.ml >= 90] <- "red"   #asignar "rojo" cuando soporte es mayor o igual a 90
color[bs.ml < 90 & bs.ml >= 70] <- "orange"  #asignar "orange" cuando soporte es menor a 90 y mayor o igual a 70
color[bs.ml < 70] <- "yellow"  #asignar "yellow" cuando soporte es menor a 70
nodelabels(node = 25:45, pch = 21, cex = 2, bg = color[-1])  #agregar etiquetas a los nodos
legend("topleft", legend = c("90 <= BP", "70 <= BP < 90", "BP < 70"),  #agregar leyenda
       pch = 21, pt.bg = c("red","orange","yellow"), pt.cex = 2, bty = "n")

### Ejercicio 2 ###
relaciones <- which.edge(bird.orders, 1:5) #Guardar en vector, las relaciones que involucran los nodos 1 al 5
color2 <- rep("black", Nedge(bird.orders)) #Crear vector con color negro y tantas posiciones como nodos hay
color2[relaciones] <- "blue"  #cambiar las posiciones de color2, que estan guardadas en relaciones, por "blue"
plot.phylo(bird.orders, "phylogram", T, edge.color = color2, #Dibujar filograma usando color2 para las ramas seleccionadas
     edge.width = 2, no.margin = F, show.tip.label = T)  

info_plot<-plot(bird.orders) #guardar información del gráfico del arbol
plot(bird.orders,show.tip.label = F, x.lim = info_plot$x.lim ) #Graficar sin nombres
tiplabels(bird.orders$tip.label, adj = -0.1, # agregar nombres con los colores deseados
          col = c(rep("blue",5), rep("black", 18)), frame="n")


## Ejercicio 3: Función que recibe árbol y vector con estados de caracter y devuelve
## filograma con ramas coloreadas dependiendo del tipo de cambio:
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

col.cambio(vector = carac, arbol = bird.orders)



















