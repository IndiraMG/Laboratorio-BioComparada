
library("ape") #Llamar la libreria
x <- paste("AJ5345", 26:49, sep = "") # crear vector con codigo de Genbank
x <- c("Z73494", x)  #agregarle ese codigo a x
sylvia.seq <- read.GenBank(x) # leer desde genbank los codigos guardados en x
sylvia.clus <- clustal(sylvia.seq) #alinear por clustal

#METODO DE DISTANCIA Y BOOTSTRAP:
alin<-dist.dna(sylvia.clus) # construir matriz de distancia cambiando la clase a dist
tree<-nj(alin) #construir arbol por metodo neighbor joining
tree<-root(tree, "AJ534526", resolve.root = T) #enraizar arbol y resolver como nodo bifurcado
funcion<-function(xx) root(nj(dist.dna(xx, p=TRUE)), "AJ534526") #definir función para hacer bootstrap
boot<-boot.phylo(tree,sylvia.clus, FUN = funcion, B=200, rooted  = T) #Bootstrap con 200 replicas
taxa.sylvia <- attr(sylvia.seq, "species") # crear vector con nombres de especies guardados en Sylvia.seq
names(taxa.sylvia) <- names(sylvia.seq) #asignar a los nombres guardados en taxa.sylvia, los nombres guardados en sylvia.seq
tree$tip.label <- taxa.sylvia[tree$tip.label] # asignar los nombres de taxa.sylvia a los labels del arbol creado anteriormente
plot(tree, no.margin = TRUE) # graficar arbol sin margenes
#agregar soporte a los nodos diviendo el resultado del boot en 200 y tilizando 2 decimales. No usar marco y definir posición y tamaño:
nodelabels(round(boot/200, 2), frame= "none", adj = c(-0.25,0), cex=0.8) 
add.scale.bar(length = 0.01) # agregar linea de escala 

#CALCULO DE LIKELIHOOD Y BOOTSTRAP:
write.dna(sylvia.clus, "sylvia.txt") # generar archivo con alineamiento en formato newick 
phyml.sylvia<-phymltest("sylvia.txt", execname="phyml") #llamar matriz y ejecutar phyml
summary(phyml.sylvia) # muestra resultados del test phyml
plot(phyml.sylvia) #permite observar cual es el mejor modelo
TR<-read.tree("sylvia.txt_phyml_tree.txt") #leer y guardar archivo de arboles generado por phyml
mltr.sylvia<-TR[[28]] # extraer el árbol 28 (último árbol) y guardar en nuevo objeto
mltr.sylvia$tip.label<-taxa.sylvia[mltr.sylvia$tip.label] # asignar los nombres de taxa.sylvia a los labels del arbol mltr.sylvia
mltr.sylvia<-root(mltr.sylvia,"Chamaea_fasciata") # enraizar
plot.phylo(mltr.sylvia) #graficar árbol
add.scale.bar(length=0.1) #graficar barra de escala                      
install.packages("phangorn") #instalar phangorn
library(phangorn) #llamar paquete phangorn
clus2<-sylvia.clus #guardar alineamiento en nuevo objeto
dimnames(clus2)[[1]]<- attr(sylvia.seq, "species") # cambiar las etiquetas de los genes por los nombres de las especies
fit<-pml(tree, as.phyDat(clus2)) # Realizar busqueda inicial con arbol creado en metodo de distancia
# Asignar parametros y optimizar busqueda:
optree<-optim.pml(fit, optInv = T, optGamma = T, optBf = T, model="GTR", optRooted = T,rearrangement = "stochastic")
bs<-bootstrap.pml(optree, bs=200) #Realizar bootstrap con 200 replicas
plotBS(optree$tree,bs, type = "phylogram", use.edge.length = TRUE) #Graficar abol con valores de bootstrap

# GRAFICAR COMPARACIÓN DE TOPOLOGÍAS Y SOPORTE:
par(mfrow= c(1,3), mai=c(0,0,0,0)) #dividir pantalla en una fila y tres columnas
# graficar arbol tipo filograma, con long de ramas, ajustar limites en x, no mostrar tiplabels y sin margen:
plot.phylo(tree, type="phylogram", use.edge.length = T, x.lim = c(0,0.095), show.tip.label = F, no.margin = T)
nodelabels(round(boot/200, 2), frame= "none", adj = c(-0.25,0), cex=0.8) #agregar soporte a los nodos 
# graficar arbol tipo filograma, blanco y sin tiplabels, fijando limites en eje x:
plot(tree,type= "phylogram", edge.color = "white", show.tip.label = F, use.edge.length = F, x.lim = c(18,30))
tiplabels(tree$tip.label, adj = 0.5, frame="none", cex=1.5) #graficar tiplabels en el centro de la pantalla
#graficar arbol de likelihood con valores de bootstrap, sin tiplabels y hacia la izquierda:
plotBS(optree$tree, bs, type="phylogram", x.lim = c(0.35,0.5), show.tip.label = F, no.margin = T, direction = "l" )




