####EJERCICIO 1:####

library("ape") #Llamar la libreria
x <- paste("AJ5345", 26:49, sep = "") # crear vector con codigo de Genbank
x <- c("Z73494", x)  #agregarle ese codigo a x
sylvia.seq <- read.GenBank(x) # leer desde genbank los codigos guardados en x
sylvia.clus <- clustal(sylvia.seq) #alinear por clustal

# claculo de distancia y bootstrap
alin<-dist.dna(sylvia.clus) # construir matriz de distancia cambiando la clase a dist
tree<-nj(alin) #construir arbol por metodo neighbor joining
tree<-root(tree, "AJ534526", resolve.root = T) #enraizar arbol y resolver como nodo bifurcado
funcion<-function(xx) root(nj(dist.dna(xx, p=TRUE)), "AJ534526") #definir función para hacer bootstrap
boot<-boot.phylo(tree,sylvia.clus, FUN = funcion, B=200, rooted  = T) #Bootstrap con 200 replicas
taxa.sylvia <- attr(sylvia.seq, "species") # crear vector con nombres de especies guardados en Sylvia.seq
names(taxa.sylvia) <- names(sylvia.seq) #asignar a los nombres guardados en taxa.sylvia, los nombres guardados en sylvia.seq
nj.est <- tree # guardar arbol en un nuevo objeto para poder modificarlo
nj.est$tip.label <- taxa.sylvia[tree$tip.label] # asignar los nombres de taxa.sylvia a los labels del arbol creado anteriormente
plot(nj.est, no.margin = TRUE) # graficar arbol sin margenes
#agregar soporte a los nodos diviendo el resultado del boot en 200 y tilizando 2 decimales. No usar marco y definir posición y tamaño:
nodelabels(round(boot/200, 2), frame= "none", adj = c(-0.25,0), cex=0.8) 
add.scale.bar(length = 0.01) # agregar linea de escala 

#calculo de likelihood:
write.dna(sylvia.clus, "sylvia.txt") #generar archivo en formato newick
phyml.sylvia<-phymltest("sylvia.txt", execname="~/phyml")
summary(phyml.sylvia) #mostrar comparación entre modelos
TR<-read.tree(silvia.tree.txt)
mltr.sylvia<-TR[[28]]
mltr.sylvia$tip.label<-taxa.sylvia[mltr.sylvia$tip.label]
mltr.sylvia<-root(mltr.sylvia,"Chamaeae_fasciata", resolve.rast=T)
plot.phylo(mltr.sylvia no.margin=T)
at.scale.bar(length=0,1)                      
                      

                      
     
                      
                      
install.packages("phangorn")
library(phangorn)
