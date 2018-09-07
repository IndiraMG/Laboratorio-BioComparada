####EJERCICIO 1:####
install.packages("phangorn")
library(phangorn)
library("ape") #Llamar la libreria
x <- paste("AJ5345", 26:49, sep = "") # crear vector con codigo de Genbank
x <- c("Z73494", x)  #agregarle ese codigo a x
sylvia.seq <- read.GenBank(x) # leer desde genbank los codigos guardados en x
sylvia.clus <- clustal(sylvia.seq) #alinear por clustal

# claculo de distancia:
alin<-dist.dna(sylvia.clus) # construir matriz de distancia cambiando la clase a dist
tree<-nj(alin) #construir arbol por metodo neighbor joining
plot.phylo(tree) #graficar

BOOTSTRAP<-boot.phylo(tree,sylvia.clus, FUN = function(xx) nj(dist.dna(xx)), B=1000, trees = T)
s <- as.splits(BOOTSTRAP$trees)
