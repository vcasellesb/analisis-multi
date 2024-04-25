tabla <- c(6,1,4,2,1,3,25,2,11,11,0,20)
tabla <- matrix(tabla, ncol=3)
rownames(tabla) <- c("A1","A2","A3","A4")
colnames(tabla) <- c("E1","E2","E3")


# Cargamos los datos
tabla <- read.table("clipboard")
tabla.N <- as.table(as.matrix(tabla))

# Tabla de frecuencias relativas
n <- sum(tabla.N)
tabla.F <- tabla.N/n
round(tabla.F, 4)

# Frecuencias relativas marginales
margin.f <- margin.table(tabla.F, 1)
margin.c <- margin.table(tabla.F, 2)

# otra forma
margin.f <- apply(tabla.F, 1, sum)
margin.c <- apply(tabla.F, 2, sum)

# Para ver la tabla con las marginales
round(addmargins(tabla.F), 4)

# Frecuencias relativas condicionadas por filas
tabla.P <- diag(1/margin.f) %*% tabla.F
tabla.P <- sweep(tabla.F, 1, margin.f, "/") # más eficiente

# o directamente de la tabla.N de frecuencias absolutas
perfiles <- prop.table(tabla.N, 1)

#####################################################
# Representación en dimensión 3 con el paquete plotly
#####################################################
library(plotly)
graph3d <- plot_ly( x = c( 1, 0, 0, 1 ) ,
                    y = c( 0, 1, 0, 0 ) ,
                    z = c( 0, 0, 1, 0 ) ,
                    type = 'scatter3d' , mode = 'lines' ,
                    name = 'plano') %>%
           add_trace( x = perfiles[,1] ,
                      y = perfiles[,2] ,
                      z = perfiles[,3] ,
                      type = 'scatter3d' , mode = 'markers',
                      name = 'perfiles')
graph3d

# añadir el centroide al gráfico 3D
graph3d %>%
   add_trace( x = margin.c[1] ,
              y = margin.c[2] ,
              z = margin.c[3] ,
              type = 'scatter3d' , mode = 'markers',
              name = 'centroide')

# Representación bidimensional
tabla.x <- 1 - perfiles[,1] - perfiles[,3]/2
tabla.y <- perfiles[,3] * sqrt(3)/2
plot.new()
lines(c(0,1,0.5,0),c(0,0,sqrt(3)/2,0), col="gray", lwd=2)
text(c(0,1,0.5),c(0,0,sqrt(3)/2),labels=colnames(tabla))
text(tabla.x,tabla.y,labels=rownames(tabla))
# Punto medio o centroide de las filas (es la marginal de las columnas)
text(1-margin.c[1]-margin.c[3]/2,margin.c[3]*sqrt(3)/2,labels=c("C"),font=2)

# Distancia ji-cuadrado entre filas
nf <- nrow(tabla.N)
D2.chisq <- matrix(0,nf,nf)
for(i in 1:(nf-1))
  for(j in i:nf)
    D2.chisq[i,j] <- 
  t(tabla.P[i,]-tabla.P[j,]) %*% diag(1/margin.c) %*% (tabla.P[i,]-tabla.P[j,])
D2.chisq <- D2.chisq + t(D2.chisq)
rownames(D2.chisq) <- colnames(D2.chisq) <- rownames(tabla.N)
#
D2.chisq

# Distancia de las filas a su centroide
dc <-  vector(mode="numeric",nf)
for(i in 1:nf)
  dc[i] <- t(tabla.P[i,]-margin.c) %*% diag(1/margin.c) %*% (tabla.P[i,]-margin.c)
names(dc) <- rownames(tabla.N); dc

# Inercia total
as.numeric(dc %*% margin.f)
# coincide con la inercia total con la función chisq.test()
as.numeric(chisq.test(tabla.N)$stat)/n

# La distancia ji-cuadrado entre perfiles equivale a la distancia 
# euclídea entre los vectores transformados y_i
Y <- tabla.P %*% diag(1/sqrt(margin.c))
colnames(Y) <- colnames(tabla.N); Y
# Transformación del centroide
cc <- margin.c %*% diag(1/sqrt(margin.c)); cc
# Distancias euclídeas al cuadrado entre vectores transformados y su centroide
as.matrix(dist(rbind(cc,Y)))[1,2:5]^2

###########################
# Escalado multidimensional
###########################
mds <- cmdscale(sqrt(D2.chisq), eig=TRUE); mds

# Mapa del MDS para las distancias ji-cuadrado entre filas
library(MASS)
eqscplot(mds$points,ty="n",xlab="PC1",ylab="PC2",xlim=c(-1.5,0.8))
abline(v=0,h=0, col="gray",lty=4)
text(mds$points[,1],mds$points[,2],labels=rownames(tabla.N),cex=0.8)
  
# Frecuencias relativas condicionadas por columnas
tabla.Pc <- tabla.F %*% diag(1/margin.c); tabla.Pc
colnames(tabla.Pc) <- colnames(tabla.N)
# Distancia ji-cuadrado entre columnas
nc <- ncol(tabla.N)
D2c.chisq <- matrix(0,nc,nc)
for(i in 1:(nc-1))
  for(j in i:nc)
    D2c.chisq[i,j] <-
  t(tabla.Pc[,i]-tabla.Pc[,j]) %*% diag(1/margin.f) %*% (tabla.Pc[,i]-tabla.Pc[,j])
D2c.chisq <- D2c.chisq + t(D2c.chisq);
rownames(D2c.chisq ) <- colnames(D2c.chisq) <- colnames(tabla.N); D2c.chisq

# Escalado multidimensional sobre la matriz de distancias entre columnas
mds.c <- cmdscale(sqrt(D2c.chisq),eig=TRUE); mds.c

# Representación de filas y columnas
eqscplot(mds.c$points,ty="n",xlab="PC1",ylab="PC2",xlim=c(-1.6,1),ylim=c(-0.6,0.8))
abline(v=0,h=0, col="gray",lty=4)
text(mds$points[,1],mds$points[,2],labels=rownames(tabla.N),cex=0.8)
text(mds.c$points[,1],mds.c$points[,2],labels=colnames(tabla.N),cex=0.8)

#####################################
# Análisis de componentes principales
#####################################
Df <- diag(margin.f)
Dc <- diag(margin.c)
Dfmh <- diag(1/sqrt(margin.f))
Dcmh <- diag(1/sqrt(margin.c))
Z <- Dfmh %*% (tabla.F - margin.f %o% margin.c) %*% Dcmh
Z.svd <- svd(Z)

# Coordenadas principales (pc) y estándares (sc)
filas.sc  <- Dfmh %*% Z.svd$u
cols.sc <- Dcmh %*% Z.svd$v
filas.pc <- filas.sc %*% diag(Z.svd$d)
cols.pc <- cols.sc %*% diag(Z.svd$d)

# Inercias principales
inercias <- Z.svd$d^2
# Inercia total
sum(inercias)

# Mapa de distancias entre las filas
eqscplot(-filas.pc[,1:2],type="n",xlab="PC1",ylab="PC2",ylim=c(-0.8,0.4))
abline(v=0,h=0, col="gray",lty=4)
text(-filas.pc[,1],-filas.pc[,2],labels=rownames(tabla.N),cex=0.8)

########################
# CA con la función ca()
########################
library(ca)
ca(tabla.N)
ca.res <- ca(tabla.N)

# Inercias principales
ca.res$sv^2

# Coordenadas estándares
ca.res$rowcoord
filas.sc[,1:2]

ca.res$colcoord
cols.sc[,1:2]

# Representación simétrica
plot(ca.res)
title(main="Solución simétrica", line=1)
# Representación asimétrica
plot(ca.res, map= "rowprincipal")
title(main="Solución asimétrica",line=1)
  
# Mejor con la función CA() del paquete FactoMineR

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/