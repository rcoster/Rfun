# Feito por: Rodrigo Coster - 2009
# www.dadosaleatorios.com.br

jv <- function(x,y,R=T) {
 t <- Sys.time()
 plot.jv <- function(jogo) {
  if (is.null(dev.list())) {
    par(mar=rep(0,4),xaxs='i',yaxs='i')
    plot(1,xlim=c(0,x),ylim=c(0,y),type='n')
  }
  p <- cbind((1:length(jogo) -1) %% y + 1,floor((1:length(jogo) -1) / y + 1))
  o <- which(jogo == 1)
  rect(0, 0, x, x, col = 'white')
  abline(v = 1:x)
  abline(h = 1:y)
  rect(p[o,2]-1,p[o,1]-1,p[o,2],p[o,1],col=1)   
 }
 mouse <- function(b,y,x,y1=ncol(map),x1=nrow(map)) { 
  if (length(b)) {
   x <- ceiling(x1*x)
   y <- ceiling(y1*y)
   if ((b == 2) && (any(c(x,y) != ultimo))) { print(c(x,y)) ; ultimo <<- c(x,y) ; return() }
   if (any(c(x,y) != ultimo)) {
    map[x,y] <<- xor(map[x,y],1)
    plot.jv(map)
    ultimo <<- c(x,y)
    return()
   }
  }
 }
 tecla <- function(a) { return(a) }
 joga <- function(jogo) {
  y <- nrow(jogo)  
  x <- ncol(jogo)
  jogon <- matrix(0,ncol=x,nrow=y)
  j1 <- jogo[,c(2:x,1)] + jogo[,c(x,2:x-1)] + jogo[c(2:y,1),] + jogo[c(y,2:y-1),] + jogo[c(2:y,1),c(2:x,1)] + jogo[c(y,2:y-1),c(2:x,1)] + jogo[c(2:y,1),c(x,2:x-1)] + jogo[c(y,2:y-1),c(x,2:x-1)]
  jogon[((j1 == 3) | (j1 + jogo == 3))] <- 1
  return(jogon)
 }
 ultimo <- rep(0,2)
 if (R == T) { map <- matrix(sample(c(0,0,0,1),x*y,rep=T),ncol=x,nrow=y) }
 else { 
  map <- matrix(0,ncol=x,nrow=y)
  plot.jv(map)
  getGraphicsEvent('Clique num quadrado para altera-lo. Quando terminar, pressione espaço.',onMouseDown=mouse,onKeybd=tecla,onMouseMove=mouse,onMouseUp=function(...) { ultimo <<- rep(0,2) ; return() } )
 }
 plot.jv(map)
 while (1 < 2) {
  map <- joga(map)
  plot.jv(map)
  Sys.sleep(.1)
 }
}
jv(101,101,F)
