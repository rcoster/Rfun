# Feito por: Rodrigo Coster - 2009
# www.dadosaleatorios.com.br

jv <- function() {
 tecla <- function(a,b) { return() }
 clica <- function(a,b,c) { 
  checa <- function() {
   jo <- cbind(matrix(1:9,ncol=3),matrix(1:9,ncol=3,byrow=T),c(1,5,9),c(7,5,3))
   for (j in 1:8) {
    if (sum(jogo[jo[,j]]) %in% c(3,12)) {
     m <- jo[c(1,3),j]
     x <- as.integer((m-1)/3)+.5
     y <- 2.5 - ((m-1) %% 3)
     lines(x,y,col='red',lwd=4)
     return(TRUE)
    }
   }
   return(FALSE)
  }
  m <- sum((as.integer(c(b,1-c)*3)+c(0,1))*c(3,1))
  if (jogo[m] == 0) {
   jogo[m] <<- 4
   x <- as.integer((m-1)/3)+.5
   y <- 2.5 - ((m-1) %% 3)
   points(x,y,pch='X')
   if ((any(jogo == 0)) && (checa() == FALSE)) { 
    m <- joga(jogo) 
    jogo[m] <<- 1 
    x <- as.integer((m-1)/3)+.5
    y <- 2.5 - ((m-1) %% 3)
    points(x,y,pch='O')
    checa()
   }
   if ((checa() == TRUE) || (all(jogo != 0))) { return('ok') }
  }
 }
 joga <- function(jogo) {
  jo <- cbind(matrix(1:9,ncol=3),matrix(1:9,ncol=3,byrow=T),c(1,5,9),c(7,5,3))
  pontos <- rbind(c(8,2),c(4,1),c(0,0))
  marca <- c(rep(3,4),1,rep(3,4))/2
  for (i in 1:3) {
   for (j in 1:8) {
    if (sum(jogo[jo[,j]]) %in% pontos[i,]) {
     marca[jo[,j]] <- marca[jo[,j]] * (8 - 2 * i) * ifelse(sum(jogo[jo[,j]]) == 8,10,1) * ifelse(sum(jogo[jo[,j]]) == 2,20,1)
    }
   } 
  }
  marca[jogo != 0] <- 0
  out <- which.max(marca)
  if (sum(marca == max(marca)) > 1) { out <- sample((1:9)[marca == max(marca)],1) }
  return(out)
 }
 par(mar=c(0,0,0,0),cex=5)
 plot(1,xlim=c(0,3),ylim=c(0,3),type='n',xlab='',ylab='')
 lines(c(2,2),c(0,3))
 lines(c(1,1),c(0,3))
 lines(c(0,3),c(1,1))
 lines(c(0,3),c(2,2))
 jogo <- matrix(0,3,3)
 if (runif(1) < .5) {
#  m <- sample(c(1,3,7,9),1)
  m <- joga(jogo)
  jogo[m] <- 1 
  x <- as.integer((m-1)/3)+.5
  y <- 2.5 - ((m-1) %% 3)
  points(x,y,pch='O')
 }
 a <- getGraphicsEvent("Tic-Tac-Toe :)", onMouseUp = clica, onKeybd = tecla) 
 return(invisible())
}

jv()
