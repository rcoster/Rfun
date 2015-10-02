# Feito por: Rodrigo Coster - 2011
# www.dadosaleatorios.com.br

###
# cm(c(casas horizontais,casa verticais,minas)) ou cm(l), onde l pode 1, 2 ou 3
# cm(c(15,15,20))
###
cm <- function(l=2) {
 joga <- function(b, x, y) {
 if ((!exists('m') || !exists('jo') || !exists('c')) || (all(jo == c))) { return(' ') }
  p <- (ceiling((x)*l[1])-1)*l[2]+ceiling((y)*l[2])
  n <- pi
  a <- 0
  if (b == 0) {
   while (length(p) > 0) {
    a <- a+1
    pa <- p[1]
    p <- p[-1]
    if ((pa > 0) && (pa <= length(m))) {
     if ((jo[pa] == 10) || ((jo[pa] == 11) && (a == 1))) { 
      n <- c[pa]
      if (n == 0) { 
       pt <- c(ceiling(pa/l[2]),(pa -1) %% l[2] +1) + cbind(c(-1,-1),c(-1,0),c(-1,1),c(0,-1),c(0,1),c(1,-1),c(1,0),c(1,1))
       v <- !(pt[1,] < 1 | pt[1,] > l[1] | pt[2,] < 1 | pt[2,] > l[2])
       pos <- 1:8
       if (!any(v)) { pos <- -((1:8)[v]) }
       pt <- pt[,v]
       p <- c(p,(pt[1,] -1) * l[2] + pt[2,])
      }
      if (n == 9) {
       jo <<- c
      }
      jo[pa] <<- n
     }
    }
   }
  }
  else if ((b == 2) && (jo[p] > 9)) {
   jo[p] <<- ifelse(jo[p] == 10,11,10)
  }
  p1 <- which(jo == 10,arr.ind=T)
  rect(p1[,2]-1,p1[,1]-1,p1[,2],p1[,1],col='white') 
  p2 <- which(jo == 11 | jo == 9,arr.ind=T)
  rect(p2[,2]-1,p2[,1]-1,p2[,2],p2[,1],col='red') 
  p3 <- which(jo < 9,arr.ind=T)
  rect(p3[,2]-1,p3[,1]-1,p3[,2],p3[,1],col='gray') 
  p4 <- which(jo < 9 & jo > 0,arr.ind=T)
  text(p4[,2]-.5,p4[,1]-.5,jo[which(jo < 9 & jo > 0)],col=rainbow(8)[jo[which(jo < 9 & jo > 0)]],cex=2)
  if (n == 9) { cat('Voce perdeu! :(\n') ; jo <<- c ; return() }
  jot <- jo
  jot[jo == 11] <- 9
  if (all(jot == c)) { cat('Voce ganhou! \\o/\n') ; jo <<- c }
  return()
 }
 l <- floor(l)
 if (length(l) == 1) {
  if (l == 1) { l <- c(9,9,10) }
  else if (l == 2) { l <- c(16,16,40) }
  else if (l == 3) { l <- c(30,16,90) }
  else { stop('l deve ser um vetor de comprimento 3, ou igual à 1, 2 ou 3') }
 }
 if (l[1] * l[2] < l[3]) { stop('Problema no tamanho do tabuleiro: Muito pequeno para o número de minas desejado') }
 par(mar=rep(0,4),xaxs='i',yaxs='i')
 plot(pi,pi,xlim=c(0,l[1]),ylim=c(0,l[2]),type='n')
 text(l[1]/2,l[2]/2,'Pressione espaço sempre que quiser começar um jogo novo, \nou clique no tabuleiro após perder para recomeçar.')
 while (getGraphicsEvent("Campo Minado",onMouseDown = joga,onKeybd = function(a) return(a)) == " ") {
  plot(pi,pi,xlim=c(0,l[1]),ylim=c(0,l[2]),type='n')
  abline(h = 2:l[2] -1)
  abline(v = 2:l[1] -1)
  m <- matrix(0,l[2],l[1])
  jo <- matrix(10,l[2],l[1])
  m[sample(1:(l[1]*l[2]),l[3])] <- 1
  c <- rbind(m[-1,],0) + rbind(0,m[-l[2],]) + cbind(m[,-1],0) + cbind(0,m[,-l[1]]) + cbind(rbind(m[-1,],0)[,-1],0) + cbind(0,rbind(m[-1,],0)[,-l[1]]) + cbind(rbind(0,m[-l[2],])[,-1],0) + cbind(0,rbind(0,m[-l[2],])[,-l[1]])
  c[m == 1] <- 9
 }
}
cm(2)
