# Feito por: Rodrigo Coster - 2011
# www.dadosaleatorios.com.br

##
# jogo(l), onde l é 1 ou 2 (dificuldade)
# jogo(2)
##

jogo <- function(lvl=1) {
 ## Rotinas de apoio
 # Checa se o movimento é valido
 pode <- function(tabul,x,y,jo) {
  p <- FALSE
  for (i in 1:3-2) {
   for (j in 1:3-2) {
    if ((x+i < 11) && (x+i > 0) && (y+j < 11) && (y+j > 0)) {
     if (tabul[x+i,y+j] %in% 1:2) { p <- TRUE }
    }
   }
  }
  if (tabul[x,y] %in% 1:2) { p <- FALSE }
  if (p == T) {
   p <- sum(move(x,y,jo) == jo) - 1 > sum(tabul == jo)
  }
  return(p)
 }
 # IA
 joga <- function() {
  mv <- 1
  r <- 0
  if (lvl == 1) {
   for (i in 1:10) {
    for (j in 1:10) {
     if (pode(m,i,j,1)) {
      t <- move(i,j,1)
      if (sum(t == 1) > r) {
       mv <- c(i,j)
       r <- sum(t == 1)
      }
     }
    }
   }
  }
  if (lvl == 2) {
   for (i in 1:10) {
    for (j in 1:10) {
     if (pode(m,i,j,1)) {
      r1 <- 0
      t <- move(i,j,1,m)
      for (k in 1:10) {
       for (l in 1:10) {
        if (pode(t,k,l,2)) {
         n <- move(k,l,2,t)
         if (sum(n == 2) > r1) {
          r1 <- sum(n == 2)
         }
        }
       }
      }
      if ((sum(t == 1) - r1 > r) || (length(mv) == 1)) {
       mv <- c(i,j)
       r <- sum(t == 1) - r1
      }
     }
    }
   }
  }
  m <<- move(mv[1],mv[2],1)
 }
 # Evento do mouse
 clica <- function(b, x, y) {
  x <- as.integer(x*10)+1
  y <- as.integer((1-y)*10+1)
  f <- 0
  if (pode(m,y,x,2) == TRUE) { 
   m <<- move(y,x,2) 
   f <- plot(m)
   joga <- 1
   while (((poss(1)) && (joga == 1)) || ((!poss(2)) && (poss(1)))) { joga() ; f <- plot(m) ; joga <- 2 }
  }
  if ((f == 1) || (!poss(2))) { return('dtm') }
  return()
 }
 # Plota
 plot.tabul <- function(tabul) {
  plot(0:10,type='n')
  for (i in 1:10) {
   for (j in 1:10) {
    rect(j,10-i,j+1,11-i,col=c('white','blue','red')[tabul[i,j] + 1])
   }
  }
  if (!any(tabul == 0)) { return(1) }
 return(0)
 }
 # Movimento
 move <- function(x,y,p,tabul=m) {
  out <- tabul
  pt2 <- matrix(c(x,y),ncol=2)
  for (i in c(1,0,-1)) { 
   for (j in c(1,0,-1)) {
    pt <- c(x,y)
    pt1 <- vector()
    cont <- 1
    while ((max(pt) <= 10) && (min(pt) >= 1) && (cont == 1)) {
     pt <- pt + c(i,j)
     pt1 <- rbind(pt1,pt)
     if (((i == 0) && (j == 0)) || ((any(pt > 10)) || (any(pt < 1)))) { cont <- 2 } 
     else if (tabul[pt[1],pt[2]] == p) { pt2 <- rbind(pt2,pt1) ; cont <- 2 }
     else if (tabul[pt[1],pt[2]] == 0) { cont <- 2 }
    }
   } 
  }
  for (k in 1:nrow(pt2)) { 
   out[pt2[k,1],pt2[k,2]] <- p
  }
  return(out)
 }
 # Confere se há jogadas possiveis
 poss <- function(jo,tabul=m) {
  for (i in 1:10) {
   for (j in 1:10) {
    if (pode(tabul,i,j,jo) == TRUE) { return(T) }
   }
  }
  return(F)
 }
 ## Jogo
 par(mar=rep(0,4),xaxs='i',yaxs='i')
 m <- matrix(0,10,10)
 class(m) <- 'tabul'
 for (i in 5:6) {
  for (j in 5:6) {
   m[i,j] <- ((i + j -1) %% 2 + 1)
  }
 }
 plot(m)
 if (runif(1) > .5) { Sys.sleep(.5) ; joga() ; plot(m) }
 getGraphicsEvent('Reversi',onMouseDown=clica)
 cat('Fim de jogo! Resultado:\n Azul (PC):',sum(m == 1),'\n Vermelho (você):',sum(m == 2),'\n') ; if (any(m == 0)) { cat(' Branco:',sum(m==0),'\n') } ;
 return(invisible())
}
jogo(2)
