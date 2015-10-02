# Criado por Rodrigo Coster - 2010
# www.dadosaleatorios.com.br

tank <- function(x) {
 desenha <- function(ang1,ang2,t) {
  ang1 <- ang1
  ang2 <- ang2
  par(mar=rep(0,4),xaxs='i',yaxs='i')
  plot(seq(0,t,by=0.01),mapa,type='n',ylim=c(0,t))
  xplot <- seq(0,t,by=0.01)
  polygon(c(xplot,t,0),c(mapa,t,t),col='slategray1')
  segments(p1,y1,sin(ang1/180*pi)*(t/30)+p1,cos(ang1/180*pi)*(t/30)+y1,lwd=2)
  segments(p2,y2,-sin(ang2/180*pi)*(t/30)+p2,cos(ang2/180*pi)*(t/30)+y2,lwd=2)
  points(p1,y1,pch=16,cex=3,col=4)
  points(p2,y2,pch=16,cex=3,col=2)
  polygon(c(xplot,t,0),c(mapa,0,0),col="green1")
  legend('topleft',paste(formatC(c('Angulo:','Força:','Vida:')),formatC(c(ang1,f1,v1),digits=2,width=7,format='f')),bty='n')
  legend('topright',paste(formatC(c('Angulo:','Força:','Vida:')),formatC(c(ang2,f2,v2),digits=2,width=7,format='f')),bty='n')
  text(t/2,t-.5,paste('Distância: ',round(t1,2)))
  if (v2 < 0) { text(10,10,'Você ganhou! \\o/',cex=3) ; stop('Fim de jogo') }
  if (v1 < 0) { text(10,10,'Você perdeu! <o>',cex=3) ; stop('Fim de jogo') }
 }
 atira <- function(p,ang,forc,t,...) {
  ang1 <- (ifelse(p == 2,ang-90,90-ang))/180*pi
  x <- ifelse(p == 1,p1,p2)
  a <- -(tan(ang1)/forc) ^2*(t1/20)
  b <- (tan(ang1)-2*a*x)
  c <- -(a*x^2 + b*x)
  x <- if (p == 1) seq(p1,t,.01) else seq(p2,.01,-.01)
  yt <- a * x^2 + b*x + c + ifelse(p == 1,y1,y2)
  y <- rep(NA,length(yt))
  if (sum(yt < mapa[x*100]) > 1) { y[1:(which(yt < mapa[x*100])[2])] <- yt[1:(which(yt < mapa[x*100])[2])] }
  else { y <- yt }
  check <- ifelse(p == 1,(p2-p1-.39)*100,(p2-p1-.39)*100)
  if (!is.na(y[check])) {
   pi <- if (p == 1) p2 else p1
   pi <- pi + c(.39,-.39)
   alv <- a * pi^2 + b*pi + c + ifelse(p == 1,y1-y2,y2-y1)
   if (any(alv < 0) && any(alv > 0)) {
    pi <- if (p == 1) p2 else p1
    d <- dnorm((a * pi^2 + b*pi + c + ifelse(p == 1,y1-y2,y2-y1))*10)
    if (p == 1) { v2 <<- v2-d*100 }
    if (p == 2) { v1 <<- v1-d*100 }
   }
  }
  lines(x,y,lty=2,...)
  return(invisible(cbind(x,y)))
 }
 tecla <- function(bot) {
  if (bot %in% c('Left','Right')) { ang1 <<- min(max(ang1 + ifelse(bot == 'Left',-.25,+.25),0),90) }
  if (bot %in% c('Up','Down')) { f1 <<- max(f1 + ifelse(bot == 'Up',.05,-.05),0) }
  if (bot == ' ') { atira(1,ang1,f1,t,col=1) ; Sys.sleep(1) ; desenha(ang1,ang2,t) ; joga() }
  desenha(ang1,ang2,t)
  return()
 }
 joga <- function() {
  if (n == 1) {
   m <- mapa[(p1*100+1):(p2*100-1)]
   a2 <- atan((m-y1)/seq(.01,p2-p1-.01,.01))
   a1 <- atan((m-y2)/seq(p2-p1-.01,.01,-.01))
   ang2 <<- max(min(70-max(a1,a2)*180/pi,70),20)
   f2 <<- (p2-p1)*t1/70
  }
  else {
   f2 <<- f2t
  }
  desenha(ang1,ang2,t)
  o <- atira(2,ang2,f2,t)
  fv <<- rbind(fv,c(f2,ifelse(is.na(o[(nrow(o) - p1*100),2]),1,2)))
  if ((any(fv[,2] == 1)) && (any(fv[,2] == 2))) {
   f2t <<- mean(c(max(fv[fv[,2] == 1,1]),min(fv[fv[,2] == 2,1])))
  }
  else {
   if (is.na(o[(nrow(o) - p1*100),2])) {
    f2t <<- f2*1.20*(t1/80+1)
    ln <<- n
   }
   else {
    f2t <<- f2*.8
   }
  }
  f3t <<- f2t
  Sys.sleep(1) 
  desenha(ang1,ang2,t)
  n <<- n+1
 }
 ln <<- vector()
 f2t <- 0
 fv <- vector()
 f1 <- 2
 f2 <- 2
 v1 <- 100
 v2 <- 100
 ang1 <- 45
 ang2 <- 45
 t1 <- round(runif(1,5,20),2)
 t <- 20
 p1 <- round(runif(1,.05,.2)*t,2)
 p2 <- round(runif(1,.80,.95)*t,2)
 mapa <- filter(cumsum(sample(c(.25,.1,0,-.1,-.25),t*100+7,rep=T,prob=c(1,5,20,5,1))),rep(1,7)/7)
 mapa <- mapa[!is.na(mapa)]
 mapa <- mapa - min(mapa) + .1
 mapa[mapa > 5] <- 5
 mapa[seq(p1*100-49,p1*100+49)] <- mean(mapa[seq(p1*100-49,p1*100+49)])
 mapa[seq(p2*100-49,p2*100+49)] <- mean(mapa[seq(p2*100-49,p2*100+49)])
 y1 <- mapa[p1*100]
 y2 <- mapa[p2*100]
 desenha(ang1,ang2,t)
 n <- 1
 getGraphicsEvent('Tanks',onKeybd=tecla)
 atira(1,ang1,2,t,col=1)
 Sys.sleep(.5)
 atira(2,ang2,2,t,col=4)
 Sys.sleep(.5)
}
tank()
