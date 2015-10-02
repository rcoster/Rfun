# Feito por: Rodrigo Coster - 2014
# www.dadosaleatorios.com.br

joga <- function(mapa = c(4, 4), base = 2, maximo = 11, sorteio = 1:2, multi = FALSE, cores = topo.colors) {
	plot.2048 <- function(mapa, base, maxpow, cores) {
		nCol <- ncol(mapa)
		nRow <- nrow(mapa)
		maxpow <- max(maxpow, mapa)
		cores <- cores(maxpow)
		par(mar = rep(0, 4), xaxs = 'i', yaxs = 'i')
		plot(0, type = 'n', xlim = c(0, nCol), ylim = c(0, nRow))
		abline(v = 0:nCol)
		abline(h = 0:nRow)
		for (i in 1:nCol) {
			for (j in 1:nRow) {
				if (mapa[j, i] > 0) {
					rect(i - 1, nRow - j, i, nRow - j + 1, density = NA, col = cores[mapa[j, i]])
					text(x = i - .5, y = nRow - j + .5, labels = base ^ mapa[j, i])
				}
			}
		}
	}

	move.2048 <- function(mapa, dir, sorteio, multi, bas) {
		sub.move <- function(x, dir, multi) {
			tam.inicial <- length(x)
			x <- x[x!=0]
			somou <- rep(FALSE, length(x))
			if (dir > 2) { x <- rev(x) }
			i <- length(x)
			# while (i <= length(x)) {
			while (i > 1) {
				if ((x[i-1] == x[i]) && (somou[i] == FALSE)) { 
					x[i-1] <- x[i-1] + 1
					pontos <<- pontos + (base ^ x[i])
					x <- x[-i]
					somou <- somou[-i]
					if (multi == FALSE) {
						somou[i-1] <- TRUE
					}
				}
				i <- i - 1
				# i <- i + 1
			}
			x <- c(x, rep(0, tam.inicial - length(x)))
			if (dir > 2) { x <- rev(x) }
			return(x)
		}
		if (dir %in% c(1, 3)) { mapa <- apply(mapa, 2, sub.move, dir = dir, multi = multi) } # Cima (1) e baixo (3)
		else { mapa <- t(apply(mapa, 1, sub.move, dir = dir, multi = multi)) } # Esquerda (2) e direita (4)
		return(mapa)
	}
	
	tecla <- function(x, map = mapa, mult = multi, bas = base, maxi = maximo, core = cores, pts = pontos) {
		if (x %in% c('Up', 'Left', 'Down', 'Right')) {
			map <- move.2048(map, if (x == 'Up') { 1 } else if (x == 'Left') { 2 } else if (x == 'Down') { 3 } else if (x == 'Right') { 4 }, multi = mult, bas = base)
			if (any(map != mapa)) {
				mapa <<- map
				if (any(mapa == 0)) {
					mapa[sample(which(mapa == 0), 1)] <<- sample(sorteio, 1)
					plot.2048(mapa, base = bas, maxpow = maxi, cores = core)
					Sys.sleep(.1)
				}
			}
		}
		if ((all(mapa[, -1] != mapa[, -ncol(mapa)])) & (all(mapa[-1, ] != mapa[-nrow(mapa), ])) & (all(mapa != 0))) {
			stop(sprintf('Fim! Pontuação: %d', pts))
		}
	}
	
	pontos <- 0
	mapa <- matrix(0, nrow = mapa[1], ncol = mapa[2])
	mapa[sample(1:(ncol(mapa) * nrow(mapa)), 1)] <- sample(sorteio, 1)
	plot.2048(mapa, base = base, maxpow = maximo, cores = cores)
	getGraphicsEvent(prompt = "Boa sorte!", onKeybd = tecla)
}

joga()
