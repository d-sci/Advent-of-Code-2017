#Day 16
setwd("C:/Users/David.simons/Documents/advent of code")

dance <- unlist(strsplit(readLines("day16.txt"), ","))
moves <- substr(dance, 1, 1)
args <- strsplit(substr(dance, 2, nchar(dance)), "/")

#part 1 ----
danceRound <- function(progs){
  N <- length(progs)
  for (i in seq_along(moves)) {
    if (moves[i] == "s") {
      n <- as.integer(args[[i]])
      progs <- progs[c((N-n+1):N, 1:(N-n))]
    } else if (moves[i] == "x") {
      x <- as.integer(args[[i]]) + 1
      progs[x] <- progs[rev(x)]
    } else if (moves[i] == "p") {
      x <- args[[i]]
      progs[match(x, progs)] <- rev(x)
    }
  }
  progs
}

progs <- danceRound(letters[1:16])
print(do.call(paste0, as.list(progs)))


#part 2 ----

#try it but see if there's a loop anywhere
progs <- letters[1:16]
history <- do.call(paste0, as.list(progs))
for (round in seq(1e9)) {
  progs <- danceRound(progs)
  arrangement <- do.call(paste0, as.list(progs))
  if (arrangement %in% history) {
    print(sprintf("Repeat found after moves %d and %d", match(arrangement, history) - 1, round))
    break
  } else history <- c(history, arrangement)
}

#therefore only need to go dance 1e9 %% 60 times (and already recorded in our history)
print(history[1e9%%60 + 1])
