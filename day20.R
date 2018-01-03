#Day 20
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

#part 1 ----
input <- strsplit(readLines("day20.txt"), ", ")
particles <- rbindlist(lapply(input, function(pva){
  pva <- as.list(as.numeric(unlist(strsplit(substr(pva, 4, nchar(pva) - 1), ","))))
  names(pva) <- paste0(rep(c("p","v","a"), each=3), c("x","y","z"))
  pva
}))
particles[, id := 0:999]

for (t in 1:1000) { #arbitrary....and likely not necessary if just look at acceleration
  particles[, `:=`(
    vx = vx + ax,
    vy = vy + ay,
    vz = vz + az)]
  particles[, `:=`(
    px = px + vx,
    py = py + vy,
    pz = pz + vz)]
}
particles[, dist := abs(px) + abs(py) + abs(pz)]

#see who is closest now after 1000 steps
print(particles[dist == min(dist), id])
#also confirm this is guy with smallest overall acceleration
print(particles[, a := abs(ax)+abs(ay)+abs(az)][a==min(a),id])


#part 2 ----
input <- strsplit(readLines("day20.txt"), ", ")
particles <- rbindlist(lapply(input, function(pva){
  pva <- as.list(as.numeric(unlist(strsplit(substr(pva, 4, nchar(pva) - 1), ","))))
  names(pva) <- paste0(rep(c("p","v","a"), each=3), c("x","y","z"))
  pva
}))

for (t in 1:100) { #also arbitrary....
  collisions <- particles[, .N, by = .(px,py,pz)][N > 1]
  if (nrow(collisions) > 0) particles <- particles[!paste(px,py,pz) %in% collisions[, paste(px,py,pz)]]
  particles[, `:=`(
    vx = vx + ax,
    vy = vy + ay,
    vz = vz + az)]
  particles[, `:=`(
    px = px + vx,
    py = py + vy,
    pz = pz + vz)]
}

print(nrow(particles))
