#Day 15
setwd("C:/Users/David.simons/Documents/advent of code")

genA <- vector("integer", 40e6)
genB <- vector("integer", 40e6)

genA[1] <- (618*16807) %% 2147483647
genB[1] <- (814*48271) %% 2147483647

for (i in 2:40e6) { #simple, but not terribly fast in R
  genA[i] <- (genA[i-1]*16807) %% 2147483647
  genB[i] <- (genB[i-1]*48271) %% 2147483647
}

#part 1
print(sum(genA %% 65536 == genB %% 65536))

#part 2
pickyA <- genA[genA %% 4 == 0][1:5e6]
pickyB <- genB[genB %% 8 == 0][1:5e6]
print(sum(pickyA %% 65536 == pickyB %% 65536))
