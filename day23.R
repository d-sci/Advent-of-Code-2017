#Day 23
setwd("C:/Users/David.simons/Documents/advent of code")

#part 1 ----

#same as day18
input <- readLines("day23.txt")
cmd <- substr(input, 1, 3)
args <- strsplit(substr(input, 5, nchar(input)), " ")

regs <- vector("integer", 8)
names(regs) <- letters[1:8]

clean <- function(x){
  suppressWarnings(ifelse(!is.na(as.numeric(x)), as.numeric(x), regs[[x]]))
}

counter <- 0
i <- 1
while (i >= 1 && i <= length(cmd)) {
  arg <- args[[i]]
  switch(cmd[i], 
         set = regs[[arg[1]]] <- clean(arg[2]),
         sub = regs[[arg[1]]] <- regs[[arg[1]]] - clean(arg[2]),
         mul = regs[[arg[1]]] <- regs[[arg[1]]] * clean(arg[2]),
         jnz = if (clean(arg[1]) != 0) i <-  i + clean(arg[2]) - 1 #minus one to counter typical plus 1
  )
  if (cmd[i] == "mul") counter <- counter + 1
  i <- i + 1
}

print(counter)
#though apparentely can also skip and just say it's (b-2)^2 where b is intial value of b
print((81-2)**2)


# part 2 ----

#obviously not about simulating! 
#setting a to 1 means b and c start at 108100 and 125100
#b increases by 17 until it equals c and this allows us to end
#every time it does, if it's NOT a prime number, h gets incremented
#https://www.reddit.com/r/adventofcode/comments/7lms6p/2017_day_23_solutions/drnjwq7/

b <- 108100
c <- 125100
h <- 0

is.prime <- function(x) { #fast enough for this exercise
  for (n in c(2, seq(3, sqrt(x), 2))) {
    if (x %% n == 0) return(F)
  }
  return(T)
}

for (x in seq(b, c, 17)) {
  if (!is.prime(x)) h <- h + 1
}

print(h)