#Day 18
setwd("C:/Users/David.simons/Documents/advent of code")

input <- readLines("day18.txt")
cmd <- substr(input, 1, 3)
args <- strsplit(substr(input, 5, nchar(input)), " ")

#part 1 ----
regs <- vector("integer", 16)
names(regs) <- letters[1:16]
sounds <- integer()

clean <- function(x){
  suppressWarnings(ifelse(!is.na(as.numeric(x)), as.numeric(x), regs[[x]]))
}

i <- 1
while (i >= 1 && i <= length(cmd)) {
  arg <- args[[i]]
  switch(cmd[i], 
         snd = sounds <- c(sounds, clean(arg)),
         set = regs[[arg[1]]] <- clean(arg[2]),
         add = regs[[arg[1]]] <- regs[[arg[1]]] + clean(arg[2]),
         mul = regs[[arg[1]]] <- regs[[arg[1]]] * clean(arg[2]),
         mod = regs[[arg[1]]] <- regs[[arg[1]]] %% clean(arg[2]),
         rcv = if (clean(arg[1]) != 0) {print(sounds[length(sounds)]); break},
         jgz = if (clean(arg[1]) > 0) i <-  i + clean(arg[2]) - 1 #minus one to counter typical plus 1
         )
  i <- i + 1
}


#part 2 ----
regs0 <- vector("integer", 16)
regs1 <- c(rep(0L, 15), 1L)
names(regs0) <- names(regs1) <- letters[1:16]

q0 <- q1 <- integer()
deadlock <- died0 <- died1 <- waiting0 <- waiting1 <- F
i <- j <- 1
counter <- 0

clean <- function(x, r){
  regs <- if (r==0) regs0 else regs1
  suppressWarnings(ifelse(!is.na(as.numeric(x)), as.numeric(x), regs[[x]]))
}

while (!deadlock) {
  #try program 0 (on line "i")
  while (!died0 && (!waiting0 || length(q0) > 0)) {
    if (length(q0) > 0) waiting0 <- F
    arg <- args[[i]]
    switch(cmd[i], 
           snd = q1 <- c(q1, clean(arg, 0)),
           set = regs0[[arg[1]]] <- clean(arg[2], 0),
           add = regs0[[arg[1]]] <- regs0[[arg[1]]] + clean(arg[2], 0),
           mul = regs0[[arg[1]]] <- regs0[[arg[1]]] * clean(arg[2], 0),
           mod = regs0[[arg[1]]] <- regs0[[arg[1]]] %% clean(arg[2], 0),
           rcv = if (length(q0) > 0) {regs0[[arg[1]]] <- q0[1]; q0 <- q0[-1]} else waiting0 <- T,
           jgz = if (clean(arg[1], 0) > 0) i <-  i + clean(arg[2], 0) - 1 #minus one to counter typical plus 1
    )
    if (!waiting0) i <- i + 1 #if waiting stay here to try again later
    if (i > length(cmd) || i < 0) died0 <- T
  }
  
  #try program 1 (on line "j")
  while (!died1 && (!waiting1 || length(q1) > 0)) {
    if (length(q1) > 0) waiting1 <- F
    arg <- args[[j]]
    switch(cmd[j], 
           snd = {q0 <- c(q0, clean(arg, 1)); counter <- counter + 1},
           set = regs1[[arg[1]]] <- clean(arg[2], 1),
           add = regs1[[arg[1]]] <- regs1[[arg[1]]] + clean(arg[2], 1),
           mul = regs1[[arg[1]]] <- regs1[[arg[1]]] * clean(arg[2], 1),
           mod = regs1[[arg[1]]] <- regs1[[arg[1]]] %% clean(arg[2], 1),
           rcv = if (length(q1) > 0) {regs1[[arg[1]]] <- q1[1]; q1 <- q1[-1]} else waiting1 <- T,
           jgz = if (clean(arg[1], 1) > 0) j <-  j + clean(arg[2], 1) - 1 #minus one to counter typical plus 1
    )
    if (!waiting1) j <- j + 1 #if waiting stay here to try again later
    if (j > length(cmd) || j < 0) died1 <- T
  }
  
  #see if we can keep going
  deadlock <- (died0 || (waiting0 && length(q0)==0)) && (died1 || (waiting1 && length(q1)==0))
}

print(counter)


