#Day 25
setwd("C:/Users/David.simons/Documents/advent of code")

steps <- 12172063
tape <- vector("logical", steps*2) #not "infinite" but more than enough
i <- steps #start in middle
state <- 1 #A=1, F=6

for (step in seq(steps)) {
  if (tape[i]) {
    switch(state,
           {tape[i] <- F; i <- i - 1; state <- 3},
           {i <- i - 1; state <- 4},
           {tape[i] <- F; i <- i + 1},
           {tape[i] <- F; i <- i + 1; state <- 5},
           {i <- i - 1; state <- 6},
           {i <- i + 1; state <- 1}
    )
  } else {
    switch(state,
           {tape[i] <- T; i <- i + 1; state <- 2},
           {tape[i] <- T; i <- i - 1; state <- 1},
           {tape[i] <- T; i <- i + 1; state <- 4},
           {i <- i - 1; state <- 2},
           {tape[i] <- T; i <- i + 1; state <- 3},
           {tape[i] <- T; i <- i - 1; state <- 5}
    )
  }
}

print(sum(tape))