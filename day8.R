#Day 8
setwd("C:/Users/David.simons/Documents/advent of code")

input <- lapply(strsplit(readLines("day8.txt"), " "), setNames, c("reg","updown","amount","if","condReg","condFun","condVal"))
regNames <- unique(unlist(lapply(input, `[`, c("reg","condReg"))))
regs <- vector("integer", length(regNames))
names(regs) <- regNames
runningMax <- 0 #for part 2

for (line in input) {
  cond <- do.call(line["condFun"], list(regs[[line["condReg"]]], as.integer(line["condVal"])))
  if (cond) 
    regs[[line["reg"]]] <- regs[[line["reg"]]] + as.numeric(line["amount"]) * ifelse(line["updown"]=="inc", 1, -1)
  if (regs[[line["reg"]]] > runningMax) #for part 2
    runningMax <- regs[[line["reg"]]] 
}

#part 1
print(max(regs))

#part 2
print(runningMax)