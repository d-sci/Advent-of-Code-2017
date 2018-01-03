#Day 2
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

#part 1 ----
input <- fread("day2.txt", header=F)
input[, rowDiff := max(.SD) - min(.SD), by = 1:nrow(input)]
print(input[, sum(rowDiff)])


#part2 ----
input <- fread("day2.txt", header=F)
getRowNum <- function(row) {
  nums <- as.vector(t(row))
  for (i in seq_along(nums)) {
    for (j in seq_along(nums)) {
      if (i != j && nums[i] %% nums[j] == 0) return(nums[i] / nums[j])
    }
  }
}
input[, rowNum := getRowNum(.SD), by = 1:nrow(input)]
print(input[, sum(rowNum)])
