# Function to get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to return length of unique

lenun <- function(x) {
  length(unique(x))
}