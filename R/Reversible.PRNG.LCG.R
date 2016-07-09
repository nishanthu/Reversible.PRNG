
set.seed.lcg <- function(seed=0, a=1664525, b=1013904223, m=2^32) {
  .state <<- seed
  .a <<- a
  .b <<- b
  .m <<- m
}

runif.lcg.next <- function(n) {
  if(!exists('.state')) set.seed.lcg()

  rns = numeric(n)
  for(i in seq_len(n)) {
    .state <<- (.a*.state + .b) %% .m
    rns[i] = .state/.m
  }

  rns
}

runif.lcg.prev <- function(n) {
  if(!exists('.state')) set.seed.lcg()

  rns = numeric(n)
  for (i in seq_len(n)) {
    x = (.m*(0:.a) - .b + .state)/.a
    x = x[which.max(x >= 0 & x%%1 == 0)]
    .state <<- x
    rns[i] = .state/.m
  }

  rns
}

rint.lcg.next <- function(n) {
  if(!exists('.state')) set.seed.lcg()

  rns = integer(n)
  for(i in seq_len(n)) {
    .state <<- (.a*.state + .b) %% .m
    rns[i] = .state
  }

  rns
}

rint.lcg.prev <- function(n) {
  if(!exists('.state')) set.seed.lcg()

  rns = integer(n)
  for (i in seq_len(n)) {
    x = (.m*(0:.a) - .b + .state)/.a
    x = x[which.max(x >= 0 & x%%1 == 0)]
    .state <<- x
    rns[i] = .state
  }

  rns
}
