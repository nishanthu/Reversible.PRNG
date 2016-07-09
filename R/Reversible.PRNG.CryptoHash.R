
runif.hash.next <- function(n) {
  if(!exists('.rprng.hash.state')) set.seed.hash()

  res = numeric(n)
  for(i in seq_len(n)) {
    .rprng.hash.state <<- .rprng.hash.state + 1
    res[i] = (strtoi(paste0(tail(unlist(strsplit(digest(.rprng.hash.state, "xxhash32"),'')),.num.digits), collapse = ''), 16L) / .max.num)
  }
  res
}

runif.hash.prev <- function() {
  if(!exists('.rprng.hash.state')) set.seed.hash()

  res = numeric(n)
  for(i in seq_len(n)) {
    .rprng.hash.state <<- .rprng.hash.state - 1
    res[i] = (strtoi(paste0(tail(unlist(strsplit(digest(.rprng.hash.state, "xxhash32"),'')),.num.digits), collapse = ''), 16L) / .max.num)
  }
  res
}

set.seed.hash <- function(seed=0) {
  .rprng.hash.state <<- seed
  .num.digits  <<- 5
  .max.num     <<- strtoi(paste0(c("0x",rep("f",.num.digits)), collapse = ''))
}
