RNGkind("Mersenne-Twister")
set.seed(1)
.Random.seed
parallel::nextRNGStream(.Random.seed)


RNGkind("L'Ecuyer-CMRG")
set.seed(1)
.Random.seed
runif(n = 1, min = 0, max = 1)
parallel::nextRNGStream(.Random.seed)
runif(n = 1, min = 0, max = 1)

