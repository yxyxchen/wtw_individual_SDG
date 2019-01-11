r1 = microbenchmark({
a = vector(length = 100)
for(i in 1 : 100){
a[i] = mean(runif(10) * i)
}
}, times = 10L)
# 3.099471

r2 = microbenchmark({
  a = sapply(1 : 100, function(x) runif(10) * x)
}, times = 10L)
# 0.517