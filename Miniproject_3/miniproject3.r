
# 1.a
#mean square error funtion 
mse <- function(n, theta){ 
    calc_estimates <- function(n, theta) {
    x = runif(n, min = 0, max = theta)
    mle = max(x)
    memo = 2 * mean(x)
    return(c(mle, memo))
  }
  estimates = replicate(1000, calc_estimates(n, theta))
  
  return(rowMeans((estimates - theta) ^ 2))
}

# 1.b
n = c(1, 2, 3, 5, 10, 30)
thetas = c(1, 5, 50, 100)

n_len = length(n)
t_len = length(thetas)

mle_matrix = matrix(nrow = n_len, ncol = t_len)
memo_matrix = matrix(nrow = n_len, ncol = t_len)
#adding values of mse of two esimation methods
for (i in 1:n_len) {
  for (j in 1:t_len){
    res = mse(n[i], thetas[j])
    mle_matrix[i, j] = res[1]
    memo_matrix[i, j] = res[2]
  }
}

mle_matrix
memo_matrix

# 1.c
#plotting mse vs n for all theta values
par(mfrow=c(2, 2))
for (i in 1:t_len){
  plot(n, mle_matrix[,i], lty="solid",type="l", main=paste("theta = ",thetas[i]), 
       ylab="MSE", ylim=c(0, max(memo_matrix[,i], mle_matrix[,i])))
  lines(n, memo_matrix[,i],lty="dotted", col='blue')
}


# 2
# 2.b
#maximum likelihood for given values
X = c(21.72, 14.65, 50.42, 28.78, 11.23)
theta = length(X) / sum(log(X))
theta

# 2.c
f = function(x, theta) {
  return(theta/x^(theta + 1))
}
neg.log.fun = function(data, par) {
  return(-sum(log(f(data, par))))
}
ml.est = optim(data = X, par = 1, fn = neg.log.fun, method = "BFGS", hessian = TRUE)
ml.est

# 2.d 
sde.mle = sqrt(diag(solve(ml.est$hessian)))
upper = ml.est$par + qnorm(0.975) * sde.mle
lower = ml.est$par - qnorm(0.975) * sde.mle

sde.mle
upper
lower


