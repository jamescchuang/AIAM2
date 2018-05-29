# 22/43
#####
# 執行前需先安裝 corpcor 套件
# corpcor: Efficient Estimation of Covariance and (Partial) Correlation
install.packages("corpcor")
#####

library("corpcor")

n <- 6 
p <- 10 
set.seed(123456)
sigma <- matrix(rnorm(p * p), ncol = p)
sigma <- crossprod(sigma) + diag(rep(0.1, p))
#####
# 需在 mvrnorm 前加入 namespace 指定 MASS package
# 或是先載入 MASS
# library("MASS")
# x <- mvrnorm(n, mu=rep(0, p), Sigma=sigma)
#####
x <- MASS::mvrnorm(n, mu=rep(0, p), Sigma=sigma)
s1 <- cov(x)
s2 <- cov.shrink(x)
par(mfrow=c(1,3))
image(t(sigma)[,p:1], main="true cov", xaxt="n", yaxt="n")
image(t(s1)[,p:1], main="empirical cov", xaxt="n", yaxt="n")
image(t(s2)[,p:1], main="shrinkage cov", xaxt="n", yaxt="n")

dev.off()

# 23/43

is.positive.definite(sigma)
is.positive.definite(s1)
is.positive.definite(s2)

rc <- rbind(
  data.frame(rank.condition(sigma)),
  data.frame(rank.condition(s1)),
  data.frame(rank.condition(s2)))
rownames(rc) <- c("true", "empirical", "shrinkage")
rc

e0 <- eigen(sigma, symmetric = TRUE)$values
e1 <- eigen(s1, symmetric = TRUE)$values
e2 <- eigen(s2, symmetric = TRUE)$values

matplot(data.frame(e0, e1, e2), type = "l", ylab="eigenvalues", lwd=2)
legend("top", legend=c("true", "empirical", "shrinkage"), lwd=2, lty=1:3, col=1:3)

# 31/43
curve(pnorm(x), -3, 3)
arrows(-1, 0, -1, pnorm(-1), col="red")
arrows(-1, pnorm(-1), -3, pnorm(-1), col="green")
pnorm(-1)


# 32/43
qnorm(0.025)
qnorm(0.5)
qnorm(0.975)


# 36/43
dnorm(0)
pnorm(-1)
qnorm(0.975)

dnorm(10, 10, 2) 
pnorm(1.96, 10, 2)
qnorm(0.975, 10, 2)
rnorm(5, 10, 2)
pnorm(15, 10, 2) - pnorm(8, 10, 2)  # P(8<=X<=15)

par(mfrow=c(1,4))
curve(dnorm, -3, 3, xlab="z", ylab="Probability density", main="Density")
curve(pnorm, -3, 3, xlab="z", ylab="Probability", main="Probability")
curve(qnorm, 0, 1, xlab="p", ylab="Quantile(z)", main="Quantiles")
hist(rnorm(1000), xlab="z", ylab="frequency", main="Random numbers")

dev.off()

# 37/43
n <- 50 
p <- 0.2 
mu <- n * p
sigma <- sqrt(n * p * (1 - p))
x <- 0:n
plot(x, dbinom(x, n, p), type = 'h', lwd = 2, xlab = "x", ylab = "P(X=x)",
     main = paste0("B(",n,",",p,")"))
z <- seq(0, n, 0.1)
lines(z, dnorm(z, mu, sigma), col = "red", lwd = 2)
abline(h = 0, lwd = 2, col = "grey")

dev.off()

# 43/43
girl.born <- function(n, show.id = F){
  
  girl.count <- 0
  for (i in 1:n) {
    if (show.id) cat(i,": ")
    child.count <- 0
    repeat {
      rn <- sample(0:99, 1, replace=T) # random number
      if (show.id) cat(paste0("(", rn, ")"))
      is.girl <- ifelse(rn <= 48, TRUE, FALSE)
      child.count <- child.count + 1
      if (is.girl){
        girl.count <- girl.count + 1
        if (show.id) cat("女+")
        break
      } else if (child.count == 3) {
        if (show.id) cat("男")        
        break
      } else{
        if (show.id) cat("男")        
      }
    }
    if (show.id) cat("\n")
  }
  p <- girl.count / n
  p
  
}

girl.p <- 0.49 + 0.51*0.49 + 0.51^2*0.49
girl.p
girl.born(n=10, show.id = T)
girl.born(n=10000)
