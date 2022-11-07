setwd("/home/antonsaukkonen/Desktop/PTA")

data <- read.table("tobacco.txt", header=T, row.names=1)
colnames(data)

#(a)
lin_model <- lm(ILL ~ CONSUMPTION, data=data)
#(b)
lin_model$coefficients
lin_model$fitted.values
summary(lin_model)
#(c) seen from summary report
#(d)
0.009081 < 0.01
#(e) + (f)
plot(ILL ~ CONSUMPTION, data=data)
abline(lin_model, col="red")
#(g)
#(h)
confint(lin_model, level=.95)
confint(lin_model, level=.99)
#(i)

k <- 2000
alpha <- 0.05
X <- cbind(rep(1, length(data[, "CONSUMPTION"])), data[, "CONSUMPTION"])

# Number of variables and number of observations.
m <- ncol(X)
n <- nrow(X)

bmatrix <- matrix(NA, nrow = k, ncol = m)
y <- data[, "ILL"]

# bootstrap
for (i in 1:(k-1)) {
  ind <- sample(1:n, n, replace = TRUE)
  xtmp <- X[ind, ]
  ytmp <- y[ind]
  
  btmp <- solve(t(xtmp) %*% xtmp) %*% t(xtmp) %*% ytmp
  bmatrix[i, ] <- t(btmp)
}

# original estimate
b <- solve(t(X) %*% X) %*% t(X) %*% y
bmatrix[k, ] <- t(b)

qintercept <- quantile(bmatrix[, 1], probs = c(0.025, 0.975))
qintercept
qconsumption <- quantile(bmatrix[, 2], probs = c(0.025, 0.975))
qconsumption

#(j) non-parametric

# Questions:

# 1) Does F statistic and PR(>|t|) that we see in the model's summary actually
# reliable only when residuals are normally distributed? 

# 2) what is the different in notation \beta and b ?

# 3) "the prediction is done under uncertanties such as (lec. slide 54)

#     - the regression parameters have been estimated
#     - the residuals explain part of the variation in y

# what is the meaning?

# 4) What multicollinearity actually means?