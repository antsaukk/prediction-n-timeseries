rm(list = ls())

library(car)

setwd("/home/antonsaukkonen/Desktop/PTA/com2")

# 2.3

data <- read.table("hald.txt", header=T, sep="\t")
colnames(data)
data <- data[, 1:ncol(data)-1]
colnames(data)
head(data)

permutation_test <- function(data) {
  k <- 2000
  alpha <- 0.05
  
  m <- ncol(data)
  n <- nrow(data)
  
  # Create matrix for collecting values of R^2 of permutated samples.
  rmat <- matrix(NA, nrow = k, ncol = m - 1)
  
  for (i in 1:k) {
    for (j in 1:(m-1)) { # Conduct significance test for every j = 0,1,2,3.
      # Permutate explatonary variable corresponding to B_(j-1).
      tmp <- data
      tmp[, j] <- sample(tmp[, j], n, replace = FALSE)
      
      # Calculate R^2.
      fit_tmp <- lm(HEAT ~ ., data = tmp)
      #rmat[i, (j - 1)] <- summary(fit_tmp)$r.squared
      rmat[i, j] <- summary(fit_tmp)$r.squared
    }
  }
  
  model <- lm(HEAT ~ ., data = data)
  rsq <- summary(model)$r.squared
  
  for (i in 1:ncol(rmat)) {
    chemi <- sum(rmat[, i] >= rsq) / k
    print(chemi < alpha)
    print("The correspoding p-value: ")
    print(chemi)
    print("result from quantile test: ")
    print(rsq > quantile(rmat[, i], 1 - alpha))
  }
  
}

# fit the model
model <- lm(HEAT ~ ., data = data)
summary(model)
# check variance inflation factors
vif(model)

# do permutation test
permutation_test(data)

# remove nonsignificant variables
data <- data[, -3]
colnames(data)

# fit the model
model <- lm(HEAT ~ ., data = data)
summary(model)
# check variance inflation factors
vif(model)

# do permutation test
permutation_test(data)

# remove nonsignificant variables
data <- data[, -3]
colnames(data)

# fit the model
model <- lm(HEAT ~ ., data = data)
summary(model)
# check variance inflation factors
vif(model)

# do permutation test
permutation_test(data)

# 2.4

# (a)
crop <- read.table("crop.txt", header=T, sep="\t")
head(crop)

crop_model <- lm(Yield ~ Fertilizer, data=crop)
summary(crop_model)

plot(Yield ~ Fertilizer, data=crop)
abline(crop_model, col="red")

par(mfrow = c(3, 2))
plot(crop_model)

# (b)
crop_model2 <- lm(Yield ~ Fertilizer + LSqrd, data=crop)
summary(crop_model)

#plot(Yield ~ Fertilizer + LSqrd, data=crop)
#abline(crop_model2, col="red")

par(mfrow = c(3, 2))
plot(crop_model2)