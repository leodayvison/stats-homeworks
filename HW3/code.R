library(ggplot2) # if needed, please type 'install.packages("ggplot2")' in your R console

#############################################################
######################   QUESTION 1    ######################
#############################################################

X <- c(0.99, 2.31, 10.85, 6.15, 10.81, 3.72, 5.75, 4.15, 9.27, 7.84, 2.31, 10.85, 6.15, 1.81, 3.72, 5.75, 10.40, 10.04, 4.15, 9.27)


########### item 3 ###########
sumX <- sum(X)
n <- length(X)
mleX <- n/sumX
mleX


########### item 4 ###########
lambda <- seq (0, 1, length.out = 300)
log_lik_x <- n * log(lambda) - lambda*sumX
plot (lambda, log_lik_x)
log_lik_max <- n * log (mleX) - mleX * sumX
points (mleX, log_lik_max, col = "purple", pch = 19 , cex = 2)

maxLik <- 20*log(mleX, base = exp(1)) - mleX*sumX
maxLik

########### item 4 ###########
expec_val <- 1/mleX
expec_val
