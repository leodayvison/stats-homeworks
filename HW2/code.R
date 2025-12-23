############ QUESTION 1 ############

# Item 1
n <- 50
k <- 0:n
p <- 0.7

probability <- dbinom(x = k,
                      size = n,
                      prob = p)
probability

# Item 2
plot(x = k,
     y = probability,
     ylab = "Probability",
     xlab = "k",
     main = "Probability Mass Function")

cumfunc <- pbinom(q = k,
                  size = n,
                  prob = p)
plot(x = k,
     y = cumfunc,
     xlab = "k",
     ylab = "Probability",
     main = "Cumulative Distribution Function")

# Item 3
expected_value <- sum(k*probability) 

diff <- (k - expected_value)^2
x_variance <- sum(diff*probability)

std_dev <- sqrt(x_variance)

expected_value
x_variance
std_dev

# Item 4
prob_leq_20 <- 1 - pbinom(q=19, size=n, prob=p)
prob_btw_3043 <- pbinom(q=42, size=n, prob=p) - pbinom(q=30, size=n, prob=p)
prob_eq_31 <- dbinom(x=31, size=n, prob=p)

prob_leq_20
prob_btw_3043
prob_eq_31

# Item 6
popular_dessert_prob <- dbinom(x = k,
                          size = n,
                          prob = p + 0.1)
more_clients <- c(k, 51:60)
more_clients_prob <- dbinom(x = more_clients,
                            size = n + 10,
                            prob = p)
plot(y=popular_dessert_prob,
     x=k,
     ylab="Probability",
     main="Probability w/ more popular dessert")
plot(y = more_clients_prob,
     x = more_clients,
     ylab="Probability",
     main="Probability w/ more clients")