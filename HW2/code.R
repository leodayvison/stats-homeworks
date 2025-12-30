library(ggplot2)
set.seed(42)
############ QUESTION 1 ############
rm(list=ls()) 			# clean the working space
graphics.off()			# close all the graphic windows

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


############ QUESTION 2 ############
rm(list=ls()) 			# clean the working space
graphics.off()			# close all the graphic windows


# Item 4
set.seed(42)
simulacoes <- rbinom(100000, size = 10^7, prob = 10^-7)
df_sim <- data.frame(vencedores = simulacoes)

x_vals <- 0:8
df_teo <- data.frame(x = x_vals, prob = dpois(x_vals, lambda = 1))

ggplot(df_sim, aes(x = vencedores)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "lightblue", color = "black") +

  geom_point(data = df_teo, aes(x = x, y = prob), color = "red") +
  geom_line(data = df_teo, aes(x = x, y = prob), color = "red") +
  labs(title = "Simulação vs Teoria", x = "Número de Vencedores", y = "Probabilidade")

############ QUESTION 3 ############
rm(list=ls()) 			# clean the working space
graphics.off()			# close all the graphic windows


# Item 1

boxmuller <- function(){
  U1 = runif(1)
  U2 = runif(1)
  R = sqrt(-2*log(U1))
  theta = 2*pi*U2
  Z0 = R*cos(theta)
  Z1 = R*sin(theta)
  temp1 = Z0*3.5 + 62
  temp2 = Z1*3.5 + 62
  res <- c(temp1, temp2)
  return(res)
}

label = rep("s1", 1000)

# Item 2
# Empty vectors
v1 = rep(NA, 1000)
v2 = rep(NA, 1000)

idx <- 1
for (i in 1:500) {
  vals <- boxmuller()
  v1[idx]     <- vals[1]
  v1[idx + 1] <- vals[2]
  idx <- idx + 2
}

v2 <- rnorm(1000, mean=62, sd=3.5)

# Item 3
# (a) Means
m1 = mean(v1)
m1_calc = sum(v1)/1000

m2 = mean(v2)

# (b) Standard Deviations
dp1 = sd(v1)
var1_calc = sum((v1-m1)^2)/(1000-1)
dp1_calc = sqrt(var1_calc)

dp2 = sd(v2)

#(c) Min and max temperature
#max
max1 = max(v1)
max1_calc = v1[1]
for (i in 1:1000) {
  if (v1[i] > max1_calc) {
    max1_calc <- v1[i]
  }
}
max2 = max(v2)

#min
min1 = min(v1)
min1_calc = v1[1]
for (i in 1:1000) {
  if (v1[i] < min1_calc) {
    min1_calc <- v1[i]
  }
}
min2 = min(v2)

#(d) Empirical and theoretical probability P (T > 68).
aux <- v1[v1 > 68]
p68_emp1 = length(aux)/length(v1)

aux <- v2[v2 >68]
p68_emp2 <- length(aux)/length(v2)


p68_teo = pnorm(68, mean=62, sd=3.5, lower.tail = FALSE, log.p = FALSE)

#(e) Empirical and theoretical probability P (60 < T < 65).
aux <- v1[60 < v1 & v1 < 65]
p60_emp1 = length(aux)/length(v1)

aux <- v2[60 < v2 & v2 < 65]
p60_emp2 <- length(aux)/length(v2)

p60_teo = pnorm(65, mean=62, sd=3.5, lower.tail = TRUE, log.p = FALSE) - pnorm(60, mean=62, sd=3.5, lower.tail = TRUE, log.p = FALSE)

#(f) Theoretical probability P (T > 75).
p75_teo = pnorm(75, mean=62, sd=3.5, lower.tail = FALSE, log.p = FALSE)

v1_75 <- v1[v1 > 75]
v2_75 <- v2[v2 > 75]

# Item 4
# comparing our function samples with rnorm's
label <- rep("empvals", 1000)
samples <- v1
df1 <- as.data.frame(cbind(samples, label))

label <- rep("rvals", 1000)
samples <- v2
df2 <- as.data.frame(cbind(samples, label))

df3 <- rbind(df1, df2)
df3$samples <- as.numeric(df3$samples)

ggplot(df3, aes(x=samples, color=label, fill=label)) +
  geom_histogram(position="identity", alpha=0.5)+
  theme(legend.position="top") + theme_classic()

# attaching normal distribution PDF
cpumean <- 62
cpusd <- 3.5
cpupdf <- dnorm(df3$samples, mean=62, sd=3.5)


ggplot(df3, aes(x = samples, color = label, fill = label)) +
  geom_histogram(
    aes(y = after_stat(density)),
    position = "identity",
    alpha = 0.5,
    bins = 30
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = cpumean, sd = cpusd),
    color = "black",
    linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "top")
