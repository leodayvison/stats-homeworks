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

################QUESTÃO 2#####################
#install.packages("palmerpenguins"); não vamos mais precisar
library(palmerpenguins);
penguins_data <-na.omit(penguins) # desconsiderando os dados faltantes

# Item 1
x <- penguins_data$body_mass_g
y <- penguins_data$bill_length_mm

plot(x, y,
     main = "Dispersão: Massa Corporal vs Comprimento do Bico",
     xlab = "Massa Corporal (g)",
     ylab = "Comprimento do Bico (mm)",
     col = "blue")

cor(x, y)

# Item 2
# Minimos Quadrados
x_media <- mean(x)
y_media <- mean(y)

numerador <- sum((x - x_media) * (y - y_media))
denominador <- sum((x - x_media)^2)

beta1 <- numerador / denominador
beta0 <- y_media - (beta1 * x_media)

# Usando lm()
reg_linear <- lm(y ~ x)
coeficientes <- coef(reg_linear)

plot(x, y,
     main = "Regressão Linear: Massa vs Bico",
     xlab = "Massa Corporal (g)",
     ylab = "Comprimento do Bico (mm)",
     col = "blue")

abline(reg_linear, col = "red", lwd = 2)

# Item 3
# Residuos
y_reg <- beta0+(beta1*x)
residuos <- y - y_reg

# MSE, RMSE e R2
mse <- mean(residuos^2)
rmse <- sqrt(mse)
sq_residuos <- sum(residuos^2)
sq_total <- sum((y - mean(y))^2)
r2 <- 1 - (sq_residuos / sq_total)

plot(y_reg, residuos,
     main = "Análise de Resíduos",
     xlab = "Valores Modelados",
     ylab = "Resíduos",
     pch = 19, col = "darkgray")
abline(h = 0, col = "red", lty = 2, lwd = 2) #zero

# Item 4
# Introduzir outlier
x_mod <- x
y_mod <- y
x_mod[1] <- 8000
y_mod[1] <- 15

# Nova regressão
reg_mod <- lm(y_mod ~ x_mod)

residuos_mod <- residuals(reg_mod)
rmse_mod <- sqrt(mean(residuos_mod^2))
r2_mod <- summary(reg_mod)$r.squared


# Plot
plot(x, y,
     main = "Influência do Outlier na Regressão",
     xlab = "Massa Corporal (g)",
     ylab = "Comprimento do Bico (mm)",
     col = "gray", pch = 1) # Dados originais em cinza claro

# Outlier em destaque
points(x_mod[1], y_mod[1], col = "blue", pch = 19, cex = 2) # Ponto azul grande
text(x_mod[1], y_mod[1], labels = "Outlier", pos = 4, col = "blue")

abline(beta0, beta1, col = "red", lwd = 2, lty = 2)
abline(reg_mod, col = "blue", lwd = 2)

legend("topleft", 
       legend = c("Reta Original", "Reta com Outlier"),
       col = c("red", "blue"), 
       lty = c(2, 1), lwd = 2)
