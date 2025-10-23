#############################################################
######################   QUESTION 1    ######################
#############################################################


# our data
gas_values <- c(15.8, 22.7, 26.8, 19.1, 18.5, 14.4, 8.3, 25.9, 26.4, 9.8, 21.9, 10.5, 17.3, 6.2, 18.0, 22.9, 24.6, 19.4, 12.3, 15.9, 20.1, 17.0, 22.3, 27.5, 23.9, 17.5, 11.0, 20.4, 16.2, 20.8, 20.9, 21.4, 18.0, 24.3, 11.8, 17.9, 18.7, 12.8, 15.5, 19.2, 13.9, 28.6, 19.4, 21.6, 13.5, 24.6, 20.0, 24.1, 9.0, 17.6, 25.7, 20.1, 13.2, 23.7, 10.7, 19.0, 14.5, 18.1, 31.8, 28.5, 22.7, 15.2, 23.0, 29.6, 11.2, 14.7, 20.5, 26.6, 13.3, 18.1, 24.8, 26.1, 7.7, 22.5, 19.3, 19.4, 16.7, 16.9, 23.5, 18.4)

# central tendency measures
mean(gas_values)
median(gas_values)

freq_table <- table(gas_values) # builds the ordered frequency table
gas_mode <- which.max(freq_table) # returns the value with the maximum frequency in the table (and also it's index)
gas_mode

# histogram
hist(gas_values, xlab="value", ylab="frequency") # for better visualization
plot(freq_table, xlab="value", ylab="frequency") # more detailed
# boxplot
boxplot(gas_values, ylab="daily gas emission")

# sorting for making things easier
gas_values <- sort(gas_values)
data_len <- length(gas_values) # there are 73 samples
# first quartile
q1 <- gas_values[(data_len+1)/4]
q1
# second quartile (median)
q2 <- median(gas_values)
q2
# third quartile
q3 <- gas_values[(data_len+1)*(3/4)]
q3

# declaring ppm > 25 condition
condition <- gas_values > 25
gas_above_limit <- gas_values[condition]
gas_above_limit
length(gas_above_limit) # 11 out of 73 days with gas emission above the limit. it's not exactly critical, but should be minimized


#############################################################
######################   QUESTION 2    ######################
#############################################################
# our data
idade <- c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46)
nacionalidade <- c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola", "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa", "Francesa", "Espanhola", "Italiana", "Alemã", "Francesa", "Italiana", "Alemã", "Italiana")
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1, 2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)

########### item 1 ###########
media_idade <- mean(idade)
media_renda <- mean(renda)
media_experiencia <- mean(experiencia)

mediana_idade <- median(idade)
mediana_renda <- median(renda)
mediana_experiencia <- median(experiencia)

dp_idade <- sd(idade)
dp_renda <- sd(renda)
dp_experiencia <- sd(experiencia)

########### item 2 ###########
df <- data.frame(idade, nacionalidade, renda, experiencia)

nac_media_renda <- tapply(df$renda, df$nacionalidade, mean)
nac_media_experiencia <- tapply(df$experiencia, df$nacionalidade, mean)

i_maior_renda <- which.max(nac_media_renda)
nac_maior_renda <- names(nac_media_renda[i_maior_renda])
print(paste("Nacionalidade com maior renda:", nac_maior_renda))

i_maior_experiencia <- which.max(nac_media_experiencia)
nac_maior_experiencia <- names(nac_media_experiencia[i_maior_experiencia])
print(paste("Nacionalidade com maior experiência:", nac_maior_experiencia))

########### item 3 ###########
pearson <- cor(df$renda, df$experiencia)
print(paste("Coeficiente de correlação de Pearson entre experiência e renda:", pearson))
plot(df$experiencia, df$renda, main = "Renda desejada em função da epxeriência", xlab = "Experiencia", ylab = "Renda Desejada")

########### item 4 ###########
selecionados <- subset(df, df$renda < 2 & df$experiencia >= 10)

num_selecionados <- nrow(selecionados)
print(paste("Numero de candidatos selecionados:", num_selecionados))

print(selecionados$nacionalidade)
print(selecionados$idade)

########### item 5 ###########
boxplot(idade ~ nacionalidade,
        data = df,
        main = "Distribuição de Idade por Nacionalidade",
        xlab = "Nacionalidade",
        ylab = "Idade (anos)"
)

#############################################################
######################   QUESTION 3    ######################
#############################################################


# importing dataset
bike_sharing <- read.csv("/home/leodayvison/Academico/Estatística para Engenharia/Homeworks/HW1/HW1_bike_sharing.csv")
bike_sharing
 
########### item 2 ###########
attach(bike_sharing)

# temperature
temp_median <- median(temp)
temp_mean <- mean(temp)
temp_q1 <- temp[(length(temp))/4]
temp_q2 <- median(temp)
temp_q3 <- temp[(length(temp))*(3/4)]

# number of casual users
casual_median <- median(casual)
casual_mean <- mean(casual)
casual_q1 <- casual[(length(casual))/4]
casual_q2 <- median(casual)
casual_q3 <- casual[(length(casual))*(3/4)]

# number of registered users
reg_median <- median(registered)
reg_mean <- mean(registered)
reg_q1 <- registered[(length(registered))/4]
reg_q2 <- median(registered)
reg_q3 <- registered[(length(registered))*(3/4)]

# table 
atrb_median <- c(temp_median, casual_median, reg_median)
atrb_mean <- c(temp_mean, casual_mean, reg_mean)
atrb_q1 <- c(temp_q1, casual_q1, reg_q1)
atrb_q2 <- c(temp_q2, casual_q2, reg_q2)
atrb_q3 <- c(temp_q3, casual_q3, reg_q3)
tab <- cbind(atrb_median, atrb_mean, atrb_q1, atrb_q2, atrb_q3)
dimnames(tab) <- list(Attribute = c("temperature", "casual users", "registered users"), Measures = c("median", "mean", "Q1", "Q2", "Q3"))

tab # table 

########### item 3 ###########



########### item 4 ###########
library(ggplot2) # if needed, please type 'install.packages("ggplot2")' in your R console
total_users <- casual + registered
real_temp <- temp*41

dates <- as.Date(dteday)
users_ts <- data.frame(Date=dates, Users=total_users)
temp_ts <- data.frame(Date=dates, Temp=real_temp)

ggplot(users_ts, aes(x = Date, y = Users)) + geom_line(color = "purple") + labs(title = "Total users time series", x = "Date", y = "Total Users") + theme_minimal()
ggplot(temp_ts, aes(x = Date, y = Temp)) + geom_line(color = "red") + labs(title = "Temperature time series", x = "Date", y = "Temperature") + theme_minimal()
