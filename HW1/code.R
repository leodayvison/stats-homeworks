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

# fica pra vc andre




#############################################################
######################   QUESTION 3    ######################
#############################################################


# importing dataset
bike_sharing <- read.csv("HW1_bike_sharing.csv")
bike_sharing
 
# item 2
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
