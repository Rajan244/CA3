library(dplyr)
data_2011 <- read.csv("dublin-pm10-2.5-2011.csv", header = TRUE, 
                      na.strings = c("", " ", "NA"))
data_2012 <- read.csv("dublin-pm10-2.5-2012.csv", header = TRUE, 
                      na.strings = c("", " ", "NA"))

#below, drop the auto genarated columns from tables
data_2011 <- data_2011[,colSums(is.na(data_2011))<nrow(data_2011)] 
data_2012 <- data_2012[,colSums(is.na(data_2012))<nrow(data_2012)]

#to show the structure of tables
str(data_2011)
head(data_2011)
str(data_2012)
head(data_2012)

#selecting only two sites 
new_data <- subset(data_2011, select = c(Marino, Rathmines, Date))
new_data1 <- subset(data_2012, select = c(Marino, Rathmines, Date))

#merging the tables
final_data <- rbind(new_data,new_data1)
View(final_data)

#Here we have replaced NA values with specific mean of that column
for(i in 1:ncol(final_data)){
  final_data[is.na(final_data[,i]), i] <- mean(final_data[,i], na.rm = TRUE)
}

#mutate_if() using to take only 2 digits after decimal point
final_data <- final_data %>% 
  mutate_if(is.numeric, round, digits = 2)

str(final_data) #showing the structure of clean dataset

#
normalty_test <- shapiro.test(final_data$Marino)
normalty_test
normalty_test1 <- shapiro.test(final_data$Rathmines)
normalty_test1

#
chisq.test(final_data$Marino)
chisq.test(final_data$Rathmines)

# To get the average value of each location
mean1 <- mean(final_data$Marino)
mean1
mean2 <- mean(final_data$Rathmines)
mean2

# to get the standard deviation of each location
sd1 <- sd(final_data$Marino) 
sd1
sd2 <- sd(final_data$Rathmines)
sd2

# To get the gather standard deviation of both location
avg_sd <- (sd1 + sd2)/2
avg_sd

# To get the N 
delta1 <- (mean2 - mean1)/avg_sd
delta1

#
library(pwr)
power.t.test(delta = delta1, n = NULL, sig.level = 0.05, power = 0.90,
             type = "two.sample", alternative = "two.sided")
#
power_info <- pwr.t.test(d = delta1,
                         sig.level = 0.05,
                         power = 0.99,
                         type = "two.sample",
                         alternative = "two.sided")
power_info
plot(power_info)

#
cohen.ES(test = c("chisq"), size = c("small"))

#
final <- cor.test(final_data$Marino, final_data$Rathmines, method = "spearman", 
                  exact = FALSE)
final
