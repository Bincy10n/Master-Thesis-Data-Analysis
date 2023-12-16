library(dplyr)
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)
library(lubridate)
library(gridExtra)

output_data_1_ <- read_excel("C:/Users/Hp/Downloads/output_data(1).xlsx")

dataset = output_data_1_ 
View(dataset)  


#understanding distribution
library(lattice)
with(dataset, densityplot(Passenger.Number)) #right skewed - long tail on the right




#checking for outliers
library(pastecs)
stat.desc(dataset$Passenger.Number, basic = FALSE, norm = TRUE)

#make sure dependent variable is numeric
dataset$Passenger.Number = as.numeric(dataset$Passenger.Number)
is.numeric(dataset$Passenger.Number)
is.numeric(dataset$Rainfall)
is.numeric(dataset$Wind)
is.numeric(dataset$Mean_temperature)

#scaling the dependent variable
dataset$Passenger.Number_z <-scale(dataset$Passenger.Number)
View(dataset)


#log transformation the dependent variable

dataset$logPassenger.Number <- log(1+dataset$Passenger.Number)
#checking normality of distribution of dependent variable

#comparing distributions after log transformation
densityplot(dataset$Passenger.Number)
densityplot(dataset$logPassenger.Number) 


hist.logPassenger.Number<- ggplot(dataset, aes(logPassenger.Number)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Passenger.Number", y = "Densidade") + stat_function (fun = dnorm, args = list(mean = mean(dataset$logPassenger.Number, na.rm = TRUE), sd = sd(dataset$logPassenger.Number, na.rm = TRUE)), colour = "black", size = 1) 
hist.logPassenger.Number 

# check outliers in log transformation via z-scores
boxplot(dataset$logPassenger.Number, xlab = "Todas as categorias", ylab = "Passenger.Number (log)")


library(psych)


dataset$logPassenger.Numberz<-scale(dataset$logPassenger.Number)
describe(dataset$logPassenger.Numberz)
stat.desc(dataset$logPassenger.Numberz)
sortcategorybylogPassenger.Numberz <- dataset[order(dataset$logPassenger.Numberz),] #sort categories by z-score of RT
table(dataset$logPassenger.Numberz > 2.5 | dataset$logPassenger.Numberz < -2.5) # 18
tail(sortcategorybylogPassenger.Numberz, n = 18)



#removing outliers from variable 'logPassenger.Number' that are 2,5 sd away from mean
dataset_logtrimmed = dataset[abs(scale(dataset$logPassenger.Number)) < 2.5,] #10 outliers removed 
densityplot(dataset_logtrimmed$logPassenger.Number)
hist.logPassenger.Number <- ggplot(dataset_logtrimmed, aes(logPassenger.Number)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Passenger.Number (log)", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(dataset_logtrimmed$logPassenger.Number, na.rm = TRUE), sd = sd(dataset_logtrimmed$logPassenger.Number, na.rm = TRUE)), colour = "red", size = 2) 
hist.logPassenger.Number #Great
skew(dataset_logtrimmed$logPassenger.Number)
attach(dataset_logtrimmed)

#shapiro test did not show normality of the distribution
shapiro.test(dataset_logtrimmed$logPassenger.Number)  



hist(dataset$Passenger.Number)
hist(dataset$Passenger.Number_z)
hist(dataset$logPassenger.Number)

#checking whether there are linear relationships 

plot(dataset$logPassenger.Number ~ dataset$Wind, data = dataset)
plot(dataset$logPassenger.Number ~ dataset$Mean_temperature, data = dataset) #looks linear
plot(dataset$logPassenger.Number ~ dataset$Rainfall, data = dataset)



tourism.lm_all_factors <- lm(dataset$logPassenger.Number ~ dataset$Mean_temperature+dataset$Rainfall+dataset$Wind, data = dataset)
summary(tourism.lm_all_factors)

#according to summary, we can see that only the variable Mean_Temp is significant

#good tutorial to implement LM: https://www.scribbr.com/statistics/linear-regression-in-r/
#explaining normal and skewed distributions: https://statisticsbyjim.com/basics/skewed-distribution/

library(corrplot)

# Calculate Pearson correlation matrix and p-values
pearson_corr <- cor(dataset_logtrimmed[c("logPassenger.Number", "Mean_temperature", "Rainfall", "Wind")], method = "pearson")
pearson_pvalues <- cor.test(dataset_logtrimmed$logPassenger.Number, dataset_logtrimmed$Mean_temperature, method = "pearson")$p.value

# Print Pearson correlation matrix and p-values
print("Pearson Correlation Matrix:")
print(pearson_corr)
print("Pearson Correlation P-values:")
print(pearson_pvalues)

# Assuming you have calculated pearson_corr and spearman_corr matrices
correlation_matrix <- pearson_corr # or spearman_corr


# Plot the correlation matrix heatmap with the "coolwarm" color palette
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", col = colorRampPalette(c("blue", "white", "red"))(20))




# Calculate Spearman correlation matrix and p-values
spearman_corr <- cor(dataset_logtrimmed[c("logPassenger.Number", "Mean_temperature", "Rainfall", "Wind")], method = "spearman")
spearman_pvalues <- cor.test(dataset_logtrimmed$logPassenger.Number, dataset_logtrimmed$Mean_temperature, method = "spearman")$p.value

# Print Spearman correlation matrix and p-values
print("Spearman Correlation Matrix:")
print(spearman_corr)
print("Spearman Correlation P-values:")
print(spearman_pvalues)




