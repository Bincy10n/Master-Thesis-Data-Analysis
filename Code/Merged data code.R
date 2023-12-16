# Load required libraries
library(ggplot2)
library(dplyr)
library(dplyr)
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)
library(lubridate)
library(gridExtra)

merged_data <-read_excel("C:/Users/Hp/Downloads/output_data(1).xlsx")

create_airport_plots <- function(country_name) {
  airport_data <- subset(merged_data, Country == country_name)
  
  plot_wind <- ggplot(airport_data, aes(x = Wind, y = Passenger.Number)) +
    geom_point(color='blue')+
    geom_smooth(method = "lm") +
    labs(x = "Wind", y = "Passenger Number") +
    ggtitle(paste("Wind vs. Passenger Number from", country_name, "Airport"))
  
  plot_rainfall <- ggplot(airport_data, aes(x = Rainfall, y = Passenger.Number)) +
    geom_point(color='red')+
    geom_smooth(method = "lm") +
    labs(x = "Rainfall", y = "Passenger Number")+
    ggtitle(paste("Rainfall vs. Passenger Number from", country_name, "Airport"))
  
  plot_temperature <- ggplot(airport_data, aes(x = Mean_temperature, y = Passenger.Number)) +
    geom_point(color='dark orange')+
    geom_smooth(method = "lm") +
    labs(x = "Mean Temperature", y = "Passenger Number") +
    ggtitle(paste("Mean Temperature vs. Passenger Number from", country_name, "Airport"))
  
  grid.arrange(plot_rainfall, plot_wind, plot_temperature, ncol=2 )
}

create_airport_plots("Great Britain")
create_airport_plots("France")
create_airport_plots("Germany")
create_airport_plots("Portugal")
create_airport_plots("Netherlands")

overall_merged<-overall<-merged_data %>%
  group_by(Year) %>%
  summarise(avg_Mean_Temp = mean(Mean_temperature),
            avg_rainfall=mean(Rainfall),
            avg_wind=mean(Wind),
            avg_passenger=mean(Passenger.Number))


a<-ggplot(overall_merged, aes(x = avg_Mean_Temp, y = avg_passenger)) +
  geom_point(color='dark orange')+
  geom_smooth(method = "lm") +
  labs(x = "Average Temperature", y = "Average Passenger Number") +
  scale_y_continuous(
    breaks = seq(25000, 45000, by = 5000),  # Specify the breaks you want
    limits = c(25000, 45000)  # Format labels as percentages
  )

b<- ggplot(overall_merged, aes(x = avg_wind, y = avg_passenger)) +
  geom_point(color='blue')+
  #geom_line()+
  geom_smooth(method = "lm") +
  labs(x = "Average Wind", y = " Average Passenger Number") +
  scale_y_continuous(
    breaks = seq(25000, 45000, by = 5000),  # Specify the breaks you want
    limits = c(25000, 45000)  # Format labels as percentages
  )

c<-ggplot(overall_merged, aes(x = avg_rainfall, y = avg_passenger)) +
  geom_point(color='red')+
  #geom_line()+
  geom_smooth(method = "lm") +
  labs(x = "Average Rainfall", y = "Avg_Passenger Number")+
  scale_y_continuous(
    breaks = seq(25000, 45000, by = 5000),  # Specify the breaks you want
    limits = c(25000, 45000)  # Format labels as percentages
  )

grid.arrange(a, b, c, ncol=3 )