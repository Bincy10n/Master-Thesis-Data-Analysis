library(dplyr)
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)
library(lubridate)
library(gridExtra)

WIND <- read_excel("C:/Users/Hp/Downloads/wind.xlsx")
RAINFALL <- read_excel("C:/Users/Hp/Downloads/total rainfall.xlsx")
MIN_TMP <- read_excel("C:/Users/Hp/Downloads/avg min temp.xlsx")
MAX_TMP <- read_excel("C:/Users/Hp/Downloads/avg max temp.xlsx")

Joined_data <- full_join(WIND, RAINFALL, by = c("Weather_Station","Month")) %>%
  full_join(MIN_TMP, by = c("Weather_Station", "Month")) %>%
  full_join(MAX_TMP, by = c("Weather_Station", "Month"))

##Finding the mean temperature
Joined_data <- Joined_data %>%
  mutate(Mean_Temp = (`Average Minimum Temperature` + `Average Maximum Temperature`) / 2)

Climate_data <- Joined_data %>%
  filter(Weather_Station %in% "Dublin airport")

# Assuming you have a data frame called "merged_data" after merging the files
Climate_data$Month <- format(as.Date(paste0(Climate_data$Month, "01"), format = "%YM%m%d"), "%Y %b")


##replacing na values with 0
Climate_data <- Climate_data %>%na.omit()

##Splitting year and month
Climate_data <- Climate_data %>%
  separate(Month, into = c("Year", "Month"), sep = " ", remove = FALSE)

# Drop rows for the year 2009
Climate_data <- Climate_data[Climate_data$Year != 2020, ]
# Change the column names of your dataset
colnames(Climate_data) <- c("Weather_Station","Year", "Month","Wind","Rainfall", "avg_min_temperature", "avg_max_temperature", "Mean_Temp")
#write_xlsx(Climate_data, "Final_climate_dublin.xlsx")


# Assuming your data is stored in a dataframe called 'climate_data'


Climate_data$Season <- case_when(
  Climate_data$Month %in% c("Dec", "Jan", "Feb") ~ "Winter",
  Climate_data$Month %in% c("Mar", "Apr", "May") ~ "Spring",
  Climate_data$Month %in% c("Jun", "Jul", "Aug") ~ "Summer",
  Climate_data$Month %in% c("Sep", "Oct", "Nov") ~ "Autumn"
)
Seasonal_data <- Climate_data %>%
  group_by(Season,Year) %>%
  summarise(avg_Mean_Temp = mean(Mean_Temp),
            avg_rainfall=mean(Rainfall),
            avg_wind=mean(Wind))

selected_seasons <- c("Summer", "Winter", "Spring", "Autumn")

# Filter the dataset to include only the selected seasons
filtered_seasonal_data <- Seasonal_data %>%
  filter(Season %in% selected_seasons)

# Calculate summaries for each selected season
seasonal_summaries <- filtered_seasonal_data %>%
  group_by(Season) %>%
  summarise(avg_Mean_Temp = mean(avg_Mean_Temp),
            avg_rainfall = mean(avg_rainfall),
            avg_wind = mean(avg_wind))

# Print the resulting seasonal summaries
print(seasonal_summaries)



summary(Seasonal_data)

plot1<-ggplot(Seasonal_data, aes(x = Year , y = avg_Mean_Temp, color = Season)) +
  geom_point(size=3) +
  geom_line()+
  labs(x = "Year", y = "Average Mean Temperature", title = "Seasonal Changes:Tempearture")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 4),  # Specify the breaks you want
    limits = c(0,20)  # Format labels as percentages
  )


plot2<-ggplot(Seasonal_data, aes(x = Year , y = avg_rainfall, color = Season)) +
  geom_point(size=3) +
  geom_line()+
  labs(x = "Season", y = "Average Rainfall", title = "Seasonal Changes:Rainfall")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(
    breaks = seq(20,130, by =30 ),  # Specify the breaks you want
    limits = c(20,130)  # Format labels as percentages
  )

plot3<-ggplot(Seasonal_data, aes(x = Year , y = avg_wind, color = Season)) +
  geom_point(size=3) +
  geom_line()+
  labs(x = "Season", y = "Average Wind speed", title = "Seasonal Changes:Wind")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(
    breaks = seq(30, 65, by = 10),  # Specify the breaks you want
    limits = c(30,65)  # Format labels as percentages
  )

grid.arrange(plot1, plot2, plot3,  ncol=2 )



Climate_data$Date <- ymd(paste(Climate_data$Year, Climate_data$Month, "1"))

Temp_plot<-ggplot(Climate_data, aes(x = Date, y = Mean_Temp)) +
  geom_line() +
  geom_point(color="blue")+
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Year", y = "Temperature") +
  ggtitle("Climate Change: Temperature Variation (with LOESS method) in Dublin Airport")

wind_plot<-ggplot(Climate_data, aes(x = Date, y = Wind)) +
  geom_line() +
  geom_point(color="blue")+
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Year", y = "Wind") +
  ggtitle("Climate Change: Wind Variation (with LOESS method) in Dublin Airport")
rainfall_plot<-ggplot(Climate_data, aes(x = Date, y = Rainfall)) +
  geom_line() +
  geom_point(color="blue")+
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Year", y = "Rainfall") +
  ggtitle("Climate Change: Temperature Variation (with LOESS method) in Dublin Airport")


# Arrange the plots using grid.arrange
grid.arrange(Temp_plot,wind_plot,rainfall_plot,ncol=2 )
summary(Climate_data)

# Create separate plots for each climate variable
plot_rainfall <- ggplot(overall, aes(x = Year, y = avg_rainfall)) +
  geom_col( fill = "blue") +
  labs(x = "Year", y = "Rainfall (mm)") +
  ggtitle("Climate Change: Annual Rainfall in Dublin")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(
  breaks = seq(0,80, by = 10),  # Specify the breaks you want
    limits = c(0,80 )  # Format labels as percentages
  )

plot_wind <- ggplot(overall, aes(x = Year, y = avg_wind)) +
  geom_col( fill = "red") +
  labs(x = "Year", y = "Wind Speed") +
  ggtitle("Climate Change: Wind Speed in Dublin")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),  # Specify the breaks you want
    limits = c(0,50)  # Format labels as percentages
  )


plot_temperature <- ggplot(overall, aes(x = Year, y = avg_Mean_Temp)) +
  geom_col( fill = "orange") +
  labs(x = "Year", y = "Temperature (Celsius)") +
  ggtitle("Climate Change: Temperature in Dublin")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(
    breaks = seq(0,12, by = 2),  # Specify the breaks you want
    limits = c(0,12)  # Format labels as percentages
  )

# Arrange the plots using grid.arrange
grid.arrange(plot_rainfall, plot_wind, plot_temperature,ncol=2 )

overall<-Climate_data %>%
  group_by(Year) %>%
  summarise(avg_Mean_Temp = mean(Mean_Temp),
            avg_rainfall=mean(Rainfall),
            avg_wind=mean(Wind))

selected_year <- c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")

# Filter the dataset to include only the selected seasons
filtered_year_data <- overall %>%
  filter(Year %in% selected_year )

# Calculate summaries for each selected season
year_summaries <- filtered_year_data %>%
  group_by(Year) %>%
  summarise(avg_Mean_Temp = mean(avg_Mean_Temp),
            avg_rainfall = mean(avg_rainfall),
            avg_wind = mean(avg_wind))

# Print the resulting seasonal summaries
print(year_summaries)
print (overall)


# Load necessary libraries if not already loaded
library(dplyr)

# Define the seasons you want to keep
selected_seasons <- c("Summer", "Winter", "Spring", "Autumn")

# Create an empty list to store the results
seasonal_summaries <- list()

# Loop through each selected season
for (season in selected_seasons) {
  # Filter data for the current season
  filtered_data <- Seasonal_data %>%
    filter(Season == season)
  
  # Calculate mean and standard deviation for each variable
  avg_Mean_Temp_mean <- mean(filtered_data$avg_Mean_Temp)
  avg_Mean_Temp_sd <- sd(filtered_data$avg_Mean_Temp)
  
  avg_rainfall_mean <- mean(filtered_data$avg_rainfall)
  avg_rainfall_sd <- sd(filtered_data$avg_rainfall)
  
  avg_wind_mean <- mean(filtered_data$avg_wind)
  avg_wind_sd <- sd(filtered_data$avg_wind)
  
  # Create a summary data frame for the current season
  summary_df <- data.frame(
    Season = season,
    avg_Mean_Temp_mean = avg_Mean_Temp_mean,
    avg_Mean_Temp_sd = avg_Mean_Temp_sd,
    avg_rainfall_mean = avg_rainfall_mean,
    avg_rainfall_sd = avg_rainfall_sd,
    avg_wind_mean = avg_wind_mean,
    avg_wind_sd = avg_wind_sd
  )
  
  # Add the summary data frame to the list
  seasonal_summaries[[season]] <- summary_df
}

# Combine the list of summary data frames into a single data frame
final_summary <- do.call(rbind, seasonal_summaries)

# Print the final summary
print(final_summary)
# Print the final summary as a LaTeX table
kable(final_summary, format = "latex", booktabs = TRUE, caption = "Summary Statistics by Season")
