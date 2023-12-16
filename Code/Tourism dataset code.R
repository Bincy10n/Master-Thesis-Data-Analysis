library(dplyr)  # Load dplyr package
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)
library(ggplot2)
library(lubridate)
data <- read.csv("C:/Users/Hp/Downloads/dublin  tourism data.csv")


# Assuming you have a data frame called "merged_data" after merging the files
data$Month <- format(as.Date(paste0(data$Month, "01"), format = "%YM%m%d"), "%Y %b")

##Splitting year and month
data <- data %>%
  separate(Month, into = c("Year", "Month"), sep = " ", remove = FALSE)

# Filter the data for foreign airports with less than 3 null values
airport_names_less_than_3_null <- data %>%
  group_by(Foreign.Airport) %>%
  summarise(null_count = sum(is.na(Passenger.Number))) %>%
  filter(null_count <1) %>%
  pull(Foreign.Airport)
airport_names_less_than_3_null


filtered_data <- data %>%
  filter(Foreign.Airport %in% airport_names_less_than_3_null)

# List of unwanted airports
unwanted_airports <- c("Abu Dhabi (AUH),United Arab Emirates","Manchester (MAN),Great Britain","London - Gatwick (LGW),Great Britain","Boston (BOS),USA" ,"Nice - Cote D'Azur (NCE),France" ,"Lisbon (LIS),Portugal"  )
# Filter out the unwanted airports
filtered_data <- filtered_data %>%
  filter(!Foreign.Airport %in% unwanted_airports)                                                                                                                     
filtered_data

filtered_data <- filtered_data %>%
  filter(!is.na(Passenger.Number))

write_xlsx(filtered_data, "Final_tourism_dublin.xlsx")
# Drop rows for the year 2009
filtered_data <- filtered_data[filtered_data$Year != 2020, ]

# Example dictionary mapping airport names to countries
airport_country_mapping <- c("Faro (FAO),Portugal"= "Portugal",
                             "Amsterdam (AMS),Netherlands"= "Netherlands",
                             "Paris - Charles De Gaulle (CDG),France"="France",
                             "London - Heathrow (LHR),Great Britain"= "Great Britain",
                             "Frankfurt (FRA),Germany"= "Germany"
)

# Function to map airport names to countries
map_airport_to_country <- function(airport_name) {
  if (airport_name %in% names(airport_country_mapping)) {
    return(airport_country_mapping[[airport_name]])
  } else {
    return("Unknown")
  }
}

# Create a new column 'Country' based on the mapping
filtered_data$Country <- sapply(filtered_data$Foreign.Airport, map_airport_to_country)

# Now 'df' contains a new 'Country' column with corresponding country names


filtered_data$time <- ymd(paste(filtered_data$Year, filtered_data$Month, "1", sep = "-"))


####
annual_passenger_data <- filtered_data %>%
  group_by(Country, Year) %>%
  summarise(Total_Passengers = sum(Passenger.Number))

# Create a line plot for annual number of passengers
plot_annual_passengers <- ggplot(annual_passenger_data, aes(x = Year, y = Total_Passengers, fill = Country)) +
  
  #geom_bar(stat = "identity", position = "dodge") +
  geom_bar(stat = "identity",width=0.7, position=position_dodge(1.5))+
  labs(x = "Year", y = "Total Passengers") +
  ggtitle("Annual Number of Passengers for Foreign Countries") +
  theme_minimal() +  # You can customize the theme if needed
  scale_color_discrete(name = "Country")  +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display the line plot
print(plot_annual_passengers)

plot_annual_passengers_facet <- ggplot(annual_passenger_data, aes(x = Year, y = Total_Passengers, fill = Country)) +
  geom_bar(stat = "identity",width=0.7, position=position_dodge(1.5)) +
  labs(x = "Year", y = "Total Passengers") +
  ggtitle("Annual Number of Passengers for Foreign Countries") +
  theme_minimal() +  # You can customize the theme if needed
  scale_fill_discrete(name = "Country") +  # Customize the legend title
  facet_wrap(~ Country, ncol = 5)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1.2,vjust=1.2))
  
# Display the facet wrap of bar plots
print(plot_annual_passengers_facet)

overall_trend <- annual_passenger_data %>%
  group_by(Year) %>%
  summarise(Average_Passengers = mean(Total_Passengers))

# Create a line plot for overall trend of passenger numbers
plot_overall_trend <- ggplot(overall_trend, aes(x = Year, y = Average_Passengers)) +
  geom_line() +
  geom_bar(stat = "identity",,fill="#104E8B") +
  labs(x = "Year", y = "Average Passengers") +
  ggtitle("Overall Trend of Annual Average Passengers") +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma) 
