# Set working directory
setwd("C:/Users/HATYTUDE CONSULTING/Downloads/Practice Datasets/NSE Statistics")

# View the files in the directory
dir()

# Load in the required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(plyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotly)
library(gganimate)
library(av)
library(readr)
library(readxl)

# Import the preferred data
money_credit_data <- read_csv("MoneyAndCreditStats04102021.csv")
nse_share_data <- read_csv("NSE ALL Share Historical Data (6).csv")
gdp_data <- read.csv("RealGDP04102021.csv")
gdp_growth_rate <- read_csv("nigeria-economic-growth-rate.csv", skip = 15)
fin_mark_indices <- read.csv("MnyMktInd04102021.csv")
first_growth_rate <- read_excel("Final Draft Q4 GDP 2018.xlsx", sheet = 3, skip = 25)
second_growth_rate <- read_excel("Q1_GDP_2020.xlsx", sheet = 3, skip = 25)
third_growth_rate <- read_excel("Q2_GDP_2021.xlsx", sheet = "Contr_Growth", skip = 25)


# Tidy the gdp data to show quarterly data from 2010
gdp_data <- gdp_data %>% filter(Period %in% c("Q1", "Q2", "Q3", "Q4")) %>% 
  select(tyear, Period, TOTAL_GDP)

# Tidy the money credit data to replace the months in figures with words
money_credit_data$Month <- ifelse(money_credit_data$Month == 1, "Jan", 
                             ifelse(money_credit_data$Month == 2, "Feb", 
                                    ifelse(money_credit_data$Month == 3, "Mar", 
                                           ifelse(money_credit_data$Month == 4, "Apr", 
                                                  ifelse(money_credit_data$Month == 5, "May", 
                                                         ifelse(money_credit_data$Month == 6, "Jun", 
                                                                ifelse(money_credit_data$Month == 7, "Jul", 
                                                                       ifelse(money_credit_data$Month == 8, "Aug", 
                                                                              ifelse(money_credit_data$Month == 9, "Sep", 
                                                                                     ifelse(money_credit_data$Month == 10, "Oct", 
                                                                                            ifelse(money_credit_data$Month == 11, "Nov", "Dec")))))))))))
# Tidy the gdp growth rate data
  # Tidy the gdp growth rate data by removing the first row
  gdp_growth_rate <- gdp_growth_rate %>% dplyr::slice(-1)
  # Tidy the gdp growth rate by changing column names
  col_names <- c("Year", "GDP Billions of US$", "Per Capital Income", "Annual Growth Rate (%")
  colnames(gdp_growth_rate) <- col_names
  # Extract the year from the Year(Date) column
  gdp_growth_rate$Year <- year(gdp_growth_rate$Year)

# Tidy the financial market indices table
  # Subset the required rows that are not null
  fin_mark_indices <- fin_mark_indices[, c(13, 2, 1, 12)]
  # Create a new column by replacing the month in numeric to string
  fin_mark_indices$Month <- ifelse(fin_mark_indices$Month == 1, "Jan", 
                                   ifelse(fin_mark_indices$Month == 2, "Feb", 
                                          ifelse(fin_mark_indices$Month == 3, "Mar", 
                                                 ifelse(fin_mark_indices$Month == 4, "Apr", 
                                                        ifelse(fin_mark_indices$Month == 5, "May", 
                                                               ifelse(fin_mark_indices$Month == 6, "Jun", 
                                                                      ifelse(fin_mark_indices$Month == 7, "Jul", 
                                                                             ifelse(fin_mark_indices$Month == 8, "Aug", 
                                                                                    ifelse(fin_mark_indices$Month == 9, "Sep", 
                                                                                           ifelse(fin_mark_indices$Month == 10, "Oct", 
                                                                                                  ifelse(fin_mark_indices$Month == 11, "Nov", "Dec")))))))))))

# Tidy the nse all share data
  # Slice out the first row
  nse_share_data <- nse_share_data %>% dplyr::slice(-1)
  # Convert the Date column from string to date class
  nse_share_data$Date <- as.Date(nse_share_data$Date, format = "%b %d, %Y")
  # Extracting the year from the dates
  nse_share_data$Month <- month(nse_share_data$Date)
  # Extracting the year from the date
  nse_share_data$Year <- year(nse_share_data$Date)
  # Tidy the money credit data to replace the months in figures with words
  nse_share_data$Month <- ifelse(nse_share_data$Month == 1, "Jan", 
                                 ifelse(nse_share_data$Month == 2, "Feb", 
                                        ifelse(nse_share_data$Month == 3, "Mar", 
                                               ifelse(nse_share_data$Month == 4, "Apr", 
                                                      ifelse(nse_share_data$Month == 5, "May", 
                                                             ifelse(nse_share_data$Month == 6, "Jun", 
                                                                    ifelse(nse_share_data$Month == 7, "Jul", 
                                                                           ifelse(nse_share_data$Month == 8, "Aug", 
                                                                                  ifelse(nse_share_data$Month == 9, "Sep", 
                                                                                         ifelse(nse_share_data$Month == 10, "Oct", 
                                                                                                ifelse(nse_share_data$Month == 11, "Nov", "Dec")))))))))))
  # Parse the vol. column from string to numeric
  nse_share_data$Vol. <- parse_number(nse_share_data$Vol.)
  nse_share_data$`Change %` <- parse_number(nse_share_data$`Change %`)
  # Dropping of specific columns that are not useful
  nse_share_data$Date <- NULL
  nse_share_data$Vol. <- NULL
  nse_share_data <- nse_share_data %>% group_by(Month, Year) %>% summarise_all(mean)
  # Creating a new column for quarters
  nse_share_data$Quarter <- ifelse(nse_share_data$Month %in% c("Jan", "Feb", "Mar"), "Q1", 
                                   ifelse(nse_share_data$Month %in% c("Apr", "May", "Jun"), "Q2", 
                                          ifelse(nse_share_data$Month %in% c("Jul", "Aug", "Sep"), "Q3", "Q4")))
  
# Create a new dataframe to summarise the volume of total shares traded
  vol_shares_data <- nse_share_data[c(1:577, 736:1260), ]
  # Replace NA values with zero
  vol_shares_data$Vol. <- replace_na(vol_shares_data$Vol., 0)
  # Summarise the volumes of shares traded by month and year
  vol_shares_data <- vol_shares_data %>% group_by(Month, Year) %>% 
    summarise_all(mean) %>% select(-Date)
  # Create a new column called Quarter
  vol_shares_data$Quarter <- ifelse(vol_shares_data$Month %in% c("Jan", "Feb", "Mar"), "Q1", 
                                    ifelse(vol_shares_data$Month %in% c("Apr", "May", "Jun"), "Q2", 
                                           ifelse(vol_shares_data$Month %in% c("Jul", "Aug", "Sep"), "Q3", "Q4")))
  # Filter out and save the years of focus for the analysis
  vol_shares_data <- vol_shares_data %>% filter(Year %in% c("2018", "2019", "2020", "2021"))
 
# Creating a new column for quarters in the money credit data
money_credit_data$Quarter <- ifelse(money_credit_data$Month %in% c("Jan", "Feb", "Mar"), "Q1", 
                                    ifelse(money_credit_data$Month %in% c("Apr", "May", "Jun"), "Q2", 
                                           ifelse(money_credit_data$Month %in% c("Jul", "Aug", "Sep"), "Q3", "Q4")))

# Joining the money credit data with the vol gdp data
l <- full_join(vol_gdp_data, money_credit_data, by = c("Quarter", "Year", "Month"))

# Tidy the growth rate data and join them all together
  # Tidy first_growth_rate
  first_growth_rate <- first_growth_rate[1, ]
  col_names <- c("Item", "Q1 2017", "Q2 2017", "Q3 2017", "Q4 2017", "Total 2017", "Q1 2018", "Q2 2018", "Q3 2018", "Q4 2018", "Total 2018", "Final")
  colnames(first_growth_rate) <- col_names
  first_growth_rate$`Total 2017` <- NULL
  first_growth_rate$`Total 2018` <- NULL
  first_growth_rate$Final <- NULL
  # Tidy second_growth_rate
  second_growth_rate <- second_growth_rate[1, ]
  col_names <- c("Item", "Q1 2019", "Q2 2019", "Q3 2019", "Q4 2019", "Total 2019", "Q1 2020")  
  colnames(second_growth_rate) <- col_names
  second_growth_rate$`Total 2019` <- NULL
  second_growth_rate$`Q1 2020` <- NULL
  # Tidy third_growth_rate
  third_growth_rate <- third_growth_rate[1, ]
  col_names <- c("Item", "Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020", "Total 2020", "Q1 2021", "Q2 2021")  
  colnames(third_growth_rate) <- col_names  
  # Join the three tables together
  gdp_growth_rates <- full_join(full_join(first_growth_rate, second_growth_rate, by = "Item"), third_growth_rate, by = "Item")
  # Gather the data into 3 columns
  gdp_growth_rates <- gather(gdp_growth_rates, key = "Period", value = "GDP Growth Rates (%)", 2:20)
  # Remove the rows with "Total 2020"
  gdp_growth_rates <- gdp_growth_rates[-17, ]
  # Separate the Period column into quarter and year
  gdp_growth_rates <- separate(gdp_growth_rates, col = "Period", into = c("Quarter", "Year"), sep = " ")
  # Drop the Item Column
  gdp_growth_rates$Item <- NULL
  # Round the growth rate figures to 2 decimal places
  gdp_growth_rates$`GDP Growth Rates (%)` <- round(gdp_growth_rates$`GDP Growth Rates (%)`, 2)
  # Convert the Year column to numeric from character
  gdp_growth_rates$Year <- as.numeric(gdp_growth_rates$Year)
  # Join the table with the summarized volume of shares traded with the gdp growth rates table
  vol_gdp_data <- full_join(vol_shares_data, gdp_growth_rates, by = c("Quarter" = "Quarter", "Year" = "Year"))
  # Change the class of % change of gdp growth rate to numeric
  vol_gdp_data$`Change %` <- round(vol_gdp_data$`Change %`, 2)
  # Filter out and save rows without NAs
  vol_gdp_data <- vol_gdp_data %>% filter(across(everything(), ~!is.na(.)))
  
# Investigate the relationship among the parameters by visuals
  # Trend of total volume of shares traded across 2019 to 2021
    vol_shares_data %>% group_by(Quarter, Year) %>% ggplot(aes(x = Quarter, y = Vol., fill = factor(Year))) + 
      geom_col(position = "dodge") + theme(legend.title = element_blank()) + 
      labs(y = "Volume of Shares Traded", title = "Volume of Shares Traded every Quarter between 2018 and 2021")
    
  # Trend of GDP Growth Rates for every quarter across 2018 through to 2021
    gdp_growth_rates %>% group_by(Quarter, Year) %>% ggplot(aes(x = Quarter, y = `GDP Growth Rates (%)`, fill = factor(Year))) + 
      geom_col(position = "dodge") + labs(title = "GDP Growth Rates for every Quarter between 2018 and 2021") + 
      theme(legend.title = element_blank())
    
  # Average closing price for each month across the years
    vol_shares_data %>% ggplot(aes(x = Month, y = Price, group = Year)) + geom_point() + geom_line() + 
      facet_wrap(vars(Year), scales = "free")
    l %>% filter(Year %in% c(2018, 2019, 2020, 2021)) %>% plot_ly(x = ~`Currency in Circulation`, y = ~Vol., color = ~factor(Year)) %>% add_markers(opacity = 5.0)
    
  # Relationship between gdp growth and vol of shares traded
    l %>% filter(Year %in% c(2018,2019,2020,2021)) %>% 
      plot_ly(x = ~`GDP Growth Rates (%)`, y = ~Vol., color = ~factor(Year)) %>% 
      add_markers(sizes = 100) %>% layout(title = "Relationship Between GDP Growth Rate and Volume of Shares Traded", yaxis = list(title = "Volume of Shares Traded"))
    
  # Relationship between credit to private sector and vol of shares traded
    l %>% filter(Year %in% c(2018,2019,2020,2021)) %>% plot_ly(x = ~ `Credit to Private Sector`, y = ~Vol., color = ~factor(Year)) %>% 
      add_markers()
    
  # Relationship between credit to private sector and the highest pricing of shares
    l %>% filter(Year %in% c(2018,2019,2020,2021)) %>% plot_ly(x = ~`Credit to Private Sector`, y = ~High, color = ~factor(Year)) %>% 
      add_markers()
    
  # Relationship between Net Domestic Credit on vol of shares traded
    l %>% filter(Year %in% c(2018,2019,2020,2021)) %>% plot_ly(x = ~`Net Domestic Credit`, y = ~Vol., color = ~factor(Year)) %>% 
      add_markers() %>% layout(title = "Relationship Between Net Domestic Credit and Volume of Shares Bought", yaxis = list(title = "Volumes of Shares Bought Per Month"))
    
  # Relationship between Net Domestic Credit on highest value of shares
    q <- l %>% filter(Year %in% c(2018,2019,2020,2021)) %>% plot_ly(x = ~`Net Domestic Credit`, y = ~High, color = ~factor(Year)) %>% 
      add_markers() %>% layout(title = "Relationship Between Net Domestic Credit and Price of Shares", yaxis = list(title = "Highest Prices of Shares Per Month"))
      