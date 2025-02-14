rm(list=ls())

library(tidyverse)
library(lubridate)


# Last inn data
url <- "https://raw.githubusercontent.com/uit-sok-1005-v23/uit-sok-1005-v23.github.io/main/storedata.csv"
data <- read.csv(url)

# Legg til Year og Month
data <- data %>%
  mutate(Year = year(as.Date(Order_Date, format = "%Y-%m-%d")),
         Month = month(as.Date(Order_Date, format = "%Y-%m-%d"), label = TRUE))

# task 1 oppgave 1 

table1 <- data %>%
  filter(Year == 2017, Month %in% c("Oct", "Nov", "Dec"), Region %in% c("Region 1", "Region 9")) %>%
  group_by(Month, Region, Customer_Segment) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(Region, Month)


# task 1 oppgave 2 

# Filtrer og grupper data
plot_data <- data %>%
  filter(Year %in% c(2015, 2016, 2017) & Region %in% c("Region 1", "Region 13")) %>%
  group_by(Year, Month, Region) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  mutate(Month = factor(Month, levels = month.abb))  # Oppdater Month-rekkefølge

# Lag plottet
ggplot(plot_data, aes(x = Month, y = Total_Sales, color = Region, group = Region)) +
  geom_line() +
  facet_wrap(~ Year) +
  labs(title = "Total Sales per Month in Region 1 and Region 13 (2015-2017)",
       x = "Month", y = "Total Sales", color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# task 1 oppgave 3 

table2 <- plot_data %>%
  spread(key = Region, value = Total_Sales) %>%
  filter(`Region 13` > `Region 1`) %>%
  select(Year, Month, `Region 1`, `Region 13`)


# task 1 oppgave 4 

table3 <- data %>%
  filter(Year == 2017, !Region %in% c("Region 3", "Region 5", "Region 8")) %>%
  group_by(Customer_Segment, Product_Category) %>%
  summarise(Average_Profit = mean(Profit, na.rm = TRUE)) %>%
  arrange(desc(Average_Profit)) 




# task 2 

library(rvest)

url <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"
webpage <- read_html(url)

# Finn tabellen på siden
table_raw <- html_table(html_nodes(webpage, "table")[[1]])

# her ryddyer vi opp datasettet 
table_clean <- table_raw %>%
  rename(
    Model = X1,
    WLTP = X2,
    STOP = X3,
    Deviation = X4
  ) %>%
  mutate(
    WLTP = as.numeric(gsub(" km.*", "", WLTP)),  # Fjern "km" og konverter til numerisk
    STOP = as.numeric(gsub(" km.*", "", STOP)),  # Fjern "km" og konverter til numerisk
    Deviation = as.numeric(gsub(" %", "", Deviation))  # Fjern "%" og konverter til numerisk
  )



# task 2 oppgave a 

library(ggplot2)

# Lag plottet
ggplot(table_clean, aes(x = WLTP, y = STOP)) +
  geom_point(color = "darkgreen", size = 2) +  # Plott datapunktene
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 45-graders linje
  labs(
    title = "Comparison of WLTP and STOP Ranges for Electric Cars",
    x = "WLTP Range (km)",
    y = "STOP Range (km)",
    caption = "The red dashed line represents the ideal case where WLTP equals STOP."
  ) +
  theme_minimal()


# task 2 oppgave b 

model <- lm(STOP ~ WLTP, data = table_clean)  # Lag lineær modell
summary(model)  # Oppsummer modellen

ggplot(table_clean, aes(x = WLTP, y = STOP)) +
  geom_point(color = "darkgreen", size = 2) +  # Plott datapunktene
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 45-graders linje
  geom_smooth(method = "lm", color = "blue") +  # Lineær regresjonslinje
  labs(
    title = "Comparison of WLTP and STOP Ranges for Electric Cars",
    x = "WLTP Range (km)",
    y = "STOP Range (km)",
    caption = "The red dashed line represents the ideal case where WLTP equals STOP.
               The blue line is the linear regression line."
  ) +
  theme_minimal()


