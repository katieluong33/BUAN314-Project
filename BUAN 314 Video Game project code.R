library(tidyverse)
library(sqldf)
df<-read.csv('https://raw.githubusercontent.com/katieluong33/BUAN314-Project/refs/heads/main/vgsales.csv?token=GHSAT0AAAAAACZU5YMSGHTOT7L3YT2QA3BKZ24RGCQ')
post<- df %>%
  select(Rank,Name, Year, Platform, Genre, Publisher, NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales) %>%
  filter(Year>=2000)

#histogram of the publishers with the most games
top_publishers <- post %>%
  count(Publisher) %>%
  arrange(desc(n)) %>%
  head(15)
print(top_publishers)

#how many games does nintnedo publish each year
ninfreq <- post %>%
  filter(Publisher == "Nintendo") %>%
  group_by(Year) %>%
  summarise(frequency = n (), .groups = "drop")
ninfreq2$Year <- as.numeric(ninfreq2$Year)
ninfreq2<- ninfreq %>%
  na.omit()
str(ninfreq2)
#how many games does EA publish each year
EAfreq <- post %>%
  filter(Publisher == "Electronic Arts") %>%
  group_by(Year) %>%
  summarise(frequency = n (), .groups = "drop")
EAfreq$Year <- as.numeric(EAfreq$Year)
str(EAfreq2)
EAfreq2 <- EAfreq %>%
  na.omit()

eaplot <- ggplot(EAfreq2, aes(x = Year, y = frequency)) +
  geom_line()

#Activision code
Actfreq <- post %>%
  filter(Publisher == "Activision") %>%
  group_by(Year) %>%
  summarise(frequency = n (), .groups = "drop")
Actfreq$Year <- as.numeric(Actfreq$Year)
str(Actfreq)
Actfreq2 <- Actfreq %>%
  na.omit()

#Top publishers from 2000-2017 bar graph
ggplot() +
  geom_line(data = ninfreq2, aes(x = Year, y = frequency, color = "Nintendo"), size = 1) +  # Line for ninfreq2
  geom_line(data = EAfreq2, aes(x = Year, y = frequency, color = "Electronic Arts"), size = 1) +    # Line for EAfreq2
  geom_line(data = Actfreq2, aes(x = Year, y = frequency, color = "Activision"), size = 1) +
  scale_color_manual(values = c("Nintendo" = "red", "Electronic Arts" = "black", "Activision" = "blue")) +  
  labs(title = "Games published from 2000-2017", 
       x = "Year", 
       y = "# of Games", 
       color = "Publisher")  # Title for the legend

#SQL for number of games by publisher
num_pub <- 'SELECT Publisher, COUNT (*) AS Games_Published
FROM post
GROUP BY Publisher
ORDER BY Games_Published DESC
LIMIT 10'
sqldf(num_pub)

#SQL for the above graph
query23 <- 'SELECT publisher, SUM(global_sales) AS total_global_sales
FROM post
WHERE publisher IN ("Activision","Electronic Arts", "Nintendo")
GROUP BY publisher'
sqldf(query23)


#portion of sales that each continent makes up from 2000-2020
#Part 1: pull only sales by continent and sum
continent_sales <- post %>%
  select(NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>%
  summarize(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Continent", values_to = "Sales")

#percentages from each continent
continent_sales <- continent_sales %>%
  mutate(percentage = Sales / sum(Sales) * 100)

# Pie chart with percentages
ggplot(continent_sales, aes(x = "", y = Sales, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sales Distribution by Continent from 2000-2020") +
  scale_fill_discrete(
    labels = c("Europe", "Japan", "North America", "Other")
  ) +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),  
            position = position_stack(vjust = 0.5))

#sql each continent percentage make-up of global sales
Query33<- 'SELECT Continent, percentage
FROM continent_sales
ORDER BY percentage DESC'
sqldf(Query33)

#plot 3 - sales over the years by continent 
continentsales_byyear<- post %>%
  select(Year, NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>%  
  group_by(Year) %>%
  summarize(
    NA_Sales = sum(NA_Sales, na.rm = TRUE),  
    EU_Sales = sum(EU_Sales, na.rm = TRUE), 
    JP_Sales = sum(JP_Sales, na.rm = TRUE), 
    Other_Sales = sum(Other_Sales, na.rm = TRUE),
  ) %>% replace_na(list(Sales = 0)) 
continentsales_byyear <- continentsales_byyear[-20,]


sales_long <- continentsales_byyear %>%
  pivot_longer(cols = c(NA_Sales, EU_Sales, JP_Sales, Other_Sales),  
               names_to = "Region", values_to = "Sales")  


ggplot(sales_long, aes(x = Year, y = Sales, color = Region, group = Region)) +
  geom_line(size = 1) +                 
  labs(
    title = "Video Game Sales by Region Over Time",
    x = "Year",
    y = "Total Sales (millions)",
    color = "Region"
  ) +
  theme_minimal() +                    
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 6))    


#plot 4 - bar of genre sales in NA 
#dataframe
genre_na <- post %>%
  group_by(Genre) %>%
  summarise(total_na_sales = sum(NA_Sales)) %>%
  arrange(desc(total_na_sales)) %>%
  head(5) %>%
  mutate(color_map = case_when(
    Genre == "Action" ~ "red",          
    Genre == "Role-Playing" ~ "green", 
    Genre == "Shooter" ~ "blue",        
    Genre == "Misc" ~ "black",           
    Genre == "Sports" ~ "orange",      
    TRUE ~ "gray"                      
  ))

# bar plot 
ggplot(genre_na, aes(x = Genre, y = total_na_sales, fill = color_map)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(title = "Total NA Sales by Genre", 
       x = "Genre", 
       y = "Total NA Sales") +
  scale_fill_identity()  #color from map


#split table and join again
Vg_info <- post %>%
  select(Rank, Name, Year, Platform, Genre, Publisher)
vg_sales <- post %>%
  select(Rank,NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales)
#check for duplicates
sum(duplicated(vg_sales$Rank))
sum(duplicated(Vg_info$Rank))

#join the tables using inner join on Rank
table <- 'SELECT *
FROM Vg_info AS t1
INNER JOIN vg_sales AS t2
ON t1.Rank = t2.Rank'
sqldf(table)

#######################################Thatcher#########################

#GRAPH NUMBER 1
# Filter data for the three periods
data_2000_2005 <- subset(post, Year >= 2000 & Year <= 2005)
data_2006_2010 <- subset(post, Year >= 2006 & Year <= 2010)
data_2011_2015 <- subset(post, Year >= 2011 & Year <= 2015)

#top 3 platforms for each period
top_platforms_2000_2005 <- head(sort(table(data_2000_2005$Platform), decreasing = TRUE), 3)
top_platforms_2006_2010 <- head(sort(table(data_2006_2010$Platform), decreasing = TRUE), 3)
top_platforms_2011_2015 <- head(sort(table(data_2011_2015$Platform), decreasing = TRUE), 3)

# Filter
top_data_2000_2005 <- subset(data_2000_2005, Platform %in% names(top_platforms_2000_2005))
top_data_2006_2010 <- subset(data_2006_2010, Platform %in% names(top_platforms_2006_2010))
top_data_2011_2015 <- subset(data_2011_2015, Platform %in% names(top_platforms_2011_2015))

# Sum
sales_2000_2005 <- aggregate(Global_Sales ~ Platform, data = top_data_2000_2005, sum)
sales_2006_2010 <- aggregate(Global_Sales ~ Platform, data = top_data_2006_2010, sum)
sales_2011_2015 <- aggregate(Global_Sales ~ Platform, data = top_data_2011_2015, sum)

# Combine the data
sales_2000_2005$Period <- "2000-2005"
sales_2006_2010$Period <- "2006-2010"
sales_2011_2015$Period <- "2011-2015"
plot_data <- rbind(sales_2000_2005, sales_2006_2010, sales_2011_2015)

ggplot(plot_data, aes(x = Platform, y = Global_Sales, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top Platforms Comparison (Global Sales)", 
       x = "Platform", y = "Global Sales (in millions)") +
  theme_minimal()

# GRAPH NUMBER 2 
# Get the top 10 platforms based on global sales
top_platforms <- head(sort(table(post$Platform), decreasing = TRUE), 10)

# Filter for the top platforms
top_data <- subset(post, Platform %in% names(top_platforms))

# Calculate market share for the top 10 platforms
market_share <- top_data %>%
  group_by(Platform) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>%
  mutate(Share = Total_Sales / sum(Total_Sales) * 100)

ggplot(market_share, aes(x = "", y = Share, fill = Platform)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Market Share by Platform (Top 10 Platforms)") +
  theme_void()

#GRAPH NUMBER 3
# Summarize sales by platform and region
platform_sales <- post %>%
  group_by(Platform) %>%
  summarize(NA_Sales = sum(NA_Sales, na.rm = TRUE),
            EU_Sales = sum(EU_Sales, na.rm = TRUE),
            JP_Sales = sum(JP_Sales, na.rm = TRUE))

# Find the top 3 platforms in each region
top_na <- platform_sales %>% arrange(desc(NA_Sales)) %>% head(3)
top_eu <- platform_sales %>% arrange(desc(EU_Sales)) %>% head(3)
top_jp <- platform_sales %>% arrange(desc(JP_Sales)) %>% head(3)

# Combine into one data frame
top_platforms <- bind_rows(top_na, top_eu, top_jp) %>% distinct()

# Create a bar chart
ggplot(top_platforms, aes(x = Platform)) +
  geom_bar(aes(y = NA_Sales, fill = "NA Sales"), stat = "identity") +
  geom_bar(aes(y = EU_Sales, fill = "EU Sales"), stat = "identity") +
  geom_bar(aes(y = JP_Sales, fill = "JP Sales"), stat = "identity") +
  scale_fill_manual(values = c("NA Sales" = "darkcyan", "EU Sales" = "burlywood", "JP Sales" = "dimgray")) +
  labs(title = "Top 3 Most Popular Platforms in Each Region",
       x = "Platform",
       y = "Sales (in millions)",
       fill = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#GRAPH NUMBER 4 
# Summarize total sales by platform for NA and EU
platform_sales <- post %>%
  group_by(Platform) %>%
  summarize(NA_Sales = sum(NA_Sales, na.rm = TRUE),
            EU_Sales = sum(EU_Sales, na.rm = TRUE),
            Total_Sales = sum(NA_Sales, EU_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 10)

# line graph with platform and comparison of country sales between EU and NA
ggplot(platform_sales, aes(x = Platform)) +
  geom_line(aes(y = NA_Sales, color = "NA Sales"), size = 1) +
  geom_line(aes(y = EU_Sales, color = "EU Sales"), size = 1) +
  geom_point(aes(y = NA_Sales, color = "NA Sales"), size = 2) +
  geom_point(aes(y = EU_Sales, color = "EU Sales"), size = 2) +
  labs(title = "Comparison of Sales between NA and EU by Top 10 Platforms",
       x = "Platform",
       y = "Sales (in millions)",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#SQL queeries for GRAPH 1
#this information 

library(sqldf)

Q1.1 <- "SELECT Platform, SUM(Global_Sales) AS Total_Sales, '2000-2005' AS Period
FROM data_2000_2005
WHERE Year BETWEEN 2000 AND 2005
GROUP BY Platform
ORDER BY Total_Sales DESC
LIMIT 3"
sqldf(Q1.1)

Q1.2 <- "SELECT Platform, SUM(Global_Sales) AS Total_Sales, '2006-2010' AS Period
FROM data_2006_2010
WHERE Year BETWEEN 2006 AND 2010
GROUP BY Platform
ORDER BY Total_Sales DESC
LIMIT 3"
sqldf(Q1.2)

Q1.3 <- "SELECT Platform, SUM(Global_Sales) AS Total_Sales, '2011-2015' AS Period
FROM data_2011_2015
WHERE Year BETWEEN 2011 AND 2015
GROUP BY Platform
ORDER BY Total_Sales DESC
LIMIT 3"
sqldf(Q1.3)

Q2.1 <- "SELECT
Platform,
SUM(NA_Sales) AS NA_Sales
FROM post
GROUP BY Platform
ORDER BY NA_Sales DESC
LIMIT 10"
sqldf(Q2.1)

Q2.2 <- "SELECT
Platform,
SUM(EU_Sales) AS EU_Sales
FROM post
GROUP BY Platform
ORDER BY EU_Sales DESC
LIMIT 10"
sqldf(Q2.2)

Q2.3 <- "SELECT
Platform,
SUM(JP_Sales) AS JP_Sales
FROM post
GROUP BY Platform
ORDER BY JP_Sales DESC
LIMIT 10"
sqldf(Q2.3)

Q3 <- "SELECT *
    FROM platform_sales
    ORDER BY Total_Sales DESC
    LIMIT 10"
sqldf(Q3)

###############Alissa############################
vg_filtered <-vgsales %>%
  filter(Year>= 2000 & Year <= 2017,
         Genre %in% c("Action", "Role-Playing","Shooter")) %>%
  filter(!is.na(Global_Sales))
#violin plot graph 1
ggplot(vg_filtered, aes(x = Genre,y = Global_Sales)) +
  geom_violin(fill = "skyblue", trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.5, fill = "hotpink") + 
  scale_y_log10() #logscale to see distribution better
theme_minimal() +
  labs(title = "Global Sales by Genre (2000-2017)",
       x = "Genre",
       y = "Global Sales (millions)")

#graph 2
#bar chart (games be Genre compared to Platform) 
library(dplyr)
modern_games <- vgsales %>%
  filter(Year>= 2000 & Year <= 2017) %>%
  filter(Platform %in% c("Wii", "DS", "X360")) %>% #(picked 3 platforms)
  group_by(Genre,Platform) %>%
  summarise(Count = n()) %>%
  #proportion of each count value relative to total
  mutate(Percentage = Count /sum(Count) * 100)  #mutate=create column(percentage) 
ggplot(modern_games, aes(x = Genre, y = Count, fill = Platform)) +  
  geom_bar(stat = "identity") +
  labs(title = "Games by Genre and Platform",
       x = "Genre",
       y = "Number of Games") +
  theme_minimal()


# graph 3
vgsales <- read.csv("https://raw.githubusercontent.com/katieluong33/BUAN314-Project/refs/heads/main/vgsales.csv?token=GHSAT0AAAAAACY67TNRJGMYPMP2KNXNQ53WZ2SCTIA")


filtered_data <- vgsales %>%
  filter(Year >= 2000 & Year <= 2017, Genre %in% c("Action", "Sports", "Shooter"))
grouped_data <- filtered_data %>%
  group_by(Genre, Year) %>%
  summarize(TotalNASales = sum(NA_Sales, na.rm = TRUE))

ggplot(grouped_data, aes(x = Year, y = TotalNASales, color = Genre, group = Genre)) +
  geom_line(size = 1) +  # Ensure lines are plotted
  labs(title = "Popularity of Genres in NA Sales Over Time",
       x = "Year",
       y = "NA Sales (in millions)") +
  theme_minimal()


#scatterplot graph 4 ->
ggplot(data = post, aes(x = Year, y =Global_Sales)) +
  geom_point()
#lineplot ->
ggplot(data = post, aes(x = Year, y = Global_Sales)) +
  geom_line()
scale_x_continuous(limits = c(2000, 2010)) #limit years




SQL:Query1 <- "
  SELECT Genre, Year, SUM(Global_Sales) AS TotalGlobalSales
  FROM vgsales
  WHERE Year BETWEEN 2000 AND 2017
    AND Genre IN ('Action', 'Role-Playing', 'Shooter')
  GROUP BY Genre, Year
  ORDER BY TotalGlobalSales DESC
  LIMIT 10;
  "
result <- sqldf(Query1)
print(result)


library(dplyr)
library(sqldf)


vgsales <- read.csv('https://raw.githubusercontent.com/katieluong33/BUAN314-Project/refs/heads/main/vgsales.csv?token=GHSAT0AAAAAACY67TNRNWTRMKFQV5LNB3ZEZ24UL3A')
Query2 <- "
  SELECT  Genre,  SUM(Global_Sales) AS TotalGlobalSales
  FROM vgsales
  GROUP BY  Genre
  ORDER BY TotalGlobalSales DESC
  LIMIT 5;
"
result <- sqldf(Query2)
print(result)

library(sqldf)

Query3 <- "
  SELECT  Year,  Global_Sales
  FROM  vgsales
  WHERE Year >= 2000
  ORDER BY Year ASC
  LIMIT 10;
"
result <- sqldf(Query3)
print(result)

Query4 <- "
SELECT Name, Global_Sales
FROM vgsales
WHERE Genre = 'Action'
ORDER BY Global_Sales DESC
LIMIT 10;
"
result <-sqldf(Query4)
print(result)


