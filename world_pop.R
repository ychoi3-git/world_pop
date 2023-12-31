# Two datasets were downloaded from datacamp.
# World_Pop data has the total population numbers for every country each year from 1960 to 2020. 
# Additionally, metadata_country table contains country information, including region, income group, and any special notes.

#load tidyverse package
suppressPackageStartupMessages(library(tidyverse))

# 1. Pivot world_pop_data and combine two data.
# load and pivot world_pop_data
world_pop <- read_csv('data/world_pop_data.csv.gz', show_col_types = FALSE)
head(world_pop)

new_world_pop <- world_pop %>% 
  pivot_longer('1960':'2020', names_to = "year", values_to = "total_pop") %>% 
  mutate(year = as.integer(year)) %>% 
  select(-'Indicator Name', -'Indicator Code')
head(new_world_pop)

# load metadata_country data 
metadata <- read_csv('data/metadata_country.csv.gz', show_col_types = FALSE)
head(metadata)

# check the elements of Country Code variable which are not shared between two data. 
length(unique(new_world_pop$`Country Code`))
length(unique(metadata$'Country Code'))
new_world_pop %>% anti_join(metadata, by = "Country Code")

# combine two data
one.table <- merge(new_world_pop, metadata, by = "Country Code", all.y = TRUE)
head(one.table)


# 2. Which countries have experienced the highest population growth from 1960 to 2020?

# organize the combined table.
pop <- one.table %>%
  select(-SpecialNotes) %>%
  arrange(TableName, year)
head(pop)

# find top 10 countries with the highest population growth from 1960 to 2020.
pop_growth <- pop %>%
  group_by(TableName) %>%
  filter(year %in% c(1960, 2020)) %>%
  summarize(population_growth = ((total_pop[year == 2020] - total_pop[year == 1960])/total_pop[year == 1960]*100)) %>%
  arrange(desc(population_growth)) %>% 
  slice_max(population_growth, n=10) 
head(pop_growth)

# visualization of top 10 countries
p <- pop_growth %>% 
  mutate(TableName = reorder(TableName, desc(population_growth))) %>% 
  ggplot(aes(TableName, population_growth, fill = TableName)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Country") + 
  ylab("population_growth % (yr2020/yr1960)") +
  ggtitle("Top 10 countries with the highest population growth from year 1960 to year 2020") +
  theme(legend.position="none") 
p

# 3. compare average population growth rate for the last 10 years among countries.

# filter out from year 2011 to year 2020 and create two variables representing the difference of population every year
# and population growth rate, respectively, and summarize average growth rate of each country.
ave_pop_growth <- pop %>% 
  group_by(TableName) %>%
  filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  mutate(diff_pop = total_pop -lag(total_pop), 
         growth_rate = diff_pop / lag(total_pop)*100) %>% 
  summarize(ave_growth_rate = mean(growth_rate, na.rm = TRUE))
head(ave_pop_growth)

# Select 10 countries with highest population growth, and 10 with lowest population growth.
top_ten_countries <- ave_pop_growth %>%  slice_max(ave_growth_rate, n =10)
bottom_ten_countries <- ave_pop_growth %>%  slice_min(ave_growth_rate, n =10)
top_ten_countries
bottom_ten_countries

#Visualization
top_10 <- top_ten_countries %>% 
  mutate(TableName = reorder(TableName, desc(ave_growth_rate))) %>% 
  ggplot(aes(TableName, ave_growth_rate, fill = TableName)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Country") + 
  ylab("population growth rate % (2011-2020)") +
  ggtitle("Top 10 countries with the highest population growth from 2011 to 2020") +
  theme(legend.position="none") 
top_10

bottom_10 <- bottom_ten_countries %>% 
  mutate(TableName = reorder(TableName, ave_growth_rate)) %>% 
  ggplot(aes(TableName, ave_growth_rate, fill = TableName)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Country") + 
  ylab("population growth rate % (2011-2020)") +
  ggtitle("10 countries with the lowest population growth from 2011 to 2020") +
  theme(legend.position="none") 
bottom_10

# 4. Create a plot that visualizes the population growth of countries over time grouped by region.

#group the dataframe by Region and year and create a variable showing average of total population each year by region
pop_growth_region <- pop %>%
  filter(!is.na(Region)) %>% 
  group_by(Region, year) %>% 
  mutate(ave_pop = mean(total_pop)/1000000)
head(pop_growth_region)

pop_growth_region %>% ggplot(aes(year, ave_pop, color = Region)) + 
  geom_line() +
  ylab("population in millons") +
  ggtitle("Population growth over time by Region ")


# 5. Which countries in South Asia show highest population growth over time?

south_asia <- pop %>%
  filter(Region == "South Asia") %>% 
  group_by(TableName) %>% 
  mutate(total_pop = total_pop/1000000)
head(south_asia)

south_asia_plot <- south_asia %>% ggplot(aes(year, total_pop, color = TableName)) + 
  geom_line() +
  ylab("population in millons") +
  labs(color = "country") +
  ggtitle("Population growth over time in South Asia ")
south_asia_plot

# 6. Which countries in Middle East & North Africa show the highest population growth over time?

middle_east_north_africa <- pop %>%
  filter(Region == "Middle East & North Africa") %>% 
  group_by(TableName) %>% 
  mutate(total_pop = total_pop/1000000)
head(middle_east_north_africa)

middle_east_north_africa_plot <- middle_east_north_africa %>% ggplot(aes(year, total_pop, color = TableName)) + 
  geom_line() +
  ylab("population in millons") +
  ggtitle("Population growth over time in Middle East & North Africa ") +
  labs(color = "Country")
middle_east_north_africa_plot 

# 7. How does income group affect a country's population growth?

pop_growth_by_income <- pop %>%
  filter(!is.na(IncomeGroup)) %>%
  group_by(IncomeGroup, year) %>% 
  mutate(ave_pop_by_income = mean(total_pop, na.rm = TRUE)/1000000)
head(pop_growth_by_income)

pop_growth_by_income %>% 
  ggplot(aes(year, ave_pop_by_income, color = IncomeGroup)) + 
  geom_line() + 
  ylab("population in millions") +
  ggtitle("Population growth over time by income")

