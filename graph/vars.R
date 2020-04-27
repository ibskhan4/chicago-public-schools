library(readxl)
library(tidyverse)
library(janitor)  
library(gt)
library(dplyr)
library(rvest)
library(ggplot2)
library(sf)
library(broom)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)

# Billboard data source: https://data.world/kcmillersean/billboard-hot-100-1958-2017

# Load in billboard weekly top 100 songs rank data

e2007 <- read_csv('data/enrollment_2007.csv', col_types = cols(
  Area = col_double(),
  Network = col_character(),
  `School ID` = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2008 <- read_csv('data/enrollment_2008.csv', col_types = cols(
  Area = col_double(),
  Network = col_character(),
  `School ID` = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2009 <- read_csv('data/enrollment_2009.csv', col_types = cols(
  Area = col_double(),
  Network = col_character(),
  `School ID` = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2010 <- read_csv('data/enrollment_2010.csv', col_types = cols(
  Area = col_double(),
  Network = col_character(),
  `School ID` = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2011 <- read_csv('data/enrollment_2011.csv', col_types = cols(
  Area = col_double(),
  `School ID` = col_double(),
  Network = col_character(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2012 <- read_csv('data/enrollment_2012.csv', col_types = cols(
  Network = col_character(),
  `School ID` = col_double(),
  Area = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2013 <- read_csv('data/enrollment_2013.csv', col_types = cols(
  Network = col_character(),
  `School ID` = col_double(),
  Area = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2014 <- read_csv('data/enrollment_2014.csv', col_types = cols(
  Network = col_character(),
  `School ID` = col_double(),
  Area = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2015 <- read_csv('data/enrollment_2015.csv', col_types = cols(
  Network = col_character(),
  `School ID` = col_double(),
  Area = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double()))  

e2016 <- read_csv('data/enrollment_2016.csv', col_types = cols(
  Network = col_character(),
  `School ID` = col_double(),
  Area = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2017 <- read_csv('data/enrollment_2017.csv', col_types = cols(
  Network = col_character(),
  `School ID` = col_double(),
  Area = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2018 <- read_csv('data/enrollment_2018.csv', col_types = cols(
  Network = col_character(),
  `School ID` = col_double(),
  Area = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double())) 

e2019 <- read_csv('data/enrollment_2019.csv', col_types = cols(
  `School ID` = col_double(),
  School = col_character(),
  Total = col_number(),
  Year = col_double(),
  Area = col_double(),
  Network = col_character())) 

aid <- read_csv('data/aid_data.csv', col_types = cols(
  year = col_double(),
  federal_aid = col_double(),
  state_aid = col_double(),
  total_expenses = col_double(),
  gap = col_double()))

dropout <- read_csv('data/dropout_data.csv', col_types = cols(
  group = col_character(),
  rate = col_double(),
  total_dropout = col_double(),
  year = col_double()))

enrollment <- e2007 %>% 
  bind_rows(e2008, e2009, e2010, e2011, e2012, e2013, e2014, e2015, e2016, e2017, e2018, e2019)

school2 <- read_csv('data/schools.csv', skip = 1,
                    na = c("undefined", ""),
                    col_types = cols(
                      school = col_double(),
                      address = col_character(),
                      zip = col_character(),
                      year = col_double(),
                      type = col_character(),
                      use = col_character(),
                      '2_plus' = col_double(),
                      private = col_double(),
                      sold = col_character(),
                      buyer = col_character(),
                      price = col_character())) 

budget <- read_csv('data/budget_data.csv')

locs <- read_csv('data/CPS_School_Locations_SY1415.csv')

graph <- school2 %>% 
  group_by(year) %>% 
  filter(type == "school closing") %>% 
  count(type) %>% 
  ggplot(aes(year, n)) +
  geom_line() +
  labs(x = "Year", 
       y = "Number of School Closings", 
       title = "Number of School Closings in CPS from 2002-2018") +
  scale_x_continuous(breaks = seq(2002, 2018, by = 1)) +
  theme_light() +
  theme(plot.title = element_text(face = "bold", color = "#063376", size = 20))


school_join <- school2 %>% 
  group_by(year) %>% 
  filter(type == "school closing") %>% 
  filter(year > 2006 && year < 2019) %>% 
  count(type) 

ejoin <- enrollment %>% 
  filter(!is.na(Total)) %>% 
  group_by(Year) %>% 
  summarize(sum = sum(Total)) %>% 
  rename("year" = Year)

school_e_join <- ejoin %>%
  inner_join(school_join, by = "year") 

# Fit regression model:

correlation <- school_e_join %>%
  summarize(correlation = cor(sum, n))


school_e_join %>%
  lm(sum ~ n, data = .) %>%
  tidy(conf.int = TRUE)

locs3 <- locs %>% 
  select(X, Y)

locx <- locs3 %>% 
  pull(X)

locy <- locs3 %>% 
  pull(Y)

cook <- get_acs(geography = "tract",
                variables = racevars, 
                year = 2018,
                state = "IL",
                county = "Cook",
                geometry = TRUE,
                summary_var = "B02001_001") 

# theme_void()




write_rds(e2007, "e2007.rds")
write_rds(e2008, "e2008.rds")
write_rds(e2009, "graph/e2009.rds")
write_rds(e2010, "graph/e2010.rds")
write_rds(e2011, "graph/e2011.rds")
write_rds(e2012, "graph/e2012.rds")
write_rds(e2013, "graph/e2013.rds")
write_rds(e2014, "graph/e2014.rds")
write_rds(e2015, "graph/e2015.rds")
write_rds(e2016, "graph/e2016.rds")
write_rds(e2017, "graph/e2017.rds")
write_rds(e2018, "graph/e2018.rds")
write_rds(e2019, "graph/e2019.rds")
write_rds(aid, "graph/aid.rds")
write_rds(enrollment, "graph/enrollment.rds")
write_rds(school2, "graph/school2.rds")









