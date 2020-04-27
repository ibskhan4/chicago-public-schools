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

#Create a directory and load in the two datasets. Make sure they are in the
#right folder.
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


#Make the dataframe an rds file that can be used in the shiny app.
write_rds(e2007, "~/Documents/ml6/graph/e2007.rds")
write_rds(e2008, "~/Documents/ml6/graph/e2008.rds")
write_rds(e2009, "~/Documents/ml6/graph/e2009.rds")
write_rds(e2010, "~/Documents/ml6/graph/e2010.rds")
write_rds(e2011, "~/Documents/ml6/graph/e2011.rds")
write_rds(e2012, "~/Documents/ml6/graph/e2012.rds")
write_rds(e2013, "~/Documents/ml6/graph/e2013.rds")
write_rds(e2014, "~/Documents/ml6/graph/e2014.rds")
write_rds(e2015, "~/Documents/ml6/graph/e2015.rds")
write_rds(e2016, "~/Documents/ml6/graph/e2016.rds")
write_rds(e2017, "~/Documents/ml6/graph/e2017.rds")
write_rds(e2018, "~/Documents/ml6/graph/e2018.rds")
write_rds(e2019, "~/Documents/ml6/graph/e2019.rds")
write_rds(enrollment, "~/Documents/ml6/graph/enrollment.rds")
write_rds(dropout, "~/Documents/ml6/graph/dropout.rds")
write_rds(aid, "~/Documents/ml6/graph/aid.rds")
write_rds(school2, "~/Documents/ml6/graph/school2.rds")
write_rds(budget, "~/Documents/ml6/graph/budget.rds")
write_rds(locs, "~/Documents/ml6/graph/locs.rds")

