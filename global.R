library(shiny)
library(shinydashboard)

# Load required libraries
library(dplyr)   # Data manipulation
library(ggplot2) # Data visualization
library(ggpubr)    
library(scales)  # For number formatting (commas, percentages)
library(tidyverse)  # For data cleaning and manipulation 
library(glue) 
library(plotly)  # For interactive visualizations
library(lubridate)   # For working with date and time
options(scipen = 100) # Prevent scientific notation in numeric outputs
library(readxl)      
library(purrr) # Mapping function over lists


# Load Data
## Read Data: Gross
### Define the folder path for the "Gross" dataset
folder_path_gross <- "dataset/DPD NPL Gross Net Extrenal/Gross/"

### List all .xlsx files in the "Gross" folder
file_list_gross <- list.files(path = folder_path_gross,
                              pattern = "\\.xlsx$",
                              full.name = TRUE)

### Combine all Excel files into one dataframe
sqllab_gross <- file_list_gross %>% 
  map_df(read_excel)


## Read Data: Net
### Define the folder path for the "Net" dataset
folder_path_net <- "dataset/DPD NPL Gross Net Extrenal/Net/"

### List all .xlsx files in the "Net" folder
file_list_net <- list.files(path = folder_path_net,
                            pattern = "\\.xlsx$",
                            full.name = TRUE)

### Combine all Excel files into one dataframe
sqllab_net <- file_list_net %>% 
  map_df(read_excel)


## Read Data: External
### Define the folder path for the "External" dataset
folder_path_external <- "dataset/DPD NPL Gross Net Extrenal/External/"

### List all .xlsx files in the "External" folder
file_list_external <- list.files(path = folder_path_external,
                                 pattern = "\\.xlsx$",
                                 full.name = TRUE)

### Combine all Excel files into one dataframe
sqllab_external <- file_list_external %>% 
  map_df(read_excel)


# Data Cleaning
## 1. Data Cleansing & Preprocessing: Gross
sqllab_gross_clean <- sqllab_gross %>% 
  
  mutate(
    # Changing data types
    cut_off_date= ymd(cut_off_date),
    island = as.factor(island), 
    region = as.factor(region),
    area = as.factor(area), 
    branch = as.factor(branch), 
    bucket = as.factor(bucket),
    noa = as.integer(noa),
    outstanding_amount = as.numeric(outstanding_amount),
    
    # adding new column
    date = day(cut_off_date),
    month = month(cut_off_date),
    year = year(cut_off_date)
  ) %>% 
  
  arrange(desc(cut_off_date))

### If there are any NA values, remove the rows containing NA
sqllab_gross_clean <- sqllab_gross_clean %>% 
  drop_na()

### If there are any duplicates, keep only the unique rows OR remove any duplicates
sqllab_gross_clean <- sqllab_gross_clean %>% 
  distinct(.keep_all = TRUE)


## 2. Data Cleansing & Preprocessing: Net
sqllab_net_clean <- sqllab_net %>% 
  
  mutate(
    # Changing data types
    cut_off_date= ymd(cut_off_date),
    island = as.factor(island), 
    region = as.factor(region),
    area = as.factor(area), 
    branch = as.factor(branch), 
    bucket = as.factor(bucket),
    noa = as.integer(noa),
    outstanding_amount = as.numeric(outstanding_amount),
    
    # adding new column
    date = day(cut_off_date),
    month = month(cut_off_date),
    year = year(cut_off_date)
  ) %>% 
  
  arrange(desc(cut_off_date))

### If there are any NA values, remove the rows containing NA
sqllab_net_clean <- sqllab_net_clean %>% 
  drop_na()

### If there are any duplicates, keep only the unique rows OR remove any duplicates
sqllab_net_clean <- sqllab_net_clean %>% 
  distinct(.keep_all = TRUE)


## 3. Data Cleansing & Preprocessing: External
sqllab_external_clean <- sqllab_external %>% 
  
  mutate(
    # Changing data types
    cut_off_date= ymd(cut_off_date),
    island = as.factor(island), 
    region = as.factor(region),
    area = as.factor(area), 
    branch = as.factor(branch), 
    bucket = as.factor(bucket),
    noa = as.integer(noa),
    outstanding_amount = as.numeric(outstanding_amount),
    
    # adding new column
    date = day(cut_off_date),
    month = month(cut_off_date),
    year = year(cut_off_date)
  ) %>% 
  
  arrange(desc(cut_off_date))

### If there are any NA values, remove the rows containing NA
sqllab_external_clean <- sqllab_external_clean %>% 
  drop_na()

### If there are any duplicates, keep only the unique rows OR remove any duplicates
sqllab_external_clean <- sqllab_external_clean %>% 
  distinct(.keep_all = TRUE)
