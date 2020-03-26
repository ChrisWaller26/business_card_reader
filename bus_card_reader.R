#### Load Pakcgaes ####

library(xgboost)
library(tidyverse)
library(purrr)
library(dplyr)
library(data.table)
library(stringr)

options(stringsAsFactors = FALSE)

#### Import Datasets ####

forenames = 
  fread("https://raw.githubusercontent.com/smashew/NameDatabases/master/NamesDatabases/first%20names/all.txt",
        header = FALSE)[[1]]
surnames = 
  fread("https://raw.githubusercontent.com/smashew/NameDatabases/master/NamesDatabases/surnames/all.txt",
        header = FALSE)[[1]]

headings = c("Company", "Name", "Job_Title", "Industry", 
            "Phone_Number", "Email", "Website", "Other")

company_table = fread("Lookup_Tables/7mm_companies.csv") %>%
  as.data.frame()

#### Process Training Data ####

url_prefices = c("http://", "www.", "http://www.", "")

company_table_reduce = 
  company_table %>%
  select(
    Company  = name, 
    Website  = domain, 
    Industry = industry
  ) %>%
  filter(
    Website != "nan"
  ) %>%
  mutate(
    Website = str_c(sample(url_prefices, 1, replace = T), 
                    Website)
  )

company_table_stacked_01 =
  company_table_reduce %>%
  pivot_longer(
    cols = everything(),
    names_to = "Type",
    values_to = "Text"
  )

sample_size = 7e6

# Generate random names

set.seed(1)

TEST_PROP = 0.2

random_names = 
  data.frame(
    Type = "Name",
    Text = str_c(sample(forenames, sample_size, replace = T),
                 " ",
                 sample(surnames, sample_size, replace = T))
  )
  
company_table_stacked_02 =
  bind_rows(
    company_table_stacked_01,
    random_names
  )

company_table_shuffle =
  company_table_stacked_02 %>%
  slice(
    sample(seq(nrow(.)),
           nrow(.)
           )
    ) %>%
  mutate(
    train_no = row_number() %% (1/TEST_PROP)
  )
  
company_table_test =
  lapply(seq(1/TEST_PROP),
         function(i){
           company_table_shuffle %>%
             filter(
               train_no == i - 1
             ) %>%
             select(
               -train_no
             )
           }
         )
  
  
  
  
  

