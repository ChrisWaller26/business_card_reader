#### Load Pakcgaes ####

library(xgboost)
library(tidyverse)
library(purrr)
library(dplyr)
library(data.table)
library(stringr)
library(ngram)
library(FeatureHashing)

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

table_size = 150e3

company_table_reduce = 
  company_table %>%
  select(
    Company  = name, 
    Website  = domain, 
    Industry = industry
  ) %>%
  filter(
    Company  != "nan",
    Website  != "nan",
    Industry != "nan"
  ) %>%
  mutate(
    Website = str_c(sample(url_prefices, 1, replace = T), 
                    Website)
  ) %>%
  slice(
    sample(seq(nrow(.), table_size))
  )

company_table_stacked =
  company_table_reduce %>%
  pivot_longer(
    cols = everything(),
    names_to = "type",
    values_to = "text"
  )

sample_size = 50e3

# Generate random names

set.seed(1)

info_table = 
  bind_rows(
    company_table_stacked,
    data.frame(
      type = "Name",
      text = str_c(sample(forenames, sample_size, replace = T),
                   " ",
                   sample(surnames, sample_size, replace = T)
      )
    )
  ) %>%
  slice(
    sample(seq(nrow(.), nrow(.)))
  )

# Convert labels to numbers

groups = unique(info_table$type)

VALID_PROP  = 0.2
DATA_SIZE  = 1e5
data_rows  = seq(DATA_SIZE)
train_rows = seq(DATA_SIZE * (1 - VALID_PROP))
valid_rows  = setdiff(data_rows, train_rows)

info_table_label =
  info_table %>%
  slice(data_rows) %>%
  mutate(
    text = sapply(text, function(x) ngram::splitter(x, split.char = TRUE)),
    type_num = match(type, groups) - 1
  ) %>%
  select(
    -type
  )

# Hashing

info_table_hash =
  hashed.model.matrix(
    ~split(text,
           delim = " ",
           type = "tf-idf"),
    data = info_table_label,
    hash.size = 2^16,
    signed.hash = FALSE
  )

# Split into Training and Test Data

dtrain = xgb.DMatrix(info_table_hash[train_rows,],
                     label = info_table_label$type_num[train_rows])
dvalid = xgb.DMatrix(info_table_hash[valid_rows,],
                     label = info_table_label$type_num[valid_rows])
watch = list(train = dtrain,
             valid = dvalid)

#### Gradient Boosting Model ####

# XGBoost

xgb_model = xgb.train(
  nrounds     = 10,
  max.depth   = 20,
  eta         = 1,
  nthread     = 12,
  data        = dtrain,
  num_class   = length(groups),
  objective   = "multi:softmax",
  watchlist   = watch,
  eval_metric = "merror"
)

#CatBoost

#LightGBM

#### Validation ####

pred = predict(xgb_model, dvalid)
accuracy = mean(pred == info_table_label$type_num[valid_rows])

errors =
  data.frame(
    text = info_table$text[valid_rows],
    type_act = info_table$type[valid_rows],
    type_pred = groups[pred + 1]
  )[pred != info_table_label$type_num[valid_rows],]
