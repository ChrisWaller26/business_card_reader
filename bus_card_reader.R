#### Load Pakcgaes ####

library(xgboost)
library(tidyverse)
library(purrr)
library(dplyr)
library(data.table)
library(stringr)
library(ngram)
library(FeatureHashing)
library(jsonlite)

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

email_list = 
  data.frame(
    text = fread("Lookup_Tables/E_Mails.csv")[[1]],
    type = "Email"
  )

job_titles = fromJSON("Lookup_Tables/cleaned_related_titles.json") %>%
  pivot_longer(
    cols = everything(),
    values_to = "job_title"
  ) %>%
  select(
    -name
  ) %>%
  filter(
    !is.na(job_title)
  ) %>%
  distinct() 

#### Process Training Data ####

url_prefices = c("http://", "www.", "http://www.", "")
table_size = 150e3
sample_size = 50e3
set.seed(1)

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
  )

company_table_stacked =
  company_table_reduce %>%
  pivot_longer(
    cols = everything(),
    names_to = "type",
    values_to = "text"
  ) %>%
  sample_n(
    table_size
  ) %>%
  bind_rows(
    email_list
  ) %>%
  bind_rows(
    data.frame(
      text = job_titles$job_title,
      type = "Job Title"
    ) %>%
      sample_n(
        sample_size
      )
  )

# Generate random phone numbers

tel_suffix = c("+4401", "+44 01", "+44 1", "+44 (0)1", "+44-01", "+44-1", "+44-(0)1", "01",
               "+4402", "+44 02", "+44 2", "+44 (0)2", "+44-02", "+44-2", "+44-(0)2", "02",
               "+4407", "+44 07", "+44 7", "+44 (0)7", "+44-07", "+44-7", "+44-(0)7", "07")

random_tel = str_c(sample(tel_suffix, sample_size, replace = T),
                   sample(100:999, sample_size, replace = T),
                   sample(c(" ", "-"), sample_size, replace = T),
                   sample(100:999, sample_size, replace = T),
                   sample(c("", " ", "-"), sample_size, replace = T),
                   sample(100:999, sample_size, replace = T)
                   )

# Generate random names

info_table = 
  bind_rows(
    company_table_stacked,
    data.frame(
      type = "Name",
      text = str_c(sample(forenames, sample_size, replace = T),
                   " ",
                   sample(surnames, sample_size, replace = T)
      )
    ),
    data.frame(
      type = "Tel",
      text = random_tel
      )
    ) %>%
  sample_n(
    nrow(.)
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
  max.depth   = 10,
  eta         = 1,
  nthread     = 6,
  data        = dtrain,
  num_class   = length(groups),
  objective   = "multi:softmax",
  watchlist   = watch,
  eval_metric = "merror"
)

#CatBoost

#LightGBM

#### Validation ####

info_table_summary =
  info_table %>%
  group_by(
    type
  ) %>%
  summarise(
    count = n()
  )

pred = predict(xgb_model, dvalid)

model_output = 
  data.frame(
    text = info_table$text[valid_rows],
    type_act = info_table$type[valid_rows],
    type_pred = groups[pred + 1]
    )

accuracy = mean(pred == info_table_label$type_num[valid_rows])

accuracy_summary =
  model_output %>%
  group_by(
    type_act,
    type_pred
  ) %>%
  summarise(
    count = n()
    ) %>%
  ungroup() %>%
  group_by(
    type_act
  ) %>%
  mutate(
    count_type = sum(count),
    acc = count / count_type,
    percent = 
      str_c(round(acc * 100, 1), "%")
  ) %>%
  ungroup() %>%
  complete(
    type_act,
    type_pred,
    fill = list(percent = "0%")
  ) %>%
  select(
    type_act,
    type_pred,
    percent
  ) %>%
  pivot_wider(
    names_from = type_pred,
    values_from = percent,
    values_fil = list(percent = "0%")
  ) %>%
  arrange(
    type_act
  )

errors =
  model_output %>%
  filter(
    type_act != type_pred
    ) %>%
  arrange(
    type_act
  )
