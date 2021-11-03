# Set Parameter -----------------------------------------------------------

set.seed(1234)
options(scipen = 10000)

# Install and Load Parameter ----------------------------------------------

# Install packages
install.packages(c(
  "tidyverse",
  "tidymodels",
  "discrim",
  "themis",
  "rpart.plot",
  "randomForest",
  "naivebayes"
))

# Load packages
library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)
library(themis)
library(discrim)


# Load Data ---------------------------------------------------------------

# Import Data
df <- read_csv2("data/bank-marketing.csv")


# Data Exploration --------------------------------------------------------

# Meampilkan 5 baris pertama data
head(df)

# Menampilkan rangkuman data menggunakan fungsi glimpse()
glimpse(df)

# Visualisasi Data
df %>%
  ggplot(aes(x = y, fill = y)) +
  geom_bar()

# Data Preprocessing ------------------------------------------------------

# Split Data: Membagi data dengan proporsi 70:30
df_split <- initial_split(df, prop = 0.7)
df_split

# Menampilkan data training
df_split %>%
  training() %>%
  glimpse()

# Menampilkan data testing
df_split %>%
  testing() %>%
  glimpse()

# Membuat Alur Pemrosesan Data --------------------------------------------

# Membuat Recipe
df_recipe <- training(df_split) %>%
  recipe(y ~ .) %>%
  step_downsample(y) %>%
  prep()
df_recipe

# Menerapkan ke data training
df_training <- juice(df_recipe)
glimpse(df_training)

# Menerapkan ke data testing
df_testing <- df_recipe %>%
  bake(testing(df_split))
glimpse(df_testing)


# Modeling ----------------------------------------------------------------

# Menset model decision tree
dt <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Menset Model Random Forest
rf <- rand_forest(mode = "classification") %>%
  set_engine("randomForest")

# Setting the Naive Bayes Model
nb <- naive_Bayes() %>%
  set_engine("naivebayes") %>%
  translate()

# Membuat Workflow Decision Tree
workflow_dt <- workflow() %>%
  add_model(dt) %>%
  add_recipe(df_recipe)

# Membuat Workflow Random Forest
workflow_rf <- workflow() %>%
  add_model(rf) %>%
  add_recipe(df_recipe)

# Membuat Workflow Naive Bayes
workflow_nb <- workflow() %>%
  add_model(nb) %>%
  add_recipe(df_recipe)


# Training ----------------------------------------------------------------

# Training Model Decision Tree
model_dt <- fit(workflow_dt, training(df_split))

# Lihat Model Decision Tree
tree_fit <- model_dt %>%
  pull_workflow_fit()
rpart.plot(tree_fit$fit, roundint = FALSE)

# Training Model Random Forest
model_rf <- fit(workflow_rf, training(df_split))

# Training model nb
model_nb <- fit(workflow_nb, training(df_split))

# Testing and Evaluation --------------------------------------------------

# Prediksi ke data testing
testing(df_split) %>%
  bind_cols(predict(model_nb, testing(df_split))) %>%
  bind_cols(predict(model_dt, testing(df_split))) %>%
  bind_cols(predict(model_rf, testing(df_split)))

# Menentukan metrik evaluasi untuk mengukur performa model
multi_metrics <- metric_set(
  accuracy,
  precision,
  recall,
  specificity,
  f_meas
)

# Melihat performa model decision tree
model_dt %>%
  predict(df_testing) %>%
  bind_cols(df_testing) %>%
  multi_metrics(truth = y, estimate = .pred_class)

# Melihat performa model random forest
model_rf %>%
  predict(df_testing) %>%
  bind_cols(df_testing) %>%
  multi_metrics(truth = y, estimate = .pred_class)

# Melihat performa model naive bayes
model_nb %>%
  predict(df_testing) %>%
  bind_cols(df_testing) %>%
  multi_metrics(truth = y, estimate = .pred_class)

# Prediksi Ke Data Baru ---------------------------------------------------

# Import data baru
df_new <- read_csv2("data/bank-marketing_new.csv")
head(df_new)

# Melakukan prediksi dan menyimpan nilai hasil prediksi
df_predicted <- predict(model_rf, df_new) %>%
  bind_cols(df_new) %>%
  write_csv("data/bank-marketing_predicted.csv")

# Menampilkan hasil prediksi
print(df_predicted)

# Siapa yang akan berlangganan
df_subscribe <- df_predicted %>%
  filter(.pred_class == "yes")
print(df_subscribe)
