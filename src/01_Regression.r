# Set Parameter -----------------------------------------------------------

# Set Seed dan Parameter
set.seed(1234)
options(scipen = 10000)

# Install & Load Package --------------------------------------------------

# Install packages
install.packages(c(
  "tidyverse",
  "tidymodels"
))

# Load packages
library(tidyverse)
library(tidymodels)

# Load Data ---------------------------------------------------------------

# Import Data
df <- read_csv("data/salary.csv")

# Menampilkan 5 baris pertama data
head(df)

# Menampilkan rangkuman data menggunakan fungsi glimpse()
glimpse(df)

# Data Exploration --------------------------------------------------------

# Visualisasi Data
df %>%
  ggplot(aes(x = YearsExperience, y = Salary)) +
  geom_point(color = "red")

# Data Preprocessing ------------------------------------------------------

# Split Data: Membagi data dengan proporsi 70:30
df_split <- initial_split(df, prop = 0.7)
df_split

# Menampilkan data training
df_split %>%
  training() %>%
  glimpse()

# Membuat Alur Pemrosesan Data --------------------------------------------

# Membuat Recipe
df_recipe <- training(df_split) %>%
  recipe(Salary ~ YearsExperience) %>%
  prep()
df_recipe

# Mererapkan ke data training
df_training <- juice(df_recipe)
glimpse(df_training)

# Menerapkan ke data testing
df_testing <- df_recipe %>%
  bake(testing(df_split))
glimpse(df_testing)

# Modeling ----------------------------------------------------------------

# Menset model liner regression
lr <- linear_reg() %>%
  set_engine("lm")

# Membuat Workflow
workflow <- workflow() %>%
  add_model(lr) %>%
  add_recipe(df_recipe)

# Training model linear regression
model <- fit(workflow, training(df_split))

# Testing -----------------------------------------------------------------

# Prediksi ke data testing
model %>%
  predict(testing(df_split)) %>%
  bind_cols(testing(df_split))

# Model Evaluation
multi_metrics <- metric_set(rmse, mape, rsq)

# Melihat performa model regresi
model %>%
  predict(testing(df_split)) %>%
  bind_cols(testing(df_split)) %>%
  multi_metrics(truth = Salary, estimate = .pred)

# Prediksi Ke Data Baru ---------------------------------------------------

# Membuat Data
df_new <- tibble(YearsExperience = 4)

# Melakukan Prediksi
model %>%
  predict(df_new)
