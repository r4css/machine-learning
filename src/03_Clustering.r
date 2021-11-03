# Set Parameter -----------------------------------------------------------

set.seed(1234)
options(scipen = 10000)

# Install & Load Packages -------------------------------------------------

# Install Packages
install.packages("factoextra")

# Load packages
library(tidyverse)
library(cluster)
library(factoextra)

# Load Data ---------------------------------------------------------------

# Import Data
df <- read_csv2("data/income-spend.csv")

# Melihat Keseluruhan Data
df

# Melihat Deskriptif Statistik
summary(df)

# Explorasi Data ----------------------------------------------------------

# Visualisasi Dalam Scatter Plot
df %>%
  ggplot(aes(x = Income, y = Spending)) +
  geom_point()

# Data Preprocessing ------------------------------------------------------

# Select Data
df_cluster <- df %>%
  select(Income, Spending)
head(df_cluster)

# Mencari Nilai Cluster Optimal -------------------------------------------

# Mencari nilai kluster optimal dengan elbow method
fviz_nbclust(df_cluster, kmeans, method = "wss")

# Mencari nilai kluster optimal dengan silhouette method
fviz_nbclust(df_cluster, kmeans, method = "silhouette")

# Machine Learning --------------------------------------------------------

# K-Means Clustering
cluster <- kmeans(df_cluster, centers = 2)
cluster

# Visualisasi Cluster K-Means
fviz_cluster(cluster,
  data = df_cluster,
  geom = "point",
  stand = FALSE
)
