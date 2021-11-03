# Set Parameter -----------------------------------------------------------

# Set Seed dan Parameter
set.seed(1234)
options(scipen = 10000)

# Install and Load Packages -----------------------------------------------

# Install Packages
install.packages(c(
  "arules",
  "arulesViz"
))

# Load Packages
library("tidyverse")
library("arules")
library("arulesViz")


# Load Data ---------------------------------------------------------------

# Load data
data(Groceries)

# See data information
print(Groceries)

# Example of Item in Transaction
inspect(head(Groceries, 5))



# Association Rules -------------------------------------------------------

# Generate Frequent Itemset
itemsets <- apriori(Groceries,
  parameter = list(
    maxlen = 1,
    support = 0.001,
    target = "frequent itemsets"
  )
)

# 5 Items with the highest support value
inspect(head(sort(itemsets, by = "support"), 5))

# Rules Generation
rules <- apriori(Groceries,
  parameter = list(
    support = 0.001,
    target = "rules"
  )
)

# Get 10 rules with the highest confidence value
highconfidence <- head(sort(rules, by = "confidence"), 10)

# Visualize ---------------------------------------------------------------

# Visualize rule
plot(highconfidence, method = "graph")
