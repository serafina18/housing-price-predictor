library(tidyverse)
theme_set(theme_bw())

housing <- read.table("housing-prices-ge19.txt", sep="\t")

# House prices and properties in New York. What properties of a house can predict its price? 
# Can we use such a model to identify houses that are extraordinarily expensive or inexpensive?
visdat::vis_miss(housing) + coord_flip() + theme(legend.position = "none")

