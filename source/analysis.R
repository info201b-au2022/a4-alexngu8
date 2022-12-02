library(tidyverse)


# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#

library("dplyr")
library("tidyr")
library("ggplot2")
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv.version")

#----------------------------------------------------------------------------#
get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/assignments/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
}

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

get_year_jail_pop <- function(df) {
    t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(df)  {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)   
} 

View(plot_jail_pop_for_us)

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/assignments/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
}

get_jail_pop_by_states <- function(df) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)   
}

plot_jail_pop_by_states <- function(df) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)   
}

plot <- ggplot(plot_jail_pop_by_states) +
  ggtitle("plot") 

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


## Load data frame ---- 


