library(tidyverse)


# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#

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
#----------------------------------------------------------------------------#
## Section 2  ---- Summary
#----------------------------------------------------------------------------#

library("dplyr")
library("tidyr")
library("ggplot2")

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv.version")

# Average female population in 2002
avg_female_jail_pop <- incarceration_trends %>%
  filter(year == "2002") %>%
  select(state, female_jail_pop) %>%
  group_by(state) %>%
  summarize(state_female_jail_pop = sum(female_jail_pop, na.rm = TRUE)) %>%
  summarize(avg_jail_pop = round(mean(state_female_jail_pop))) %>%
  pull(avg_jail_pop)

avg_female_jail_pop <- prettyNum(avg_1988_female_jail_pop, big.mark = ",", scientific = FALSE)
avg_female_jail_pop

# Highest population of Black individuals
highest_black_jail_pop <- incarceration_trends %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(black_jail_pop_1 = sum(black_jail_pop), female_jail_pop_1 = sum(female_adult_jail_pop)) %>%
  filter(black_jail_pop_1 == max(black_jail_pop_1)) %>%
  pull(black_jail_pop_1)

highest_black_jail_pop

# The proporation of Black individuals list
black_jail_county_proportion_2002 <- incarceration_trends %>% 
  filter(year == 2002) %>% 
  group_by(county_name) %>% 
  summarise(the_avg_black = mean(black_jail_pop, na.rm = TRUE),
            the_avg_jail = mean(total_jail_pop, na.rm = TRUE),
            proportion = round(the_avg_black / the_avg_jail, digit = 4))

black_jail_county_proportion_2002

# The lowest proportion that was in 2002

lowest_proportion_black_2002 <- black_jail_county_proportion_2002 %>% 
  filter(proportion == max(proportion, na.rm = TRUE))

lowest_proportion_black_2002


#----------------------------------------------------------------------------#
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

get_year_jail_pop <- function() {
  jail_pop_df <- incarceration_trends %>% 
    group_by(year) %>% 
    filter(total_jail_pop != "NA") %>% 
    summarise(total_jail_pop = sum(total_jail_pop)) 
  return(jail_pop_df)
}


plot_jail_pop_for_us <- function()  {
  ggplot(get_year_jail_pop(), 
         aes(x = year, y = total_jail_pop)) + 
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Years", 
         y = "Total Jail Population",
         title = "Increase of Jail Population in United States from 1970-2018", 
         caption="The U.S. prison population, which had been increasing from 1980 to 2005, has been on a downward trend since 2010."
    )
}

plot_jail_pop_for_us()

#---------------------------------------------------------------------------#
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Jail Population by State 
states <- c("NY", "CA", "WA", "TX", "AL")

get_jail_pop_by_states <- function(states) {
  plotdata2 <- incarceration_trends %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    select(state, year, total_jail_pop)
  return(plotdata2)
}


plot_jail_pop_by_states <- function(states)  {
  linechart2 <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total_jail_pop, color = state)) +
    labs(title = "United States Jail Population Increase by States" , x = "Year",
         y = "Total Jail Population", caption = "Growth Between 1970-2018")
  return(linechart2)
}


plot_jail_pop_by_states(c("NY", "CA", "WA", "TX", "AL"))


#----------------------------------------------------------------------------#
## Section 5  ---- 
#----------------------------------------------------------------------------#
black_white <- function() {
  df <- data.frame(black_jail_pop = incarceration_trends$black_jail_pop, white_jail_pop = incarceration_trends$white_jail_pop)
  return(df)
}
View(male_female())

plot_inequality <- function() {
  inequal <- 
    ggplot(black_white(), aes(x = black_jail_pop, y = white_jail_pop)) +
    geom_point() +
    ggtitle("Jail Population for White and Black individuals") +
    labs(caption = "This plot shows the different patterns for Black vs White individuals
         jail populations")
  return(inequal)
}
plot(plot_inequality())
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
install.packages("usmap")
library(usmap)
map_black_pop <- function(){
  df <- incarceration_trends %>%
    filter(year == max(year)) %>%
    select(state, black_jail_pop, total_pop) %>%
    mutate(black_prop = black_jail_pop/total_pop * 100) %>%
    group_by(state) %>%
    summarise(black_prop = mean(black_prop, na.rm = TRUE))
  
  is.na(df)<-sapply(df, is.nan)
  df[is.na(df)]<-0
  return(df)
}

black_value <- function(){
  df <- map_black_pop() %>% filter(black_prop != is.na(black_prop))
  PL<- plot_usmap(region = "state", data = df, values = "black_prop") +
    scale_fill_continuous(
      na.value = "gray48", low = "blue", high = "red", 
      name = "Percentage Black Individuals in Prison") +
    labs(title = "Black Population Ratio per State (2018)",
    )
  return(PL)
}
black_value()
#----------------------------------------------------------------------------#


## Load data frame ---- 


