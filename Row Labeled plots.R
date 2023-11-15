# A Row-labeled dot plot

library(tidyverse)
source('C:/Users/darsh/Downloads/hw.R')

# Reading comma delimited files

DowJones <- read.csv(file = "C:/Users/darsh/Downloads/Stats 515/Week 3/DowJones2014Jan18.csv")

is_tibble(DowJones)#modified data frame(same as data frame)

class(DowJones)
head(DowJones)



#A row labeled plot

ggplot(DowJones, aes(x = Yearly, y = Company)) +
  geom_point(
    shape = 21,
    fill = "blue",
    color = "black",
    size = 3
  ) +
  labs(x = "One Year Percent Change",
       title = "Dow Jones January 18, 2014") + hw

# When we read the company names in the row-labeled dot plot
# we may notice they are in descending alphabetical order
# from top to bottom.
#
# Alphabetic order may be good for finding a company names
# but people are often interested in the rank order of the
# companies based on a variable such as the one-year percent
# change.

# The reorder() function provides simple way
# to control the row plotting order so that
# the company with the largest percent
# change in yearly value appears at the top.
#
# The second argument of reorder() specifies tibble
# variable that we want to control the Company order
# on the y-axis.  In table terminology y-axis order is
# the row order.  In table terminology rows are numbered
# top down.  The graph y-axis convention the number increase
# from bottom to top. Convention conflicts can be bothersome
# and be behind communication problems.

# 3.1 Sorting the rows

ggplot(DowJones, aes(x = Yearly,  y = reorder(Company, Yearly))) +
  geom_point(
    shape = 21,
    fill = "blue",
    size = 3,
    color = "black"
  ) +
  labs(x = "One Year Percent Change",
       y = "Company",
       title = "Dow Jones January 18, 2014") + hw

# We can use -Yearly reverse the company plotting order.

# 3.2 Reversing the sort order

ggplot(DowJones, aes(x = Yearly, y = reorder(Company,-Yearly))) +
  geom_point(
    shape = 21,
    fill = "blue",
    size = 3,
    color = "black"
  ) +
  labs(x = "One Year Percent Change",
       y = "Company",
       title = "Dow Jones January 18, 2014") + hw


################################################



# In plot production, sorting cases and variables
# often serves to simplify plot appearance!
# This can also bring out patterns that were
# obscure and call attention
# to deviations from the dominant patterns.


# In-class exercise =======================================
# Read the DowJones2012Jan28.csv file into a tibble
# and give it a new name.
ile.
# ===============================================

