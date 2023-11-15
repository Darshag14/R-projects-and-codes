# A Row-labeled dot plot

# Sections===================================

# 0.  Setup
#
# 1.  Reading comma delimited files
#     to create tibbles and data frames
# 1.1 Accessing rows and columns of data frames

# 2.  Row-labeled dot plots and controlling row order

# 0. Setup =============================================

library(tidyverse)
source('hw.R')

# 1. Reading comma delimited files
#    to create tibble
DowJones <- read.csv(file = "DowJones2014Jan18.csv")
#is.tibble(DowJones)
is_tibble(DowJones)
# Entering an object name invokes a default
# print function of object of its class.
DowJones

# R functions are designed to work with
# objects in particular classes
#
# Objects may be in more than one class so can
# be used by more functions.

class(DowJones)

# Recently developed tibbles are very similar to
# data.frames that have been used in recent decades.
# The similarily is sufficient to allow tibbles to be
# used in functions developed for data.frames.

# Addition print functions for some classes
# such a tibbles

head(DowJones)

# By default head prints the first 6 rows
# It has an second argument, n, that specifies
# the number of rows.
# Sometimes a function argument name can be omitted.

head(DowJones, n = 12)
head(DowJones, 2)

# We can look at the last 5 rows using tail.

tail(DowJones, n = 5)

# 2. Accessing tibble rows and columns
#
# An old approach uses a square bracket notation [,]
# This uses of integer, logical, character string vectors.
# to select rows and columns.  (I may refer to these
# vectors as subscript.)
# One in a while this is approach is more
# convenient that the tidyverse approach describe
# further below

# 2.1 Row and column integer based selection

DJ <- DowJones  # This makes  a copy with a shorter name.

DJ[1,3] # This uses constants
        # It selects the 1st row and 3rd column
row <- 1
col <- 3

DJ[row,col] # vector of length 1 work fine.


1:3      # This create a vector of length 3 and
DJ[1:3,] # selects the first three rows and all columns

subs <- c(4,2,3,1) # This creates a named vector of length 4
DJ[subs, 3:2 ]     # These vectors also rearrange rows and columns

DJ[1:4, -c(2,3)]  # Negative integers remove rows or columns

# 2.1.2 Logical vector selection

colnames(DJ) # Reminder of column names
rowsubs <- DJ[,2] > 50
rowsubs

colsubs <- colnames(DJ) %in% c("Close","Weekly")
colsubs

DJ[rowsubs,colsubs]

# Note that logical vector lengths
# need match the number of tibble rows and columns
# R replicates shorter vectors to have needed length
# and provides a warning message.

DJ[c(TRUE,FALSE,FALSE),]

# 2.1.3 Character string selection
#
# Unlike data.frames and matrices, tibbles don't use
# row names.  Column selection still works.

DJ[, c("Close","Weekly")]

# 2.2 Tidyverse subsetting methods.
#
# These methods seek to make scripts easier to read
# by using functions to avoid the [] notation.

# It keeps chosen rows by using a filter function
filter(DJ, Close < 100, Weekly > 1)

# It arranges rows with the arrange function
arrange(DJ, Weekly)

# 3. A row labeled plot

ggplot(DowJones, aes(x = Yearly, y = Company)) +
  geom_point(
    shape = 21,
    fill = "blue",
    color = "black",
    size = 3
  ) +
labs(x = "One Year Percent Change",
    title = "Dow Jones January 28, 2012") + hw

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

# In plot production, sorting cases and variables
# often serves to simplify plot appearance!
# This can also bring out patterns that were
# obscure and call attention
# to deviations from the dominant patterns.


# In-class exercise =======================================
# Read the DowJones2012Jan28.csv file into a tibble
# and give it a new name.
#
# Modify the script for the row labeled plot
# in section 3.2
#
# Use the new tibble, change the symbol shape to a diamond,
# the fill color to yellow, and fix the date in the title
# to match the data one the csv file.
# ===============================================

