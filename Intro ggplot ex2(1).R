### Introduction to ggplot - Example 2 ####
###########################################


## ggplot functions/codes: 
# https://ggplot2.tidyverse.org/reference/
# http://www.sthda.com/english/wiki/ggplot2-point-shapes



# Old faithful geyser dataset is used for this example
#__________________________________________________________


# install.packages("tidyverse") # this is another way of installing a package
library(tidyverse)
source('C:/Users/darsh/Downloads/hw.R') # Instructor provide ggplot theme


# 1. The old faithful geyser data


# https://en.wikipedia.org/wiki/Old_Faithful
# https://www.livescience.com/4957-secret-faithful-revealed.html
# https://www.livescience.com/28699-old-faithful-hidden-cavern.html


# The data set, faithful, is in the MASS package.
library(MASS) # This has the faithful data set 


### let's look at the dataset.
data(faithful)

?faithful #or us the Help tab

head(faithful) # look at the first few rows of the dataset
str(faithful) # looking at the structure of the dataset


# Here we see the eruption duration
# and the wait time to the next eruption
# are measured in minutes.

# Knowing a variable's units of measure is
# is often important for analysis, interpretation of
# results and communication. Below we will put the
# units of measure in the plots.


# Getting descriptive statistics.

# We can get descriptive statistics for
# variables in data.frames and
# other data structures

summary(faithful)




# 2. Producing a scatter plot using ggplot

# Let's create a scatter plot between the 
#variables waiting and eruptions


ggplot(faithful, aes(x=waiting, y= eruptions))+
  geom_point(fill='blue', size=3.5, color='black', shape=21)
#use the 'color' function to make the borders of the circles.
  #use 'fill' to color the shapes and 'color' for the borders






# Interesting Facts

# The ggplot() function development
# followed the design and terminology
# described in Leland Wilkinson's illuminating
# book, "The Grammar of Graphics".
#
# In the book, the term aesthetics refers to
# the Greek word that means perception.
# In the syntax above,the aes() function
# pairs aesthetic key words to data.frame/tibble variables.
#
# The most commonly used aesthetics are
# x and y position, size, shape, fill and color.
# ggplot uses these aesthetics when rendering
# geometric objects that we can see.
#
# Functions with prefix "geom_" specify
# geometric objects.
# Three examples are geom_point(), geom_line(), and
# geom_polygon().



# let's use geom function arguments
# to change aesthetic constants.

# Below the blue filled circle wit
ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(fill='blue', size=3.5, color='black', shape=21)+



# The x and y aesthetics specified in the ggplot()
# function were passed to the geom_point function.
#
# Notes:
# 1) There is no default "geom_" function.
# 2) We can add multiple geom layers to a plot using "+"
# 3) Each geom function can have it own
#    tibble or data.frame,
#    aes() function that pairs aesthetics with
#      tibble or data frame variables, and
#    aesthetic key words paired with constants.


# 3. Adding a smooth of form y = f(x)+ e
#    to suggest a possible functional relationship
#    in the presence of noise, e.


#Loess regression is a nonparametric technique 
#that uses local weighted regression to fit a smooth 
#curve through points in a scatter plot.

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(fill='blue', size=3.5, color='black', shape=21) +
  geom_smooth(method='loess', col='black')



# We can choose other data fitting methods.
# Below lm, specifies a linear regression model.
# Below we also change the geom layer plotting
# order and aesthetic value constants.

ggplot(faithful,aes(x=waiting,y=eruptions)) + 
  geom_point() 




# What if we changed the order of the layers??

ggplot(faithful,aes(x=waiting,y=eruptions)) + 
  geom_smooth(method=lm, color="red",size=1.1) + geom_point()

# Now black points appear on top of the red line.


# 4. Plot labeling

# In this class, graphics are to include variable names
# and the units of measure.  The units of measure may
# appear in variable labels or perhaps in the title.

# When the graphics are in the class project report, the
# labeling can appear in figure caption.


#Let's create labels as follows,

#let x-axis be the Waiting Time Between Eruptions in Minutes
#let y-axis be the Eruption Duration in Minutes
#let the title be Old Faithful Geyser Eruptions

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape=21, size=2, fill="green", color="black")+
  geom_smooth(method=loess, size=1.2)  
  


# Plotting space constraints may motive the use of
# shorter labels. We might use abbreviations such
# min. for minutes.  For a long text string another
# option is to use two lines. The text "\n" in a
# character string signals the start a new line of text.
# Note the x-axis label in the example below.

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape=21, size=2.,fill="green",color="black")+
  geom_smooth(method=loess,size=1.2, span=.6)+
  labs(x="Waiting Time Between Eruptions\n In Minutes",
       y="Eruption Duration in Minutes",
       title="Old Faithful Geyser Eruptions")

# If both variables are in the same units,
# the units might be specified in the title.

# 5. Themes that control plot appearance


#  The theme "hw" that we will be using in class:

#Following is the description about the theme and 
#reasoning behind choices that were made
# according to the author of the theme, 
#Dr. Daniel Carr,in his own words

############################################################
# There are many theme functions that control plot that
# appearance such as plot fill color and grid lines.
# The default theme, theme_gray, I provides a
# light gray fill and white grid lines at two
# scales of resolution.

# I have long advocated using a gray fill to increase
# the plot presence on page. Adding a dark gray outline lifts
# the plot off the page and gives presence as an entity.
# The White grid lines have low contrast when plotted
# on a light gray background. They appear in the background
# while supporting higher perception accuracy extraction
# for the location of symbols plotted on top.
#
# The hw.R script read in Section 1 includes the default
# theme_gray function but modifies some of its default
# argument. It
# outlines the plot with dark gray line,
# centers the title,
# removes the superfluous tick marks,
# recovers tick mark wasted space,
# and removes the secondary grid lines,
#
# Previously the high contrast black ticks marks and
# axis labels pushed the plot without an outline into
# the background.  The white grid lines running into page
# glued it the plot to page.
#
# Recently the plot title default location was changed
# in theme_gray to be at the top left of the plot panel
# rather than at the top center.  The English reading
# convention is left to right and then top down.  Those
# with this convention may well look first at the top
# left first. A can be made for this change.
# However I am used to more symmetry in title,
# so I re-centered the title.
#######################################################

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape=21, size=2, fill="green", color="black")+
  geom_smooth(method=loess, size=1.2, span=.6) +
  labs(x="Waiting Time Between Eruptions in Minutes",
       y="Eruption Duration in Minutes",
       title="Old Faithful Geyser Eruptions") + hw

# 6. Kernel versus histogram density plots

# Many people are familiar with histograms
# they can be useful for communication
# purposes.

ggplot(faithful,aes(x=waiting)) +
  geom_histogram()+
  labs(x="Waiting Time Between Eruptions In Minutes",
       y="Density",
       title="Old Faithful Geyser Eruptions") + hw

# We can control the binwidth,the
# rectangle fill color and the line color.

ggplot(faithful,aes(x=waiting, y=..count../sum(..count..))) + #..before and after indicates that it is a parameter and not a value
  geom_histogram(binwidth=2,                     #each line of hist. is called bin.
                 fill="cornsilk",color="black")+
  labs(x="Waiting Time Between Eruptions In Minutes",
       y="Counts",
       title="Old Faithful Geyser Eruptions")+hw



# Histograms typically encode one of three different
# quantities on the y axis:
# counts, percent of total count, or data density.
# The little used ggplot syntax below specifies
# using density for the y axis. This enables
# comparison with a superposed kernel density estimate.

ggplot(faithful,aes(x=waiting,y = ..density..)) +
  geom_histogram(binwidth=2,
                 fill="cornsilk",color="black")+
  labs(x="Waiting Time Between Eruptions In Minutes",
       y="Density",
       title="Old Faithful Geyser Eruptions") + hw

# We can save a graphic object, plot it and access it by
# name, change specification and a plot layers.
# Before for superposing a density
# curve, we superpose scaled frequency polygon
# which is similar to a density plot.  Some statistic
# reasons some consider frequency polygons to be
# better density estimates than histogram estimate.


# Save the plot
histPlot <- ggplot(faithful,aes(x=waiting,..density..))+
  geom_histogram(binwidth=2, fill="cornsilk",color="black")+
  labs(x="Waiting Time Between Eruptions in Minutes",
       y="Density",
       title="Old Faithful Geyser Eruptions")+hw

# Plot it
histPlot



# Add a frequency polygon and plot
histPlot+ geom_freqpoly(binwidth=2,color="red",size=1.2)#gives sharp points, hence cant integratte


# Add a computed kernel density, extend the x axis and plot
histPlot+ geom_line(stat="density",color="blue",size=1.2)+
  xlim(33,104)

# Above we superpose a density curve using geom_line
# and extend the plot limits because the kernel
# density estimates are positive over a larger interval
# than the data minimum and maximum.
# Yes, the extension can be obviously wrong when tails
# go outside the limits of possible values.

# Below, we fill the area under the density curve.
# A alpha argument controls fill transparency
# The alpha argument can have values
# ranging from 0 to 1 with 0 being completely transparent.

histPlot +     #adjust controls the smoothness of the line
  geom_density(adjust=.8,fill="cyan",color="black",alpha=.4)+#alpha controls the transparency
  xlim(35,102)#to fill the color of area under curve


# We still need to settle on kernel smoothing parameter
# It is controlled above by adjust=.4.  Decades ago
# we used our opinion and picked between densities that
# looked too rough and too smooth.  Today, cross validation
# algorithms can often suffice. More knowledge about the data,
# the task and the audience might guide us toward a different
# choices. Knowledge may be indicate there is more real
# structure than suggest by peaks and valley of the sample
# at hand.





