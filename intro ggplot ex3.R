### Introduction to ggplot - Example 3 ####
###########################################


# diamonds dataset is used for this example
#_____________________________________________________-

library(tidyverse)
source('C:/Users/darsh/Downloads/hw.R') #used for the background of the graphs


#1. Let's look at diamond dataset 

?diamonds
data(diamonds)
str(diamonds)
class(diamonds)

# 2. Using bar height to encode factors level counts

# 2.1 Diamond color as a factor

# Show  the  levels of the diamond color on the x-axis.
# Use  geom_bar to show the counts for the levels

ggplot(diamonds , aes( x = color) ) +
  geom_bar() +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set")+ hw


ggplot(diamonds , aes( x = color) ) +
  geom_bar(fill="cornsilk",color='black') +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set")+ hw



# Also use the diamond color factor to control
# the bar fill color.
# This example used default fill colors

ggplot(diamonds , aes( x=color, fill=color))+ geom_bar() + hw

# Make the bar outline black
# Provide  x axis, y axis and title labels
# Remove the redundant legend

ggplot(diamonds, aes(x=color, fill=color))+
  geom_bar(color="black") +        # black outline
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set")+ hw +
  theme(legend.position="none")


# 2.2 Modify the previous script and
#     produce the plot using the
#     diamond factor "cut" rather
#     than the factor color
#     There are three place to make changes,
#     Don't forget the label

ggplot(diamonds, aes(x=cut, fill=cut))+
  geom_bar(color="black") +        # black outline
  labs(x="Diamond Cut Classes",
       y="Count",
       title="Diamond Data Set")+ hw +
  theme(legend.position="none")







# 3. Stacked Bar Plots

ggplot(diamonds, aes( x=color, fill=cut) )+
  geom_bar(color="black") +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set",
       fill="Cut") + hw

# Color linking with more the four colors
# is getting cognitively difficult
#
# If we don't have names for the colors that
# makes the task harder. What is the color name
# for "Good" in the legend. It is easier to
# think "look for red" than "look for an off yellow color".
#
# It helps when the legend fill color order
# matches the stack bar fill color order. This recently became
# the ggplot2 default.



#R support several ways of specifying color. 
# The rgb function defines red, green and blue light intensities on a 0 to 1 scale.
# Is is light additive color model. rgb(1,0,0) is full intensity red.  
# rgb(0,1,0) is full intensity green and rgb(0,0,1) is full intensity blue. 
# rgb(1,1,0) is yellow, rgb(1,0,1) is magenta, and rgb(0,1,1 ) is cyan.
# rgb(1,1,1) is white.  rgb(0,0,0) is black. 
# rgb(.5,.5.5) is one shade of gray. This the same as gray(.5)


# The example below the shows controlling the fill colors
# using scale_fill_manual

ggplot(diamonds,aes(x=color,fill=cut))+
  geom_bar(color=gray(.55)) +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set",
       fill="Cut")+
  scale_fill_manual(
    values=c("red","orange",rgb(0,.8,0),'cyan','violet'))+ hw





# 4. Plots for continuous variables
# 4.1 Histograms

ggplot(data=diamonds, aes(x=carat))+
  geom_histogram(fill="cyan",color="black") + hw

# Increase the number of x-axis bins

ggplot(data=diamonds, aes(x=carat))+
  geom_histogram(fill="cyan",color="black",bins=50) + hw


#adjust the x-axis

ggplot(data=diamonds, aes(x=carat, y=..density..))+
  geom_histogram(fill="cyan",color="black",bins=50) +
  geom_density(fill="cornsilk", color="black")+
  xlim(0,3.5) +  hw






# 4.2 Kernel Density plots

ggplot(data=diamonds, aes(x=carat))+
  geom_density(fill="cyan",color="black")+ hw

# Change the kernel width

ggplot(data=diamonds, aes(x=carat))+
  geom_density(fill="cyan",color="black",adjust=2) + hw

# Change the kernal shape
# See ?density for options

ggplot(data=diamonds, aes(x=carat, color=cut))+
  geom_density(fill="cyan", color="black",
               kernel="epanechnikov", adjust=2) + hw

#4.3 Superposed Density Plots

# Superposed density plots for each level
# of the factor cut, also use this
# as the density fill color
# The group argument groups data by level of cut
# so there is density plot for each level.
# Set the transparency to .2. The transparency scale
# from 0 (transparent) to 1 (no color mixing).

ggplot(data=diamonds, aes( x=carat, group=cut, fill=cut) )+
  geom_density( color="black", adjust=2, alpha= .2) +
  scale_fill_manual(
    values=c("red", "yellow", "green","cyan", "violet")) + hw

# The mixing of multiple colors is hard to decode
# With only one color in the plot the color match with the
# legend will not be exact because the alpha blends the gray.
# One could switch to a white plot background with gray grid lines

# Omit some high carat outliers using xlim() for a better view
# of the densities

ggplot(data=diamonds, aes(x=carat,group=cut,fill=cut))+
  geom_density(color="black",adjust=2,alpha=.2)+
  xlim(0,3.5)

#4.4 Scatterplots and smooths
#    see R For Everyone 7.7.2

# Saving the plot setup

scatt <- ggplot(diamonds, aes( x=carat, y=price) )

# Show the points

scatt + geom_point()  + hw

# 4.5 Superposed scatterplot
#      with point color encoding diamond color

scatt + geom_point( aes( color = color) ) + hw

#want to have scatter plots for each color
# 4.5 Juxtaposed scatterplots using facet_wrap
# wrap arranges the graphs in linear manner and grid arranges in grid manner

scatt + geom_point(aes(color=color))+ facet_wrap(~color) +hw

# 4.6 Two-way juxtaposed scatterplots
#     using facet_grid

scatt + geom_point(aes(color=color))+ facet_grid(clarity~color)+hw

# 4.7  Restricting the x-axis scale and better labeling for axes

scatt2 <- ggplot(diamonds, aes( x=carat, y=price/1000) )


scatt2 + geom_point(aes( color = color))+
  facet_grid( clarity~cut )+
  hw + xlim(0,3.5)+
  labs(x="Carats, 9 Diamonds With Carats > 3.5 removed",
       y="Price in $1000",
       title=paste("Diamonds: Row Panels for Clarity Classes",
                   "Column Panels for Cut Classes",sep="\n"),
       color="Color") + hw

