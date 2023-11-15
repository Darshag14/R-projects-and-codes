#                  Regression: Boston Housing Data

# Includes sections from the
# ISLR Chapter 3 Lab: Linear Regression
# and a lot more.

# Sections:

# 0.  Setup

# 1.  Learning about the data
# 1.1 Viewing the data in table
# 1.2 Viewing the data in scatterplot matrix
#     univariate densities
#     bivariate density plots with smooth

# 2.  Single predictor models
# 2.1 Fit a single predictor model
# 2.2 Look at the model summary
# 2.3 Extracting estimates
#     coefficients, predicted values, ...
# 2.4 Computing confidence interval
#     based on linear regression models

# 3.  Graphics
# 3.1 Note on the attach() function
# 3.2 ISLR base-level R graphics
# 3.3 A better plot with ggplot
# 3.4 Regression diagnostics plots
# 3.4.1 Residuals versus fitted value
# 3.4.2 A Quantile - Quantile plot
# 3.4.3 A spread - location plot
# 3.4.4 Standarized residuals versus leverage
# 3.5 Normal distribution hypothesis test

# 4.  Multivariate Regression
# 4.1 R model specification syntax
# 4.2 Anova for nested model comparison
# 4.3 Model syntax for variable transformations
#     polynomials, splines, logs and more

# 5.  Viewing the model.matrix
#     and producing correlation plots
# 5.1 A two variable model
# 5.2 An all variable model
# 5.3 corrplot details
# 5.4 A transformation model
# 5.5 Comments. Factors and the model matrix

# 6.  Colinearity and the
#     variable inflation factor

# 7.  Correlation plot alternative

#======================================

# 0. Setup

## Run

library(MASS)
library(ISLR)
library(lattice)
library(hexbin)
library(ggplot2)
source("hw.r")
library(car)   # vif() and qqPlot functions
library(splines)
library(corrplot)

##========================================

# 1. Learning about the data
#
# Read the class pdf file:
# Learning about data and model building.
#
# 1.1 View the data with a data editor
#
# The View() function provide
# a scrollable table.

View(Boston)

# What are the variable names?
# We can read the names from the table
# but they leave a lot of questions.
#
# What do the variable names mean?
# What are the units of measure?
# What is the dependent variable?
#
# What is known the variable relationships?
# Are there previous models of the data?
# Are there models of similar data?

?Boston
#
# The documentation begins to answer questions.
# More can be learned from the cited papers.
#
# A paper title suggests there are collinearity.
# issues to address with this data.
#
# 1.2 Viewing the data in scatterplot matrix
#     using univariate and bivariate density
#     plots and smooths
#
# The dependent variable is the median value
# of owner-occupied homes. Below we put this in
# the top left corner the scatterplot matrix
# where many people will look first.
#
# Below we show the univariate densities
# down the diagonal from the top left to
# the bottom right. Off the diagonal we
# show bivariate density using hexagon bin
# plots. The darker hexagon cells have
# more counts.

varNum <- function(x){
   val <- 1:ncol(x)
  names(val) <- colnames(x)
   return(val)
}
varNum(Boston)

Boston1 <- Boston[,c(14,1:13)]

plot(cor(Boston1))
# Below, we modify hexbin have to 15 xbins
# and set the trans() function power to 0.5.
# The square root of cell counts will
# be encode as gray level
#
# We comment out the panel.loess function
# for the first example  because some panels
# have data ill-suited to smoothing.

offDiag <- function(x,y,...){
  panel.grid(h = -1,v = -1,...)
  panel.hexbinplot(x,y,xbins = 15,...,border = gray(.7),
  trans = function(x)x^.5)
#  panel.loess(x , y, ..., lwd=2,col='red')
}

onDiag <- function(x, ...){
  yrng <- current.panel.limits()$ylim
   d <- density(x, na.rm = TRUE)
   d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
   panel.lines(d,col = rgb(.83,.66,1),lwd = 2)
   diag.panel.splom(x, ...)
}

splom(Boston1,as.matrix = TRUE,
  xlab = '',main = "Boston Housing: Selected Variables",
  pscale = 0, varname.cex = 0.8,axis.text.cex = 0.6,
  axis.text.col = "purple",axis.text.font = 2,
  axis.line.tck = .5,
  panel = offDiag,
  diag.panel = onDiag
)

# The y-axis in the top row of the scatterplot
# matrix top encodes the dependent variable,
# the median value ($1000) of owner-occupied homes
# in different Boston Suburbs.
#
# In the top row, the x-axes for columns to 2 to 14
# the encode the predictor or explanatory variables.
#
# Two predictor variables, rm and lstat,
# are more directly suggestive
# of functional relationships than the
# others.  We can envision straight lines or
# smooth curves going though the body of data,
# for each plot in the top row, but it better
# to show these explicitly. 
#
# However it is not appropriate to fit curves to
# categorical values encoded as integers.
# Also there may be problems with smooth
# curve fits when the continuous predictor
# values are concentrated  at two points.
# Below we remove selected variables to avoid
# Includes omitting variables with one or two high
# univariate density spikes.  Looking down
# the diagonal we see that
# variables crim, zn, chas and black
# have such spikes. We do keep some of the
# variables that have gaps on the x-axis.
#
# (An automated procedure could run smoothing
# functions and omit the variables for
# which it doesn't work.)


varNum(Boston1)
Boston2 <- Boston1[,-c(2,3,5,13)]

# uncomment the panel.loess line
offDiag <- function(x,y,...){
  panel.grid(h = -1,v = -1,...)
  panel.hexbinplot(x,y,xbins = 15,...,border = gray(.7),
    trans = function(x)x^.5)
  panel.loess(x , y, ..., lwd = 2,col = 'red')
}

splom(Boston2, as.matrix = TRUE,
  xlab = '',main = "Boston Housing: Selected Variables",
  pscale = 0, varname.cex = 0.8,axis.text.cex = 0.6,
  axis.text.col = "purple",axis.text.font = 2,
  axis.line.tck = .5,
  panel = offDiag,
  diag.panel = onDiag
)

# As envisioned from the scatterplot matrix,
# The top row variables rm and lstat look promising
# because they track more of y-axis (housing price)
# variability.
#
# Historically to explore the data
# we would fit the dominant structure,
# obtain the residuals, replace y
# in the scatterplot matrix with the residuals
# make another scatterplot matrix.
#
# The idea was to remove the visully dominant
# strucure to see if there was additional
# structure beneath the surface.
#
# The generalized additive models of today(gam)
# have put this idea to work.
# This beyond the scope of this class.
# Those interested can read class texts:
# R For Everyone Section 20.3 and
# ISLR Chapter 7 and Section 7.1
#=============================================

# 2.  Single predictor models

# 2.1 A first linear model

lm.fit <- lm(medv~lstat,data=Boston)
lm.fit

# 2.2  Look at the model summary

summary(lm.fit)

# For the moment assume the model
# distribution assumptions are satisfied.
#
# Is the model significantly better
# than fitting just the mean?
# What does the F-statistic indicate?
#
# Which regression coefficients,
# if any, are significantly
# different than 0?
#
# What fraction of the variability is
# explained by the model?
#
# 2.3 Extracting estimates
#     coefficients, predicted values, ...
#
# The R script above has saved the components of
# linear regression in the object lm.fit
# This is a list object.  We can extract
# results using the list component
# names.  However there are handy extractor
# functions written for model list objects.
# Some extractor functions include useful
# computations. Examples appear
# below and in section 4.4.

# See list component names
names(lm.fit)

# Access by name
lm.fit$coefficients

# Some extractor functions

coef(lm.fit)
predict(lm.fit)
residuals(lm.fit)
rstandard(lm.fit)

inf <- influence(lm.fit)
range(inf$hat) # hat values indicate point leverage

# 2.4 Computing confidence intervals
#     based on linear regression models
#
# Compute confidence intervals for
# regression coefficients

confint(lm.fit) # 95% is the default
confint(lm.fit,level = .99)

# R will compute confidence intervals
# for the mean response at given predictor
# values.
#
# R will compute confidence intervals
# for future observations at given
# predictor values. Each new value
# also includes is own random error.
# This increases the size of the
# confidence interval.

## Run prediction at 3 values

vals = c(5,10,15)

predict(lm.fit, data.frame(lstat = vals),
  interval = "confidence")

predict(lm.fit,data.frame(lstat = vals),
  interval = "prediction")

#=====================================
#
# 3. Graphics
#
# 3.1 Note on the attach() function
#
# Attaching a data.frame makes its variables
# accessible without reference to the data.frame.
# We can detach() the data.frame when we no
# longer want direct access to the variables.

# 3.2 Quick R base-level graphic used in ISLR

attach(Boston)

plot(lstat,medv)

plot(lstat,medv,pch = 20, col="red")

plot(lstat,medv,pch="+",col="blue")

# Plot redesign opportunities.
#
# There is no title that provides context.
#
# The description (meaning) of the x and y
# axis variables is vague and the units
# of measure are absent.
#
# Horizontal text for y axis tick labels
# (or grid line labels) would be easier
# to read.
#
# There are no grid lines.
#
# There is no pointwise confidence interval
# for the fitted regression line.
#
# The data points could be more prominent.
#
# The plot can be better separated from
# white page by using (gray level) contrast.

# 3.3 A better plot
#
# We can add grid lines, predicted confidence
# intervals for the fitted line and
# better labels. Adding color provides
# some visual appeal.  We can do this
# with based level graphics but here we
# use ggplot().
#
# R syntax comment:
# Surrounding the sequence of script lines
# with () causes R to continue reading
# lines until if finds the matching parenthesis.
# My slight appearance preference is to see
# the ggplot continuation symbol "+" at the
# left of a new line.

ggplot(Boston,aes(x = lstat,y = medv)) +
  geom_point(shape = 21,fill = "red",
     color = "black",size = 2) +
  stat_smooth(method = lm,
     color = "blue",fill = "cyan") +
  labs(
    x = "Lower Status Percent of Population",
    y = "Median House Value ($1000)",
    title = "Boston Housing Data") + hw

# 3.4  Regression diagnostic plots
#
# The following puts R's four standard
# regression diagnostic plots in four
# panels on one page.

plot(lm.fit,las = 1)

# 3.4.1 Residuals vs fitted Values plot
#
# The red line is not close to be being
# a horizontal line centered at 0.
# Rather it is curved with mostly
# positive residuals for small fitted
# values.  There are three numbered positive
# points near the top in the fitted
# interval 25 to 30.  An algorigthm has
# flagged these as outliers. The pattern
# is not consistent with the assumption
# that the errors are independent identically
# distributed samples from a normal
# distribution with mean 0.
#
# 3.4.2 The Quantile - Quantile plot
#
# The top right of the Q-Q plot shows a thick
# thick right tail. This includes three oultiers.
# When the plot is tall the row numbers are
# easy to read due to less overplotting. The
# numbers are 268, 372 and 373.
#
# 3.4.3 The Scale location plot
#
# If the model errors are independent identically
# distributed samples from a normal
# distribution with mean 0, the variance of the
# standardize residual not should be a function of
# other variables or compute variables such as the
# fitted values.
# In the plot we are hoping to see a roughly flat red line
# at y = 1.  The very curved line indicates a big
# departure fro the identically distributed assumpton.
#
# The values encoded on the y axis warrant an explanation
# because they involve multiple transformations. 
# Technically the estimated residual's variance for each point is a
# function of the point's leverage.  When we treat residuals
# as surrogates for model errors we need to remove the part
# that is due to leverage and a chose to rescale the residuals
# based on model overall estimate of the standard deviation.  
# This adjustment produces the standardized residuals used
# in the plot. Ideally standarized residual should looked
# similar to a sample from a standard normal distribution.
#
# The y-axis encodes the square root of the absolute value
# of the each standardized residual. The negative values have
# been made positive. This makes it easier to see how far
# both positive and negative value are from zero.
# As indicated above, we hope to a horizontal red line
# close to y = 1.
#
# Technically the standardized residuals are
# correlated but these correlations are near
# zero so this is usually ignored.
#
# A sequence of random variables is called
# homoscedastic if all variables in the sequence
# have the same finite variance.  We the regression 
# residual to appeae homoscedastic. If the variances
# differ the sequence is called heteroscedastic.
#
# Scale and location plots can also
# be made with model predictor (explanatory)
# variables use on the x-axis rather than the
# fitted values. These can reveal model problems
# and sometimes suggest ways to improve the model.
#
# Sometimes we known that case values have
# different variances and sometimes we have reasonable
# estimates of the variances (or standard errors.)
# This information can be include in the model.
#
# For example estimates state mortality rates
# for high population states typically have
# smaller standard error than the estimates
# for low population states.  Inverse variance weights
# can be used in regression models to provide much
# better models.  The basic idea is simple.
# Give the most weight to the best estimates.

# 3.4.4 The Standarized residuals versus leverage plot
#
# The standardized residual
# versus leverage plot has high leverage points
# on the right.  Those with large magnitude
# residuals have high influence on the fitted
# values and coefficient estimates. They are
# pulling the smooth upward and away from 0.
#
# The 375th suburb in the data set has the
# high leverage of 0.027. How extremme is
# this? As a rule of thumb value
# above 2*p/n = 2*2/506= 0.008 is high.
# Here p=2 is the number of variables in the
# model counting the column of 1's used to estimate
# the mean and n is 506 the number of suburbs (cases).
#
# Some high leverage cases may be compatible
# with many other cases. They are not pulling
# the fit away from the fit for the other cases
# so are not called influential points.
#
# We might choose to omit or down weight
# influential points.  Fairly often this
# allows other points to pop out as having
# high influence.
#
# As for the solid red line, a smooth of
# the standardized residuals, the big departures
# from the horizontal line with y = 0 are
# warning that this an inappropriate model
# for making inferences using the standard
# methods.
#
# When red dashed lines appear they
# show contours for Cook's Distance. Their
# appearance warns us that one or more
# points is substantially influencing the
# regression coefficients. The dashed lines
# delimit areas in the plot associated
# with high leverage-based pull on the
# regression coefficients.

# 3.5  Normal distribution hypothesis test
#
# We can test the hypothesis that  of the
# standardized residuals have a normal 
# distribute.  

shapiro.test(rstandard(lm.fit))

# The hypothesis is stongly rejected
# 
# The test is not very powerful for small
# samples sizes.   Looking
# a Q-Q plot to make a decision is recommended.
#
# For very large data sets 
# test statistic will often reject
# the null hypothesis with the residual 
# distribution fairly close to a normal
# distribution and the model is quite good

#============================================

#4. Multiple Linear Regression
#
# Look at the top row of the scatterplot matrix
# with the median house price, on the y-axis.
# We see how the loess smooths relate to
# the bivariate distributions.  The lstat and rm
# variables tracked the shape of the bivariate
# distributions fairly well but there is a lot
# of vertical variation about the curves. Look at
# the high housing value points and see how far they
# are from the smooth curves. Many of these points
# are going to be outliers when fitting a single
# predictor variable.

# If a really good fit is to be found it will
# take some additional variables to address the points
# with a big vertical distance to the curve in
# the lstat plot. None of the other smooths in the
# top row look very promising in this regard.
#
# Later we will see that a random forest model is
# better suited to modeling this data.

# 4.1 R model specification syntax
#
# "~" means is modeled by
#
# "." means include all other variables
#
# "+" means include variables
#
# "-" means omit variables
#
# #___________________________
#
# "a:b" means interaction (product) of
#       the variables.  Same as I(a*b)
#
# "a*b" means include both variables and
#       their interaction a + b + a:b
#
# Since * is special model symbol
# symbol the notation I( ) supports
# use ordinary multiplication as to
# create a new variable as in I(a*b)
#
## Run: fit all other variable

lmAll.fit  <- lm(medv~., data = Boston)
summary(lmAll.fit)

# Run: fit both variables

lm.fit <- lm(medv~lstat+age,data = Boston)
summary(lm.fit)

# Run: fit all other variables
#  except age

lm.fit <- lm(medv~.-age, data = Boston)
summary(lm.fit)

# A model updating option with the
# same results as above

lm.fit <-  update(lm.fit, ~.-age)
summary(lm.fit)

# Compare some summaries

summary( lm(medv~lstat*age,
  data = Boston))

summary( lm(medv~lstat+age+I(lstat*age),
  data = Boston))

summary( lm(medv~lstat+age + lstat:age,
  data = Boston))

#--------------------------------------

# 4.2 Anova for nested model comparison

# A linear model in lstat
lm.fit <- lm(medv~lstat)

# A quadratic model in lstat
lm.fit2 <- lm(medv~lstat+I(lstat^2))

summary(lm.fit2)
plot(lm.fit2)

# Is the quadratic model
# a significantly better fit?
# We can use anova()to compare two
# regression models when one
# model just includes additional
# variables.

anova(lm.fit,lm.fit2)

# The fit is much better. The p value
# is very close to zero. Still,
# when the residuals are inconsistent
# with model assumptions
# they undermine the foundation for
# making probability statements.
# We can still talk in terms of
# R-squared improvement.

# 4.3 Model syntax for variable transformations
#     polynomials, splines, logs and more

# The syntax poly() supports fitting
# polynomial models and ns() supports
# fitting natural splines.
#
# On the left consider all the points with low
# status percent between 0 and 10.  The range of
# the median house values is large.  A regression
# line cannot fit this data very well.  Another
# predictor variable that will spread the values
# out over the x-axis will have a much better
# chance of improving the fit.
#
# On the far right side of x-axis there is not
# much data to tie down the polynomial model.
# The curve looks too curved.
#
# Polynomials can have more peaks and valleys than
# we would like.  Smoothing splines are better
# behaved.  A natural spline is below.

lm.fit4 <- lm(medv~ns(x = lstat,3))
summary(lm.fit4)

ggplot(Boston,aes(x = lstat,y = medv)) +
  geom_point(shape = 21,fill = "red",
     color = "black",size = 2) +
  stat_smooth(method = "lm",formula = y~ns(x,5),
     color = "blue",fill = "cyan") +
 labs(
    x = "Lower Status Percent of Population",
    y = "Median House Value ($1000)",
    title = "Boston Housing Data") + hw

# The fit looks better on the right.  There are
# still big positive residuals for x in the
# interval [5 10].
#
# We can make log and power transformations. This
# does not conflict with the special with model
# syntax.

summary(lm(medv~log(rm),data = Boston))

lm.fit6 <- lm(medv~poly(lstat,2)+poly(rm,2)+ptratio,
   data = Boston)
summary(lm.fit6)
plot(lm.fit6)

# Now we are up to an R-squared of about 0.77 with
# this model.  However the single point 365 is really
# influencing the regression coefficients.
#=================================================

# 5. The model matrix and variable correlations
#
# An R model matrix can include predictor variables,
# transformed factors (categorical variables), and
# variable  interactions.
# The model matrix is used to fit the data.
#
# 5.1 Looking at the model matrix and correlations for
#    a simple two variable

# The first column is all 1's to fit the mean.  We remove this.

modelMat1 <- model.matrix(medv~lstat+rm, data = Boston)[,-1]
corMat1 <- cor(modelMat1)
corMat1

corrplot(corMat1, method = "ellipse",
  mar = c(0,0,2,0),  # space for title
  title = "Model Matrix Variable Correlations")

# 5.2 The model with all the original variables
#     Below lmAll.fit is the list we produced
#     above from that model.  We can refer to it.

varMat <- model.matrix(lmAll.fit)[,-1]
varCor <- cor(varMat)

# Plot Correlations

corrplot(varCor,method = "ellipse",
   tl.col = "black", mar = c(1,1,3,0),
   title = "Boston Housing Variables")

# 5.3 Correlation plot details

# The plot looks more organized if we
# arrange the variables by  the first
# principal component.

corrplot(varCor,order = "FPC",method = "ellipse",
  tl.col = "black", mar = c(0,0,2,0),
  title = "Boston Housing Variables")

# The narrow ellipses have high correlation.
# The positive slope and blue indicated
# positive correlations. We can also
# include value. The numbers are readable
# if the plot is big enough.

corrplot(varCor,order = "FPC",method = "ellipse",
  tl.col = "black", mar = c(0,0,2,0),
  title = "Boston Housing Variables")

corrplot(varCor,order = "FPC",add = TRUE, # add the above plot
  type = "lower", method = "number",number.font = 2,
  number.cex = .75,col = "black",
  diag = FALSE,tl.pos = "n", cl.pos = "n")

# 5.4 The model design matrix for
#     model specifed transformations

# We can access the model matrix variables
# produced using variable transformations
# such as poly() and ns().  We can look
# at the columns of values and at their
# correlation matrix.

modelMat2 <- model.matrix(lm(medv~poly(lstat,2)+poly(rm,2)+ptratio,
   data = Boston))[,-1]

corMat2 <- cor(modelMat2)

corrplot(corMat2,order = "FPC",method = "ellipse",
  tl.col = "black", mar = c(1,1,3,0),
  title = "Boston Housing Computed Variables")

corrplot(corMat2,order = "FPC",add = TRUE, # add the above plot
  type = "lower", method = "number",number.font = 2,
  number.cex = .75,col = "black",
  diag = FALSE,tl.pos = "n", cl.pos = "n")



# 6. Colinearity and the variance inflation factor
#
# A problem with predictor variable collinearity
# (correlation) is that this inflates the
# estimated variance of regression coefficients.
#
# We can regress the ith predictor variable on the other
# predictor variables to obtain the R-squared value, Ri**2.
# The variance inflation factor for the ith regression
# coefficient is 1/(1-Ri**2).
# If the ith predictor were linearly independent of other
# predictor Ri**2 would be 0 and there would no inflation.
#
# It easy to obtain the variable inflation factor using
# saved model results.

vif(lmAll.fit)

# Sometimes omitting one variable or more variables
# produces a model that fits almost as well and 
# variance inflation is substantially reduced.

