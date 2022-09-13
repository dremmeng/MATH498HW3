suppressMessages(library(fields))
suppressMessages(library(fda))
suppressMessages(library( lubridate))
load("~/School/MATH498/HW3/BoulderDaily.rda")

dim(BoulderDaily)
names(BoulderDaily)
#######################################################
########## omitting missing values across the data set
#### but only include the temperatures as missing
#### (including rainfall and snow have many more missing)
#### this makes the subsequent analysis easier
####
#### call this new data frame BDClean
####
BDClean <- na.omit(BoulderDaily[, c(1:5, 9)])
dim(BDClean) # note fewer nonmissing observations
names(BDClean)


BDates<- ymd( paste0(BDClean$year,"/",BDClean$month,"/",BDClean$day ))
BDClean$dates<- BDates

# find all years with fewer than 35 days missing
# I think this is pretty hard R coding so it 
# may take a few reads to figure this out. 
timeYear<- year(BDates)
countDays<- table( timeYear) 
indGood<- countDays > 365 -35
goodYears<- names( countDays)[ indGood]
# NOTE goodYears are charcter strings 
daysToKeep<- !is.na( match( as.character(timeYear), 
                      goodYears)
               )
# subset data frame to years with more than 365 -35 obs
BDClean<- BDClean[daysToKeep,]



#Finally omit any rows of the data frame with a missing tmin
ind<- !is.na(BDClean$tmin)
BDClean<- BDClean[ind,]


# s is the fraction of year for a particular day
nYear<- ifelse(leap_year(BDClean$dates), 366, 365)
s<-  yday(BDClean$dates)/ nYear

Y<- BDClean$tmin


bplot.xy(
  s,
  Y,
  N = 55,
  col = "green4",
  xlab = "Fraction of Year",
  ylab = "degree (F)"
)

# interpret next line
table( year(BDClean$dates))
   
# a sin cosine basis with  6 pairs and the constant function
fracOfYear<- s

freqX <- outer(2 * pi * fracOfYear, 1:6, "*")
dim(freqX)
Phi <- cbind(rep(1, length(Y)),
           sin(freqX), cos(freqX))

colNames <-  c("Contant", paste0("S", 1:6), paste0("C", 1:6))
dimnames(Phi) <-   list(NULL, colNames)

LSFit1 <- lm( Y  ~ Phi - 1)
#

# -1 means do not automatically include a
# constant vector in the model
# we have already built it into Phi
#
summary(LSFit1)
# some  standard diagnostic plots
#  -- not too easy to read with so many points so
# use some boxpolots instead.
# set.panel( 2,2)
# plot(LSFit1 )
# set.panel() # set back to 1 plot per view

# basic check on residuals
set.panel(1, 2)
bplot.xy(
  LSFit1$fitted.value,
  LSFit1$residuals,
  N = 50,
  xlab = "predicted temp",
  ylab = "redsiduals (F)"
)
yline(0, col = "red")

ind<- !is.na( c(Y))
bplot.xy(
  fracOfYear[ind],
  LSFit1$residuals,
  N = 50,
  xlab = "Fraction of Year",
  ylab = "residuals (F)"
)
set.panel()

# boxplot with fitted cycle added
# pay attention to dimensions!
fracGrid <- (1:365)/365
freqXPred <- outer(2 * pi * fracGrid, 1:6)
XPred <-  cbind(rep(1, length(fracGrid)),
                sin(freqXPred),
                cos(freqXPred))
seasonalCycle <- XPred %*% LSFit1$coefficients
dim(XPred )
length( seasonalCycle)

bplot.xy(
  s,
  Y,
  N = 40,
  col = "green1",
  xlab = "Fraction of Year",
  ylab = "degree (F)"
)

lines(fracGrid, seasonalCycle , col = "magenta",
      lwd = 2)

#### finally loop through years to get a 
#### smoothed cycle for each year. 
yearTag<- unique( BDClean$year)
NYear<- length( yearTag)
 
fracGrid<- (1:365)/365 
PhiYear<- cbind(rep(1, length(fracGrid)),
                sin(freqXPred),
                cos(freqXPred))
# this matrix will hold predicted values for each year
YHat<- matrix( NA, 365, NYear)

for( k in 1: NYear ){
# pull out one years data 
  ind<- BDClean$year == yearTag[k]
  yTemp<- Y[ind]
  cHat<- lm( Y[ind] ~ Phi[ind,] -1 )$coefficients
# predicted values at the fraction of year  
# note use of a different basis matrix for prediction
  YHat[,k]<- PhiYear%*%cHat
}

out<- fbplot( (YHat), x= 1:365, ylim=c(-15,70),
              xlab="day of Year", ylab="Daily tmin")


