# This file is to help people get up and running if they're
# having trouble with the bike share data. Also, check out
# www.itl.nist.gov/div898/handbook/pmc/section4/pmc4.htm
# for general information about time series.
#
# AUTHOR: Ryan McNamara
# DATE:   Feb 27, 2015


# Get zoo package for dealing with irregular time series.
install.packages("zoo")
library("zoo")

# Grab the data - your folder will vary!
tr <- read.csv("~/R/Bikeshare/train.csv")
te <- read.csv("~/R/Bikeshare/test.csv")

# zoo will fill in the missing values so we can plot "count."
# as.POSIXct basically just converts datetime to an actual datetime.
z.count <- zoo(tr$count, as.POSIXct(tr$datetime))
plot(z.count)

tr.ts <- ts(z.count)

# Hopefully your machine didn't throw up on you!
