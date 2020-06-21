
# Using crime data from the file uscrime.txt (http://www.statsci.org/data/general/uscrime.txt, 
# description at http://www.statsci.org/data/general/uscrime.html), test to see whether there are any outliers in the last column (number of crimes per 100,000 people). 
# Use the grubbs.test function in the outliers package in R.

# Step 1: install library, load data from http://www.statsci.org/data/general/uscrime.txt and view data
library(readr)
file_path <- "http://www.statsci.org/data/general/uscrime.txt"
crime_df <- read.table(file=file_path, sep="\t", header=T, stringsAsFactors = F)
crime_df <- na.omit(crime_df) # remove missing values
tail(crime_df)

# Step 2: install and load required library outliers
# test to find if there is an outlier in the last column - number of crimes per 100,000 people
library(outliers)
grubbs.test(crime_df$Crime)
grubbs.test(crime_df[,16], type = 11, opposite = FALSE, two.sided = FALSE )
# type=10 is a test for one outlier, 
grubbs.test(crime_df[,16], type=10, opposite = TRUE)
grubbs.test(crime_df[,16], type=11) # type=11 is a test for two outliers on opposite tails
# grubbs.test(crime_df[,10], type=20) # type=20 is test for two outliers in one tail (but limited to n < 31).
# verify the grubbs.test result with a five-number summary 
summary(crime_df$Crime)
#create a box and whisker plot
boxplot(crime_df[,16], ylab ='Crime Values', xlab="Crimes per 100,000 people", main = 'Crime Outliers')
