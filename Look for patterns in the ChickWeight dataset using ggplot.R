#
#
#
# run in RSTudio like this:
# > source("this source file.R")
#
# USES DATASET: built in dataset ChickWeight
#
# -----------------------------------------------------------------------------
library(ggplot2)

# -----------------------------------------------------------------------------
readkey <- function()
{
    cat ("Press [enter] to continue")
    line <- readline()
}
# -----------------------------------------------------------------------------

data(ChickWeight)   # load dataset

print(class(ChickWeight))

message("first rows of data")
print(head(ChickWeight))	# have a first look at it - the columns, the range of numbers
# 
#  weight Time Chick Diet
# 1     42    0     1    1
# 2     51    2     1    1
# 3     59    4     1    1
# 4     64    6     1    1
# 5     76    8     1    1
# 6     93   10     1    1
#
message("summary:")
print(summary(ChickWeight))
#     weight           Time           Chick     Diet   
# Min.   : 35.0   Min.   : 0.00   13     : 12   1:220  
# 1st Qu.: 63.0   1st Qu.: 4.00   9      : 12   2:120  
# Median :103.0   Median :10.00   20     : 12   3:120  
# Mean   :121.8   Mean   :10.72   10     : 12   4:118  
# 3rd Qu.:163.8   3rd Qu.:16.00   17     : 12          
# Max.   :373.0   Max.   :21.00   19     : 12          

# factorize DIet and CHick Ids
message("Chick factor levels")
print(levels(factor(ChickWeight$Chick)))
# [1] "18" "16" "15" "13" "9"  "20" "10" "8"  "17" "19" "4"  "6"  "11" "3"  "1"  "12" "2"  "5"  "14" "7"  "24" "30"
# [23] "22" "23" "27" "28" "26" "25" "29" "21" "33" "37" "36" "31" "39" "38" "32" "40" "34" "35" "44" "45" "43" "41"
# [45] "47" "49" "46" "50" "42" "48"
message("Diet factor levels")
print(levels(factor(ChickWeight$Diet)))
# [1] "1" "2" "3" "4"

# -----------------------------------------------------------------------------
# PLOTS
# -----------------------------------------------------------------------------

# plot the 2 key columns first as bar charts to see the distribution independently
message("barplot of weight:")
barplot(ChickWeight$weight)
readkey()
# add a horizontal line at the average
message("mean")
abline(h= mean(ChickWeight$weight), col="red")
readkey()

message("median")
abline(h= median(ChickWeight$weight), col="lightgrey")
readkey()

message("barplot Time:")
barplot(ChickWeight$Time)
with(ChickWeight, barplot(Time)) # this is the same 
readkey()

message("scatter plot: sequentially by Diet")
# scatter plot to see how they're related - and show a different symobl for each Diet (1:4) 

with(ChickWeight,
	for(i in levels(factor(Diet))){
	plot(Time[Diet==i],weight[Diet==i],pch=i)
	readkey()
    } 
)
# with(ChickWeight, plot(Time,weight,pch=as.integer(factor(Diet)))) # using symbols
with( ChickWeight, plot(Time,weight,pch=levels(factor(Diet))))  # using digits

readkey()

# aha - seems a kind of linear relationship - lets fit a Linear Regression model and display 
# the resulting L-R line over top of the scatter plot
message("adding L-R line using abline")

lrLine <- lm(ChickWeight$weight ~ ChickWeight$Time )
abline(lrLine) # display L-R line 
readkey()

# -----------------------------------------------------------------------------
# USING GGPLOT2 library
# -----------------------------------------------------------------------------

# lets aggregate by by Time by averaging the weights at each Time coordinate
#
message("aggregated by Time (shows average weight at each Time age)")

ChickAgg <- aggregate(ChickWeight["weight"],ChickWeight["Time"],FUN=mean)
p <- ggplot(ChickAgg, aes(x = Time, y=weight)) + geom_bar(stat="identity")
print(p)
readkey()

# it seems there is a distribution of weights at each Time value (Time here is aka a feature) 
# now plot a boxplot to show that  

# first, use cut to group times into Buckets for the columns
message("boxplot:")

buckets <- seq.int(0,max(ChickWeight$Time,1))   # cut needs a vector of numbers to use as the columns
groupedTimes <- cut(ChickWeight$Time,buckets)

# now plot as boxplot
p <- ggplot(ChickWeight, aes(Time,weight,group=groupedTimes)) + geom_boxplot()
print(p)

readkey()

# try look at a grid of plots with each plot showing data points for one Diet 
message("A grid of plots: split by Diet")

p <- ggplot(ChickWeight, aes(Time,weight)) +
  geom_point() +
#  scale_x_log10(breaks = seq(2e4, 4e4, 1e4)) +
#  scale_y_log10(breaks = seq(50, 75, 5)) +
  facet_wrap(~ Diet, ncol = 4)
print(p)
readkey()

# now a grid of plots with each plot showing data points for one Chick 
message("A grid of plots: split by Chick Id")

p <- ggplot(ChickWeight, aes(Time,weight)) +
     geom_point() +
     facet_wrap(~ Chick, ncol = 10)
print(p)
readkey()

# zoom in on the distribution within one of the buckets
# create a subset of the dataset of only the 20day entries as a data frame 
ChickWeight20 <- subset(ChickWeight,Time==20)

# or create a vector of weights at Time=20
weightAt20 <- ChickWeight$weight[ChickWeight$Time == 20]
# weightAt20
# [1] 199 209 198 160 220 160 288 125 100 120 181 195  91 259 133 144 115 318 164 170  76 259 236 185 212 279 157 235 291 156 327 361 225
#[34] 169 280 250 295 199 269 199 197 231 210 303 233 264
message("Looking at the Time=20 bucket only: scatter")
plot(weightAt20)
readkey()

message("bar chart")
barplot(weightAt20)
readkey()
message("mean")
abline(h=mean(weightAt20), col="red")
readkey()
message("median")
abline(h=median(weightAt20), col="lightgrey")
message("+/- 1 std dev")
abline(h=median(weightAt20) + sd(weightAt20), col="red")
abline(h=median(weightAt20) - sd(weightAt20), col="red")

# -----------------------------------------------------------------------------
# MELT AND CAST (aka Pivot tables) 
# -----------------------------------------------------------------------------

# FIRST Melt (normalize?) the data frame so it can be recast 
# before that, standardize the column names all lower caps
names(ChickWeight) <- tolower(names(ChickWeight))

chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)
message("melted dataset - head:")
print(head(chick_m))
#   time chick diet variable value
# 1    0     1    1   weight    42
# 2    2     1    1   weight    51
# 3    4     1    1   weight    59
# 4    6     1    1   weight    64
# 5    8     1    1   weight    76
# 6   10     1    1   weight    93

# Now cast (Aggregate?) the melted dataset into various aggregated tables
#
chick_c <- cast(chick_m, time ~ variable, mean) # average effect of time
print(chick_c)
#    time    weight
# 1     0  41.06000
# 2     2  49.22000
# 3     4  59.95918
# 4     6  74.30612
# 5     8  91.24490
# 6    10 107.83673
# 7    12 129.24490
# 8    14 143.81250
# 9    16 168.08511
# 10   18 190.19149
# 11   20 209.71739
# 12   21 218.68889
chick_c <- cast(chick_m, diet ~ variable, mean) # average effect of diet
print(chick_c)

chick_c <- cast(chick_m, diet ~ time ~ variable, mean) # average effect of diet & time
print(chick_c)
#     time
# diet    0     2        4        6         8        10       12       14       16       18       20       21
#    1 41.4 47.25 56.47368 66.78947  79.68421  93.05263 108.5263 123.3889 144.6471 158.9412 170.4118 177.7500
#    2 40.7 49.40 59.80000 75.40000  91.70000 108.50000 131.3000 141.9000 164.7000 187.7000 205.6000 214.7000
#    3 40.8 50.40 62.20000 77.90000  98.40000 117.10000 144.4000 164.5000 197.4000 233.1000 258.9000 270.3000
#    4 41.0 51.80 64.50000 83.90000 105.60000 126.00000 151.4000 161.8000 182.0000 202.9000 233.8889 238.5556


# How many chicks at each time? - checking for balance
chick_c <- cast(chick_m, time ~ diet, length)
chick_c <- cast(chick_m, chick ~ time, mean)
chick_c <- cast(chick_m, chick ~ time, mean, subset=time < 10 & chick < 20)

chick_c <- cast(chick_m, diet + chick ~ time)
chick_c <- cast(chick_m, chick ~ time ~ diet)
chick_c <- cast(chick_m, diet + chick ~ time, mean, margins="diet")


message("done")
