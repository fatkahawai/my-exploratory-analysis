# ----------------------------------------------------------------------------
#
# Example - Exploring And Visualising data using ggplot2
#
# the dataset is in the learningr package - from the "Learning R" O'Reilly book.
#
# 2016
# ----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
readkey <- function()
{
    cat ("Press [enter] to continue")
    line <- readline()
}
# -----------------------------------------------------------------------------

library(ggplot2)

data(obama_vs_mccain, package = "learningr")

message("head:")

print(head(obama_vs_mccain))
#        State Region Obama McCain Turnout Unemployment Income Population Catholic
# 1    Alabama     IV 38.74  60.32    61.6          5.9  22732    4802982        6
# 2     Alaska      X 37.89  59.42    68.0          6.6  29382     721523       NA
# 3    Arizona     IX 44.91  53.39    55.7          7.1  25203    6412700       29
#   Protestant Other Non.religious Black Latino Urbanization
# 1         80     1            11  26.2    3.9         94.4
# 2         NA    NA            NA   3.3    5.5          1.2
# 3         44     5            17   4.1   16.3         56.3

message("summary:")

print(summary(obama_vs_mccain))
#        State        Region       Obama           McCain         Turnout     
# Alabama   : 1   IV     : 8   Min.   :32.54   Min.   : 6.53   Min.   :50.80  
# Alaska    : 1   I      : 6   1st Qu.:42.76   1st Qu.:40.39   1st Qu.:61.05  
# Arizona   : 1   III    : 6   Median :51.38   Median :46.80   Median :64.90  
# Arkansas  : 1   V      : 6   Mean   :51.29   Mean   :47.00   Mean   :64.09  
# California: 1   VIII   : 6   3rd Qu.:57.34   3rd Qu.:55.88   3rd Qu.:67.95  
# Colorado  : 1   VI     : 5   Max.   :92.46   Max.   :65.65   Max.   :78.00  
# (Other)   :45   (Other):14                                   NA's   :4      
#  Unemployment      Income        Population          Catholic    
# Min.   :3.40   Min.   :19534   Min.   :  563626   Min.   : 6.00  
# 1st Qu.:5.05   1st Qu.:23501   1st Qu.: 1702662   1st Qu.:12.00  
# Median :5.90   Median :25203   Median : 4350606   Median :21.00  
# Mean   :6.01   Mean   :26580   Mean   : 6074128   Mean   :21.67  
# 3rd Qu.:7.25   3rd Qu.:28978   3rd Qu.: 6656506   3rd Qu.:29.00  
# Max.   :9.40   Max.   :40846   Max.   :37341989   Max.   :46.00  
#                                                   NA's   :2      
#   Protestant        Other       Non.religious       Black          Latino     
# Min.   :26.00   Min.   :0.000   Min.   : 5.00   Min.   : 0.4   Min.   : 1.20  
# 1st Qu.:46.00   1st Qu.:2.000   1st Qu.:12.00   1st Qu.: 3.1   1st Qu.: 4.30  
# Median :54.00   Median :3.000   Median :15.00   Median : 7.4   Median : 8.20  
# Mean   :53.84   Mean   :3.286   Mean   :16.04   Mean   :11.1   Mean   :10.32  
# 3rd Qu.:62.00   3rd Qu.:4.000   3rd Qu.:19.00   3rd Qu.:15.2   3rd Qu.:12.05  
# Max.   :80.00   Max.   :8.000   Max.   :34.00   Max.   :50.7   Max.   :46.30  
# NA's   :2       NA's   :2       NA's   :2                                     
#  Urbanization   
# Min.   :   1.2  
# 1st Qu.:  45.8  
# Median : 101.2  
# Mean   : 385.6  
# 3rd Qu.: 221.4  
# Max.   :9856.5 

obama <- obama_vs_mccain$Obama

# one column
print(mean(obama))
print(median(obama))

# cut the obama column data into decile buckets 
buckets <- cut(obama, seq.int(0, 100, 10))

message("levels:")
print(levels(buckets))
# [1] "(0,10]"   "(10,20]"  "(20,30]"  "(30,40]"  "(40,50]"  "(50,60]"  "(60,70]" 
# [8] "(70,80]"  "(80,90]"  "(90,100]"

# how many rows fall into each bucket ?
message("table: how many rows fall into each bucket ?")
print(table(buckets))
#  (0,10]  (10,20]  (20,30]  (30,40]  (40,50]  (50,60]  (60,70]  (70,80]  (80,90] 
#       0        0        0        8       16       16        9        1        0 

message("quantile:")
print(quantile(obama))
#     0%    25%    50%    75%   100% 
# 32.540 42.755 51.380 57.335 92.460 

# look if any correlation between columns ( -1 is no correl at all, 1 is perfect correl)
# this means the same as cor(obama_vs_mccain$Obama, obama_vs_mccain$McCain))
message("correlation: Obama / Mccain vote")
print(with(obama_vs_mccain, cor(Obama, McCain)))
# [1] -0.9981189 almost perfect lack of correlation obviously 

message("correlation: Black, Latino vote:")
print(with(obama_vs_mccain, cor(Black, Latino)))
#[1] -0.1009063 not really no

# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

gg1 <- ggplot(obama_vs_mccain, aes(Income, Turnout)) +
  geom_point() 
message("ggplot: Income vs Turnout")
print(gg1)
readkey()


gg1 <- ggplot(obama_vs_mccain, aes(Income, Turnout)) +
  geom_point(color = "violet", shape = 20) 
print(gg1)
readkey()


gg1 <- ggplot(obama_vs_mccain, aes(Income, Turnout)) +
  geom_point() +
  scale_x_log10(breaks = seq(2e4, 4e4, 1e4)) +
  scale_y_log10(breaks = seq(50, 75, 5))
message("ggplot: with log scale")
print(gg1)
readkey()

gg1 <- ggplot(obama_vs_mccain, aes(Income, Turnout)) +
  geom_point() +
  scale_x_log10(breaks = seq(2e4, 4e4, 1e4)) +
  scale_y_log10(breaks = seq(50, 75, 5)) +
  facet_wrap(~ Region, ncol = 5) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
message("ggplot: as grid of plots with theme")
print(gg1)
readkey()

(gg1 <- ggplot(obama_vs_mccain, aes(Income, Turnout)) + 
  geom_point()
)

(gg2 <- gg1 +
  facet_wrap(~ Region, ncol = 5)
)
print(gg2)
readkey()

# ----------------------------------------------------------------------------
# Line Plots
# ----------------------------------------------------------------------------

message("Line plots")
with(
  crab_tag$daylog,
  plot(Date, -Max.Depth, type = "l", ylim = c(-max(Max.Depth), 0))
)
readkey()

with(
  crab_tag$daylog,
  lines(Date, -Min.Depth, col = "blue")
)
readkey()


plot(-Min.Depth + -Max.Depth ~ Date, crab_tag$daylog, type = "l")
readkey()

# GGPLOT -----------------------------------------------------------------------

message("ggplot: Line plots")
gg1 <- ggplot(crab_tag$daylog, aes(Date, -Min.Depth)) +
  geom_line() 
print(gg1)
readkey()

gg1 <- ggplot(crab_tag$daylog, aes(Date)) +
  geom_line(aes(y = -Max.Depth)) +
  geom_line(aes(y = -Min.Depth))
print(gg1)
readkey()

library(reshape2)
crab_long <- melt(
  crab_tag$daylog, 
  id.vars      = "Date", 
  measure.vars = c("Min.Depth", "Max.Depth")
)
gg1 <- ggplot(crab_long, aes(Date, -value, group = variable)) +
  geom_line()
print(gg1)
readkey()

gg1 <- ggplot(crab_tag$daylog, aes(Date, ymin = -Min.Depth, ymax = -Max.Depth)) +
  geom_ribbon(color = "black", fill = "white") 
message("ggplot: ")
print(gg1)
readkey()

# ----------------------------------------------------------------------------
# Histograms
# ----------------------------------------------------------------------------
message("Histogram plots")

with(obama_vs_mccain, hist(Obama))

with(obama_vs_mccain, 
  hist(Obama, 4, main = "An exact number of bins")
)
readkey()

with(obama_vs_mccain, 
  hist(Obama, seq.int(0, 100, 5), main = "A vector of bin edges")
)
readkey()

with(obama_vs_mccain, 
  hist(Obama, "FD", main = "The name of a method")
)
readkey()

with(obama_vs_mccain, 
  hist(Obama, nclass.scott, main = "A function for the number of bins")
)
readkey()

binner <- function(x) 
{
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 50)
}
with(obama_vs_mccain, 
  hist(Obama, binner, main = "A function for the bin edges")
)
readkey()

with(obama_vs_mccain, hist(Obama, freq = FALSE))
readkey()

# GGPLOT ----------------------------------------------------------------------------

message("ggplot: Histogram plots")

gg1 <- ggplot(obama_vs_mccain, aes(Obama)) +
  geom_histogram(binwidth = 5)
print(gg1)
readkey()

gg1 <- ggplot(obama_vs_mccain, aes(Obama, ..density..)) +
  geom_histogram(binwidth = 5)
print(gg1)
readkey()

# ----------------------------------------------------------------------------
# Boxplots
# ----------------------------------------------------------------------------


boxplot(Obama ~ Region, data = obama_vs_mccain)

ovm <- within(
  obama_vs_mccain, 
  Region <- reorder(Region, Obama, median)
)
boxplot(Obama ~ Region, data = ovm)
message("Box plots")
print(gg1)
readkey()


# GGPLOT -----------------------------------------------------------------------

gg1 <- ggplot(ovm, aes(Region, Obama)) +
  geom_boxplot()
message("ggplot: Box plots")
print(gg1)
readkey()

# ----------------------------------------------------------------------------
# Barcharts
# ----------------------------------------------------------------------------
message("Bar plots")

ovm <- ovm[!(ovm$State %in% c("Alaska", "Hawaii")), ]

par(las = 1, mar = c(3, 9, 1, 1))
with(ovm, barplot(Catholic, names.arg = State, horiz = TRUE))
readkey()

religions <- with(ovm, rbind(Catholic, Protestant, Non.religious, Other))
colnames(religions) <- ovm$State
par(las = 1, mar = c(3, 9, 1, 1))
barplot(religions, horiz = TRUE, beside = FALSE)
readkey()

barchart(State ~ Catholic, ovm)
readkey()

barchart(
  State ~ Catholic + Protestant + Non.religious + Other, 
  ovm, 
  stack = TRUE
)
readkey()

# GGPLOT -----------------------------------------------------------------------
# melt changes the data frame from wide to long or vice versa
message("ggplot: Bar plots")

religions_long <- melt(
  ovm, 
  id.vars = "State",
  measure.vars = c("Catholic", "Protestant", "Non.religious", "Other")
)

gg1 <- ggplot(religions_long, aes(State, value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip()
print(gg1)
readkey()

gg1 <- ggplot(religions_long, aes(State, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()
print(gg1)
readkey()

# ----------------------------------------------------------------------------
