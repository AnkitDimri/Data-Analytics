library (tabulizer)
library (dplyr)
library (tidyr)
library (ggplot2)
library (stringr)

tab = extract_tables (file = "~/ankit/Github/Data-Analytics/datasets/task1/CRS-2016.pdf")

# Display table 6
tab [[6]]

# Display data for years 2011 to 16
tab [[6]][17:22, ]

# Put this in data frame
df = as.data.frame (tab [[6]] [17:22, ])
# Print data frame
df

# Tidy the data by seperating V2, V3 and V4 into seperate columns
df = separate(df, V2, into = c("2", "3", "4"), sep = ' ')
df = separate(df, V3, into = c("5", "6"), sep = ' ')
df = separate(df, V4, into = c("7", "8"), sep = ' ')

# Rename columns
colnames (df) [colnames(df) == "V1"] <- "year"
colnames (df) [colnames(df) == "2"] <- "live.births"
colnames (df) [colnames(df) == "3"] <- "still.birth"
colnames (df) [colnames(df) == "4"] <- "deaths"
colnames (df) [colnames(df) == "5"] <- "crs.births"
colnames (df) [colnames(df) == "6"] <- "crs.deaths"
colnames (df) [colnames(df) == "7"] <- "srs.births"
colnames (df) [colnames(df) == "8"] <- "srs.deaths"

# Print the tidy data frame
df

vital = df [, 1:4]
vital$year = vital$year 
vital$live.births = as.numeric(vital$live.births) /1000
vital$still.birth = as.numeric(vital$still.birth) /10
vital$deaths = as.numeric(vital$deaths) / 400

# Changes for plot facilitation

y = c()
for (year in vital$year) {
  y = c(y, rep (c(year), 3))
}

class = rep (c("live.births", "still.birth", "deaths"), 6)

val = c()
for (i in 1:length (vital [,1])) {
  val = c(val, c(vital$live.births[i], vital$still.birth[i], vital$deaths[i]))
}

data = data.frame (y, class, val)

# Plot 1: Vital statistics
ggplot(data, aes(fill=class, y=val, x=factor (y))) + geom_bar(position="dodge", stat="identity") + xlab ("Years") + ylab ("Value") + ggtitle ("Vital events: Birth, Still Birth and Deaths registered in the year 2011-16") + scale_fill_discrete (labels=c("Deaths (x400)", "Live Birth (x1000)", "Still Birth (x10)"))


# Districtwise registered birth and deaths
dist = tab [[10]]
dist

# Clean-up the data
dist = dist [9:38,]
dist

dist = as.data.frame (dist)
dist

dst = str_extract(dist$V2, '[^[:digit:]]+')
val = str_remove_all (dist$V2, "[[A-Z]\\(\\)]")
val = str_remove (val, "[:space:]+")

dist = data.frame (District = dst, v = val)

dist = separate(dist, v, into = c("Birth", "Birth.rate", "Death", "Death.rate", "Infant.death", "Still.Birth", "Still.Birth.rate"), sep = '[:blank:]+')

dist$Birth = as.numeric (dist$Birth)
dist$Birth.rate = as.numeric (dist$Birth.rate)
dist$Death = as.numeric (dist$Death)
dist$Death.rate = as.numeric (dist$Death.rate)
dist$Infant.death = as.numeric (dist$Infant.death)
dist$Still.Birth = as.numeric (dist$Still.Birth)
dist$Still.Birth.rate = as.numeric (dist$Still.Birth.rate)

dist

rates = data.frame (District = dist$District, Birth.rate = as.numeric(dist$Birth.rate), Death.rate = as.numeric(dist$Death.rate), Still.Birth.rate = as.numeric(dist$Still.Birth.rate))
rates

# rate data for plotting

di = c()
for (dis in rates$District) {
   di = c(di, rep (c(dis), 3))
}

class = rep (c("Birth.rate", "Death.rate", "Still.Birth.rate"), 30)

val = c()
for (i in 1:length (vital [,1])) {
  val = c(val, c(rates$Birth.rate[i] / 2, rates$Death.rate[i], rates$Still.Birth.rate[i]))
}

dis_data = data.frame (di, class, val)



ggplot(dis_data, aes(fill=class, y=val, x=factor (di))) + geom_bar(position="dodge", stat="identity") + xlab ("Districts") + ylab ("rates") + ggtitle ("Birth, Death and Still Birth rates in Urban areas of districts\nKarnataka 2016") + scale_fill_discrete (labels=c("Birth rate (x2)", "Death rate", "Still Birth rate")) + coord_flip()

boxplot (rates$Birth.rate, rates$Death.rate, rates$Still.Birth.rate, names = c("Birth rate", "Death rate", "Still Birth rate"), ylab = "Rates", main = "Rate boxplot (No outliers for any rate\ndepicting no differnece in rate across Urban\nareas in all districts)")
# No outliers in the plots, therefore the rates are similar accross all districts


# Finding outliers
br_out = boxplot (dist$Still.Birth.rate)$out
br_out

which (br_out %in% dist$Birth.rate)


pie (dist$Birth, labels = dist$District, col=rainbow(length(dist$District)), main = "Birth contribution accross Districts in Urban areas-2016", radius = 0.9, cex = 0.7, font = 2, init.angle = 46)
pie (dist$Death, labels = dist$District, col=rainbow(length(dist$District)), main = "Death contribution accross Districts in Urban areas-2016", radius = 0.9, cex = 0.7, font = 2, init.angle = 40)



####################
# Basic Statistics #
####################

# District with minimum birth rate 2016
rates [which (rates$Birth.rate == min (rates$Birth.rate)),c(1,2)]

# District with maximum birth rate 2016
rates [which (rates$Birth.rate == max (rates$Birth.rate)),c(1,2)]

# Mean of birth rate 2016
print (mean (rates$Birth.rate))

# Mean birth accross districts 2016
print (as.integer (mean (dist$Birth)))

# Median of birth rate accross districts 2016
print (median (rates$Birth.rate))

# varience of birth rate accross districts
print (var (rates$Birth.rate))

# Standard deviation of birth rate accross districts 2016
print (sd (rates$Birth.rate))

# Total summary of rates accross districts in 2016
summary (rates)
