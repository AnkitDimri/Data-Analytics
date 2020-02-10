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
dist

rates = data.frame (District = dist$District, Birth.rate = dist$Birth.rate, Death.rate = dist$Death.rate, Still.Birth.rate = dist$Still.Birth.rate)
rates
