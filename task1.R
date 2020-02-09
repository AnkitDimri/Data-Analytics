library (tabulizer)
library (dplyr)
library (tidyr)
library (ggplot2)

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
vital$year = as.numeric(vital$year)
vital$live.births = as.numeric(vital$live.births)
vital$still.birth = as.numeric(vital$still.birth)
vital$deaths = as.numeric(vital$deaths)

# Changes for plot facilitation

y = c()
for (year in vital$year) {
  y = c(y, rep (c(year), 3))
}

class = rep (c("live.births", "still.death", "deaths"), 6)

val = c()
for (i in 1:length (vital [,1])) {
  val = c(val, c(vital$live.births[i], vital$still.birth[i], vital$deaths[i]))
}

data = data.frame (y, class, val)

ggplot(data, aes(fill=class, y=val, x=y)) + geom_bar(position="dodge", stat="identity")