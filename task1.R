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

barplot(t(as.matrix(vital)), beside=TRUE)

ggplot(vital, aes(x = c("live.births", "still.birth", "deaths"), fill = "year" )) +
  geom_bar(position = position_dodge()) +
  theme_classic()

m_birth = mean (vital$`2`)
m_birth
