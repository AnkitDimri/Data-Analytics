library (tabulizer)
library (dplyr)
library (tidyr)

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

# Print the tidy data frame
df

vital = df [, 1:4]
vital$`2` = as.numeric(vital$`2`)

m_birth = mean (vital$`2`)
m_birth
