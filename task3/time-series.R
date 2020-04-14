# import the dataset
im = read.csv2("imports.csv", sep = ",")
im

# Make time series variable out of it
im = ts (im)
plot (im [[, 2:ncol (im)]])

# Plotting the yearly mean values
mean = list ()
for (i in (1:ceiling (nrow (im) /  12)))
  mean [[i]] = c (i)

for (i in (1:ceiling (nrow (im) /  12))) {
  m = rep (0, ncol (im) - 1)
  for (j in 1:12) {
    if (i+j > nrow (im)) 
      break
    
    for (k in 2:ncol (im))
      m [k-1] = m [k-1] + im [i+j, ] [k]

  }
  m = m / 12
  mean [[i]] = c (mean [[i]], m)
}
mean


df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames (df) = c ("years", "MeanAirportsCIF", "MeanParcelPostCIF", "MeanSeaportsCIF", "MeanAirportsWeight", "MeanParcelPostWeight", "MeanSeaportsWeight")

for (i in (1:ceiling (nrow (im) /  12)))
  df [nrow (df) + 1, ] = c (mean [[i]])

mean = ts (df [, 2:ncol (df)])
plot (mean)
