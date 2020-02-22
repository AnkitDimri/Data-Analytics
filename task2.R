library (ggplot2)

deliveries = read.csv2 ("datasets/task2/deliveries 1.csv", sep = ",")
deliveries = as.data.frame (deliveries)
deliveries

summary (deliveries)

deliveries$over = as.numeric (deliveries$over)
deliveries$total_runs = as.numeric (deliveries$total_runs)
deliveries$total_runs

overs = unique (deliveries$over)
total_runs = c ()
for (over in overs) {
  runs = deliveries [deliveries$over %in% over,"total_runs"]
  total_runs = c(total_runs, sum (runs))
}

total_runs
run_over = data.frame (over = overs, runs = total_runs)

ggplot(run_over, aes(over,runs), las=2) + geom_line(color = "green")
