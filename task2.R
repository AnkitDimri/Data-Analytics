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

ggplot(run_over, aes(over,runs), las=2) + geom_line(color = "green") + labs (title = "Runs scored per over in the IPL season 2019", x = "Over", y = "Runs")


wickets = deliveries[which ( deliveries$player_dismissed != ""), c("over", "player_dismissed")]
wickets


w = table (wickets$over)
w

plot (w, ylab = "Wickets", xlab = "Over", main = "Wickets taken per over in the IPL season 2019")


team = deliveries [, c ("batting_team", "total_runs")]
teams = unique (team$batting_team)
teams


tms = c()
rns = c()
for (t in teams) {
  tms = c(tms, t)
  rns = c(rns, sum (team [which (team$batting_team == t), "total_runs"]))
}

team_runs = data.frame (teams = tms, runs = rns)
team_runs

ggplot(team_runs, aes(x = factor (teams), y = runs), las=2) + geom_bar(color = "green") + labs (title = "Runs scored per over in the IPL season 2019", x = "Over", y = "Runs")

par (mar = c (5,8.5,5,2))
barplot (height = team_runs$runs, horiz = T , names.arg = factor (team_runs$teams), las = 1, cex.names = 0.7)

