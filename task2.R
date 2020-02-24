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
barplot (height = team_runs$runs, horiz = T , names.arg = factor (team_runs$teams), las = 1, cex.names = 0.7, col = "blue4", xlim = c (0, 3000))








players_runs = deliveries [, c ("batsman", "batsman_runs")]
players = unique (players_runs$batsman)
players

plyr = c()
rn = c()
for (p in players) {
  plyr = c(plyr, p)
  rn = c(rn, sum (players_runs [which (players_runs$batsman == p), "batsman_runs"]))
}

player_runs = data.frame (player = plyr, runs = rn)
player_runs

player_runs = player_runs [order (player_runs$runs,decreasing = T),]
player_runs = player_runs [1:10,]

par (mar = c (5,4.5,5,2))
barplot (height = player_runs$runs, horiz = T , names.arg = factor (player_runs$player), las = 1, cex.names = 0.7, col = "blue4", xlim = c (0, 800))






baller = read.csv("baller.csv")
baller

#RANKING BAllERS

baller ['WKTS/MATCHES'] = baller ['WKTS']/baller ['MATCHES']
baller ['ECONOMY'] = baller ['RUNS']/(baller ['BALLS']/6)

w1 = 5
w2 = -3

baller ['Score'] = w1 * baller ['WKTS/MATCHES'] + w2 * baller ['ECONOMY']
baller ['Score'] = baller ['Score'] + min (baller$Score) * (-1)
baller = baller [order (baller$Score, decreasing = TRUE), ]
baller = baller [1:10, ]
baller = baller [10:1, ]

par (mar = c (5,6.5,5,2))
barplot (height = baller$ECONOMY, horiz = T , names.arg = factor (baller$PLAYER), las = 1, cex.names = 0.7, col = "blue4", xlim = c (0, 10))

#RANKING BATSMAN

bt = read.csv("batsman.csv")
bt = bt [, c("PLAYER", "INN", "RUNS", "AVG", "SR", "X4S", "X6S")]
bt
bt ['RUN/INN'] = bt ['RUNS']/bt['INN']
bt



w1 = 7
w2 = 4
w3 = 5
w4 = 1
bt ['Score'] = w1 * bt$`RUN/INN` + w2 * bt$X4S +w3 * bt$X6S + w4 * bt$SR
bt = bt [order (bt$Score, decreasing = TRUE),]
bt
cd = data.frame (bt [1:10, ])
cd = cd [10:1, ]
cd
# plotting top 10 batsman
par (mar = c (5,6.5,5,2))
barplot (height = cd$AVG, horiz = T , names.arg = factor (cd$PLAYER), las = 1, cex.names = 0.7, col = "blue4", xlim = c (0, 100))
