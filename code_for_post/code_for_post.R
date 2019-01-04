##################################################
# Starter and Bench Majority Data Analysis
# Author: Thomas Bassine
# Date: 1/2/19
# Purpose: Analyze starter and bench data for each 
# team in 2018-19. This code produces the plots
# used in my article.
###################################################

# 1) Read in Data (from github):
x <- read.csv('https://raw.githubusercontent.com/tvbassine/nba_bench_fun/master/starter_bench_units_data/units_2018_2019_through_12_30.csv',
              stringsAsFactors = F)

# Get ggplot functions:
library('ggplot2')


###################################################
# 2) Basic Questions:

# a) What % of time did each # of starters play?

agg_by_time <- aggregate(time_total ~ sum_start, data = x, sum)
agg_by_time$prop <- agg_by_time$time_total / sum(agg_by_time$time_total)
sum(agg_by_time$prop[4:6]) #total proportion of starter majority
                           #minutes

# b) How has each lineup composition do overall?

net_by_start <- aggregate(net_pts ~ sum_start, data = x, sum)

###################################################
# 3) Plotting time played with each starter compostion by team:

# a) Which teams played the most starter heavy and bench heavy lineups?

time_by_team <- aggregate(time_total ~ sum_start + team, data = x, sum)

# Change to proportion of time rather than raw seconds:

for(j in unique(time_by_team$team)){
  rows <- which(time_by_team$team == j)
  time_by_team$time_total[rows] <- time_by_team$time_total[rows] / 
                                  sum(time_by_team$time_total[rows])
}

# Get average time as well (already computed in agg_by_time):
agg2 <- data.frame(sum_start = 0:5,
                   team = 'Average',
                   time_total = agg_by_time$prop)

time_by_team <- rbind(time_by_team, agg2)

# b) Code for making pretty plot with ggplot:

# Create a variable for the color scheme:
time_by_team$Team = 'Other'
time_by_team$Team[time_by_team$team %in%
                    c('Philadelphia 76ers',
                      'Toronto Raptors',
                      'Miami Heat',
                      'Milwaukee Bucks',
                      'Average')] <- time_by_team$team[time_by_team$team %in%
                                                                 c('Philadelphia 76ers',
                                                                   'Toronto Raptors',
                                                                   'Miami Heat',
                                                                   'Milwaukee Bucks',
                                                                   'Average')]
# Plotted With ggplot:

ggplot2::ggplot(data=time_by_team, 
                aes(x=sum_start, y=time_total, group=team)) +
geom_line(aes(group=team, colour = Team)) +
scale_color_manual(
         values = c(
        `Philadelphia 76ers`="blue",
        `Toronto Raptors` = "orange",
        `Miami Heat`= "red",
        `Milwaukee Bucks` = "green",
        `Average` = "black",
        `Other`="grey")) +
labs(title = "Teams Vary In How Often They Deploy \nDifferent Numbers of Starters", 
        subtitle = "Through Games Played on Dec. 30th",
        x = 'Total Number of Starters On Court',
        y = 'Proportion of Total Minutes',
        caption = 'Source: basketball-reference.com') +
theme(plot.title = element_text(hjust = 0.5, size = 16))

# Find Sixers % of time with 2 or more starters:
sum(time_by_team$time_total[time_by_team$Team == 'Philadelphia 76ers' &
                            time_by_team$sum_start >= 2]) /
sum(time_by_team$time_total[time_by_team$Team == 'Philadelphia 76ers'])

# Find Bucks % of time with 2 or more starters:
sum(time_by_team$time_total[time_by_team$Team == 'Milwaukee Bucks' &
                            time_by_team$sum_start >= 2]) /
sum(time_by_team$time_total[time_by_team$Team == 'Milwaukee Bucks'])

###################################################
# 4) Plotting time played with each starter compostion by team:

# a) Which teams had the most successful starters and benches?

z <- aggregate(net_pts ~ team + sum_start, data = x, sum)

# Get number of games played for each team so we can convert
# net points into net points per game:
for(j in unique(z$team)){
  len <- length(unique(x$id[x$team == j]))
  rows <- which(z$team == j)
  
  z$net_pts[rows] = z$net_pts[rows] / len
}

# Get average value of net points per game for
# each number of starters on court:
z2 <- data.frame(team = 'Average',
                 sum_start = 0:5,
                 net_pts = 0)

for(i in 1:nrow(z2)){
  z2$net_pts[i] <- mean(z$net_pts[z$sum_start == 
                                  z2$sum_start[i]]
                        )
}

z <- rbind(z, z2)

# Code for plotting net points per game by starters with 
# ggplot:

# Make a variable to aid in the colororing of the teams
# in the plot:
z$Team = 'Other'
z$Team[z$team %in%
        c('Philadelphia 76ers',
          'Toronto Raptors',
          'San Antonio Spurs',
          'Milwaukee Bucks',
          'Average')] <- z$team[z$team %in%
                                            c('Philadelphia 76ers',
                                              'Toronto Raptors',
                                              'San Antonio Spurs',
                                              'Milwaukee Bucks',
                                              'Average')]
# PLOT with ggplot:

ggplot2::ggplot(data=z, 
                aes(x=sum_start, y=net_pts)) +
geom_line(aes(group=team, colour = Team)) +
scale_color_manual(
          values = c(
          `Philadelphia 76ers`="blue",
          `Toronto Raptors` = "orange",
          `San Antonio Spurs`= "red",
          `Milwaukee Bucks` = "green",
          `Average` = "black",
          `Other`="grey")) +
labs(title = "How Effective Is Each Team When \nPlaying A Given Number Of Starters?", 
          subtitle = "Check out the Bucks and Raptors!",
          x = 'Total Number of Starters On Court',
          y = 'Net Points (Team - Opponent) Per Game',
          caption = 'Source: basketball-reference.com') +
theme(plot.title = element_text(hjust = 0.5, size = 16))

# Interesting. Can pictorally view the importance of starters. 
# (Especially 5 starter units)
# Hello Bucks and 3 starter lineups!
# Hello Raptors and 5 starter lineups!
# Spurs also stand out by having a solid bench but bad 
# starter majority lineups.

###################################################
# 5) 
# Get starter majority and bench majority net points per 
# game and proportion of total minutes for each team.
# Then write this to a csv so I can make a google sheet
# that contains this info.

# Initialize dataframe:
w <- data.frame(team = unique(z$team),
                start = 0,  # starter net pts. per game
                bench = 0,  # bench net pts. per game
                start_min = 0, # proportion of min. playing starter maj.
                bench_min = 0, # prop. of min. playing bench maj.
                stringsAsFactors = F)
for(i in 1:30){
  w$start[i] <- sum(z$net_pts[z$team == w$team[i] &
                              z$sum_start > 2])
  w$bench[i] <- sum(z$net_pts[z$team == w$team[i] &
                              z$sum_start <= 2])
  w$start_min[i] <- sum(time_by_team$time_total[
                        time_by_team$team == w$team[i] & 
                        time_by_team$sum_start > 2])
  w$bench_min[i] <- sum(time_by_team$time_total[
                        time_by_team$team == w$team[i] & 
                        time_by_team$sum_start <= 2])
}

# I set the working directory to my desktop folder, 
# but user can change to wherever they want:
# setwd("~/Desktop/Threes and Layups Articles/How Should We Interpret Net Ratings?")
colnames(w) <- c('Team', 'Starter Net Pts Per Game',
                 'Bench Net Pts Per Game',
                 'Starter Prop. of Mins',
                 'Bench Prop. of Mins')
# write.csv(w, 'Summary_Through_Dec_30.csv', row.names = F)

###################################################
# 6) Plot to see distribution of starter and bench net points.
# Particularly interested in the spread.

par(mar=c(5.1,6.1,4.1,5.1))

plot( rep(1,30) ,  w$`Starter Net Pts Per Game`[1:30],
      xlim = c(0.75,1.5),
      xaxt = 'n',
      yaxt = 'n', pch = 19,
      xlab = '',
      ylab = 'Net Points Per Game',
      main = 'Starter Majority Net Points Per Game \nHas Larger Spread')
points( rep(1.25,30), w$`Bench Net Pts Per Game`[1:30], 
        pch = 19)
axis(1, at = c(1,1.25), c('Starters', 'Bench'), las = 2,
     cex = .75)
axis(2, at = seq(-7,9), las = 2,
     cex = .75)
segments(y0 = w$`Starter Net Pts Per Game`[order(w$`Starter Net Pts Per Game`)[7]],
         x0 = .95,
         y1 = w$`Starter Net Pts Per Game`[order(w$`Starter Net Pts Per Game`)[25]],
         x1 = .95,
         lwd = 2)
segments(y0 = w$`Bench Net Pts Per Game`[order(w$`Bench Net Pts Per Game`)[7]],
         x0 = 1.3,
         y1 = w$`Bench Net Pts Per Game`[order(w$`Bench Net Pts Per Game`)[25]],
         x1 = 1.3,
         lwd = 2)
# Top and Bottom of Bars:
segments(y0 = w$`Starter Net Pts Per Game`[order(w$`Starter Net Pts Per Game`)[7]],
         x0 = .9,
         y1 = w$`Starter Net Pts Per Game`[order(w$`Starter Net Pts Per Game`)[7]],
         x1 = 1,
         lwd = 2)
segments(y0 = w$`Starter Net Pts Per Game`[order(w$`Starter Net Pts Per Game`)[25]],
         x0 = .9,
         y1 = w$`Starter Net Pts Per Game`[order(w$`Starter Net Pts Per Game`)[25]],
         x1 = 1,
         lwd = 2)
segments(y0 = w$`Bench Net Pts Per Game`[order(w$`Bench Net Pts Per Game`)[7]],
         x0 = 1.25,
         y1 = w$`Bench Net Pts Per Game`[order(w$`Bench Net Pts Per Game`)[7]],
         x1 = 1.35,
         lwd = 2)
segments(y0 = w$`Bench Net Pts Per Game`[order(w$`Bench Net Pts Per Game`)[25]],
         x0 = 1.25,
         y1 = w$`Bench Net Pts Per Game`[order(w$`Bench Net Pts Per Game`)[25]],
         x1 = 1.35,
         lwd = 2)
text(1.3, 6, 'Gap Between \n7th Best and \n7th Worst', cex = .75)

# Arrows:
graphics::arrows(x0 = 1.25, y0 = 4.5, x1 = .95, y1 = 1,
                 length = .1,
                 lwd = 2)
graphics::arrows(x0 = 1.35, y0 = 4.5, x1 = 1.35, y1 = 1.2,
                 length = .1,
                 lwd = 2)

