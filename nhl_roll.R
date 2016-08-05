library(ggplot2)

nhl_seas <- read.csv(file = "nhl/nhl_20152016_res.csv", sep=",", header = TRUE)
colnames(nhl_seas)
nhl_seas$Date <- as.Date(nhl_seas$Date, "%Y-%m-%d")
nhl_seas$Notes <- NULL
nhl_seas$LOG <- NULL
nhl_seas$Att. <- NULL
nhl_seas$vis_win <- ifelse(nhl_seas$Home_Goals < nhl_seas$Visitor_Goals, 1, 0)
nhl_seas$home_win <- ifelse(nhl_seas$vis_win == 0, 1, 0)


nyr_seas <- nhl_seas[nhl_seas$Visitor=="New York Rangers" |
                       nhl_seas$Home == "New York Rangers",]

nyr_seas$win <- ifelse((nyr_seas$Visitor == "New York Rangers" & 
                          nyr_seas$vis_win ==1) |
                         (nyr_seas$Home == "New York Rangers" & 
                          nyr_seas$home_win==1), 1,0) 

roll_avg_funct <- function(df_col, ind, lookback=5) {
  sum(df_col[(ind-lookback):ind]) / lookback
}

nyr_seas$roll_avg_5 <- NA
nyr_seas$roll_avg_10 <- NA
nyr_seas$roll_avg_15 <- NA
nyr_seas$roll_avg_20 <- NA
for (i in 5:length(nyr_seas$win)) {
  nyr_seas$roll_avg_5[i] <- roll_avg_funct(nyr_seas$win, i)
}
for (i in 10:length(nyr_seas$win)) {
  nyr_seas$roll_avg_10[i] <- roll_avg_funct(nyr_seas$win, i, lookback = 10)
}

for (i in 15:length(nyr_seas$win)) {
  nyr_seas$roll_avg_15[i] <- roll_avg_funct(nyr_seas$win, i, lookback = 15)
}

for (i in 20:length(nyr_seas$win)) {
  nyr_seas$roll_avg_20[i] <- roll_avg_funct(nyr_seas$win, i, lookback = 20)
}

rownames(nyr_seas) <- 1:nrow(nyr_seas)

nyr_seas$g_number <- 1:nrow(nyr_seas)

qplot(Date, roll_avg_20, data=nyr_seas) + geom_smooth()