#NFL DFS Optimization

setwd("C:/Users/gelmashni/Dropbox/2019 Fantasy Football/NFL DFS Analysis/Week 8")

devtools::install_github("zamorarr/coach")

library(coach)

require(xlsx)
data <- read.xlsx(file="NFLDaily.xlsx",sheetName = "Week8FD")
print(data)

str(data)
head(data)
names(data)
summary(data)

data$row_id <- as.integer(data$row_id)
data$player_id <- as.character(data$player_id)
data$player <- as.character(data$player)
data$team <- as.character(data$team)
data$position <- as.character(data$position)
data$salary <- as.integer(data$salary)

set.seed(100)
n <- nrow(data)
data$fpts_proj <- rnorm(n, data$fpts_avg)
print(data)

model <- model_fd_nfl(data)

scores <- optimize_generic(data, model, L = 20)
scores

write.csv(scores, file = "NFL_DK_Predictions.csv")