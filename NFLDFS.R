#NFL DFS Analysis

setwd("C:/Users/gelmashni/Dropbox/2018 Fantasy Football/NFL Data (Big Data Ball)")

#Analysis of Offense
require(xlsx)
data <- read.xlsx(file="NFLDailyRB.xlsx",sheetName = "Sheet1")

#NFL Analysis
str(data)
head(data)
names(data)
summary(data)

data$logDK_Salary <- log(data$DK_Salary)
data$SqrtDK_Salary <- sqrt(data$DK_Salary)

par(mfrow=c(2,3))
hist(data$logDK_Salary, col = "navy", xlab = "Log DK Salary", main = "Histogram of Log DraftKings Salary", ylim = c(0,2000))
boxplot(data$logDK_Salary, col = "navy", main = "Boxplot of Log DraftKings Salary")
hist(data$SqrtDK_Salary, col = "navy", xlab = "Square Root DK Salary", main = "Histogram of Square Root DraftKings Salary", ylim = c(0,2000))
boxplot(data$SqrtDK_Salary, col = "navy", main = "Boxplot of Square Root DraftKings Salary")
hist(data$DK_Salary, col = "gold", xlab = "DK Salary", main = "Histogram of DraftKings Salary", ylim = c(0,2000))
boxplot(data$DK_Salary, col = "gold", main = "Boxplot of DraftKings Salary")

data$total <- data$FINAL + data$PtsAllowed


nflsub1 <- subset(data, select = c(SqrtDK_Salary, DK_Score, FINAL, Point_Diff, X1stDowns, RunPct, CompPct, PassPct,
                                      TOP, TOP_Pct, Turnovers, TO_Diff, SACKED, X3DPct, CloseSpread, CloseTotal,
                                      PtsAllowed, total, Starter, Venue, Opponent, SnapCount, SnapPct), na.rm = TRUE)

nflsub2 <- subset(nflsub1, select = c(SqrtDK_Salary, DK_Score, FINAL, Point_Diff, X1stDowns, RunPct, CompPct, PassPct,
                                   TOP, TOP_Pct, Turnovers, TO_Diff, SACKED, X3DPct, CloseSpread, CloseTotal,
                                   PtsAllowed, total, Starter, Venue, Opponent, SnapCount, SnapPct), na.rm = TRUE)

nflsub3 <- subset(nflsub2, select = c(SqrtDK_Salary, DK_Score, FINAL, Point_Diff, X1stDowns, RunPct, CompPct, PassPct,
                                      TOP_Pct, Turnovers, TO_Diff, SACKED, X3DPct, CloseSpread, CloseTotal,
                                      PtsAllowed, total, Venue, Opponent, SnapCount, SnapPct), na.rm = TRUE)

nflsub4 <- subset(nflsub3, select = c(SqrtDK_Salary, DK_Score, FINAL, X1stDowns, RunPct, CompPct, PassPct,
                                      SACKED, X3DPct, CloseSpread, CloseTotal,
                                      total, Venue, Opponent, SnapCount, SnapPct), na.rm = TRUE)

nflnumeric1 <- subset(data, select = c(SqrtDK_Salary, DK_Score, FINAL, Point_Diff, X1stDowns, RunPct, CompPct, PassPct,
                                   TOP, TOP_Pct, Turnovers, TO_Diff, SACKED, X3DPct, CloseSpread, CloseTotal,
                                   PtsAllowed, total, SnapCount, SnapPct), na.rm = TRUE)




c <- cor(nflnumeric1)
par(mfrow = c(1,1))
corrplot(c, method = "square")

#####################################################
################### DraftKings Models ###############
#####################################################

##############################################################

# Define the upper model as the FULL model
upper.nfl <- lm(DK_Score ~ .,data=nflsub4);
summary(upper.nfl)

# Define the lower model as the Intercept model
lower.nfl <- lm(DK_Score ~ 1,data=nflsub4);
summary(lower.nfl)

# Call stepAIC() for variable selection

# Forward Model

forward.nfl.lm <- stepAIC(object=lower.nfl,scope=list(upper=upper.nfl,lower=lower.nfl),
                          direction=c('forward'));
summary(forward.nfl.lm)

vif(forward.nfl.lm)
sqrt(vif(forward.nfl.lm)) > 2

# Backward Model

backward.nfl.lm <- stepAIC(object=upper.nfl,direction=c('backward'));
summary(backward.nfl.lm)

vif(backward.nfl.lm)
sqrt(vif(backward.nfl.lm)) > 2

# Need a SLR to initialize stepwise selection
nfl.slr.lm <- lm(DK_Score ~ SqrtDK_Salary,data=nflsub4);
summary(nfl.slr.lm)

# Stepwise Model

stepwise.nfl.lm <- stepAIC(object=nfl.slr.lm,scope=list(upper=formula(upper.nfl),lower=~1),
                           direction=c('both'));
summary(stepwise.nfl.lm)

vif(stepwise.nfl.lm)
sqrt(vif(stepwise.nfl.lm)) > 2

# Judge models

AIC(forward.nfl.lm)
AIC(backward.nfl.lm)
AIC(stepwise.nfl.lm)

# Final Model

best.nfl.stepwise <- stepwise.nfl.lm

library(modelr)
data.frame(
  R2 = rsquare(best.nfl.stepwise, data = nflsub1),
  RMSE = rmse(best.nfl.stepwise, data = nflsub1),
  MAE = mae(best.nfl.stepwise, data = nflsub1)
)

library(broom)
glance(best.nfl.stepwise)

anova(best.nfl.stepwise)
summary(best.nfl.stepwise)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(best.nfl.stepwise)

# Influential Points

par(mfrow=c(1,1))
influencePlot(best.nfl.stepwise,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

cook <- cooks.distance(best.nfl.stepwise)
nflsub3$cook <- round(cook, 6)

################ influential points removed  #######
cooks_cutoff_nfl <- 1/1000

nflsub3 <- nflsub3[which(nflsub3$cook < cooks_cutoff_nfl),]

nflsub3
