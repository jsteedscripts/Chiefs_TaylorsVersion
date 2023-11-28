library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(caret)
library(ggplot2)
library(dplyr)
library(rpart)
library(caret)
library(pROC)
library(gbm)
library(corrplot)
library(flextable)
library(officer)
library(magrittr)

##### Load initial data #####

# NFL data from the past 10 seasons
past_10 <- load_pbp(2012:2022)

# KC data from the past 10 seasons
kc <- subset(past_10, home_team=="KC" | away_team=="KC")


##### Create D_outcome dataset we'll use to build models #####

# Create dataset we'll use to predict from
D_outcome <- kc[ , c("game_id", "week", "game_date", "home_team", "away_team", 
                        "home_score", "away_score")]
D_outcome <- distinct(D_outcome)

# Create year variable
D_outcome$year <- as.numeric(str_sub(D_outcome$game_date, 1, 4))

# Replace df with the actual name of your data frame
D_outcome <- D_outcome %>% filter(year != 2023)

# Create kc_home variable (yes/no)
D_outcome$kc_home <- ifelse(D_outcome$home_team=="KC", "yes", "no")

# Create kc_win variable (yes/no)
D_outcome$kc_win <- NA
for (i in 1:nrow(D_outcome)) {
  if(D_outcome$home_team[i]=="KC" & D_outcome$home_score[i]>=D_outcome$away_score[i]){
    D_outcome$kc_win[i] <- "yes"
  } 
  
  if(D_outcome$home_team[i]=="KC" & D_outcome$home_score[i]<D_outcome$away_score[i]){
    D_outcome$kc_win[i] <- "no"
  } 
  
  if(D_outcome$home_team[i]!="KC" & D_outcome$home_score[i]<D_outcome$away_score[i]){
    D_outcome$kc_win[i] <- "yes"
  } 
  
  if(D_outcome$home_team[i]!="KC" & D_outcome$home_score[i]>D_outcome$away_score[i]){
    D_outcome$kc_win[i] <- "no"
  } 
}

# Create numeric variable kc_score
D_outcome$kc_score <- 0
for (i in 1:nrow(D_outcome)) {
  if(D_outcome$home_team[i]=="KC"){
    D_outcome$kc_score[i] <- D_outcome$home_score[i]
  } else {
    D_outcome$kc_score[i] <- D_outcome$away_score[i]
  }
}



##### Calculate some offensive measures for each game #####
# Grab only plays where KC is on offense
kc_off <- subset(kc, posteam=="KC")

### Predictor 1: Average kick distance for a game ###
D_kc_avg_kick_dist <- kc_off[ , c("game_id", "kick_distance")]
D_kc_avg_kick_dist <- na.omit(D_kc_avg_kick_dist)

D_kc_avg_kick_dist <- group_by(D_kc_avg_kick_dist, game_id)
D_kc_avg_kick_dist <- mutate(D_kc_avg_kick_dist, kc_avg_kick_dist=mean(kick_distance))
D_kc_avg_kick_dist <- distinct(D_kc_avg_kick_dist, game_id, kc_avg_kick_dist)

D_outcome <- left_join(D_outcome, D_kc_avg_kick_dist,
                       by=c("game_id"="game_id"))


### Predictor 2: Passing efficiency ###
D_kc_passing_efficiency <- kc_off[, c("game_id", "pass_attempt", "complete_pass")]
D_kc_passing_efficiency <- na.omit(D_kc_passing_efficiency)

D_kc_passing_efficiency <- group_by(D_kc_passing_efficiency, game_id) %>%
  summarise(kc_pass_efficiency = sum(complete_pass) / sum(pass_attempt))

D_outcome <- left_join(D_outcome, D_kc_passing_efficiency, by = c("game_id" = "game_id"))


### Predictor 3: Number of offensive penalties in a game ###
D_penalty <- kc_off[, c("game_id", "penalty", "penalty_team")]
D_kc_penalties <- subset(D_penalty, penalty_team=="KC")

D_kc_penalties <- group_by(D_kc_penalties, game_id) %>%
  summarise(kc_num_offensive_penalties = sum(penalty))

D_outcome <- left_join(D_outcome, D_kc_penalties, by = c("game_id" = "game_id"))
D_outcome$kc_num_offensive_penalties <- ifelse(is.na(D_outcome$kc_num_offensive_penalties), 0, D_outcome$kc_num_offensive_penalties)


### Predictor 4: Avg yards gained by offense ###
D_kc_avg_yds_gained <- kc_off[, c("game_id", "yards_gained", "play_id")]
D_kc_avg_yds_gained[is.na(D_kc_avg_yds_gained)] <- 0

D_kc_avg_yds_gained <- group_by(D_kc_avg_yds_gained, game_id) %>%
  summarise(kc_avg_yards_gained = mean(yards_gained, na.rm = TRUE))

D_outcome <- left_join(D_outcome, D_kc_avg_yds_gained, by = c("game_id" = "game_id"))


### Predictor 5: Average passing yards per game ###
D_kc_avg_passing_yds <- kc_off[, c("game_id", "pass_attempt", "passing_yards")]
D_kc_avg_passing_yds <- na.omit(D_kc_avg_passing_yds)

D_kc_avg_passing_yds <- group_by(D_kc_avg_passing_yds, game_id)
D_kc_avg_passing_yds <- mutate(D_kc_avg_passing_yds, kc_avg_passing_yds=mean(passing_yards))
D_kc_avg_passing_yds <- distinct(D_kc_avg_passing_yds, game_id, kc_avg_passing_yds)

D_outcome <- left_join(D_outcome, D_kc_avg_passing_yds,
                       by=c("game_id"="game_id"))


### Predictor 6: Average rushing yards per game ###
D_kc_avg_rushing_yds <- kc_off[, c("game_id", "pass_attempt", "rushing_yards")]
D_kc_avg_rushing_yds <- na.omit(D_kc_avg_rushing_yds)

D_kc_avg_rushing_yds <- group_by(D_kc_avg_rushing_yds, game_id)
D_kc_avg_rushing_yds <- mutate(D_kc_avg_rushing_yds, kc_avg_rushing_yds=mean(rushing_yards))
D_kc_avg_rushing_yds <- distinct(D_kc_avg_rushing_yds, game_id, kc_avg_rushing_yds)

D_outcome <- left_join(D_outcome, D_kc_avg_rushing_yds,
                       by=c("game_id"="game_id"))


##### Calculate some defensive measures for each game #####
# Grab only plays where KC is on defense
kc_def <- subset(kc, posteam!="KC")

### Predictor 7: Average sacks ###
D_kc_avg_sacks <- kc_def[ , c("game_id", "sack")]
D_kc_avg_sacks <- na.omit(D_kc_avg_sacks)

D_kc_avg_sacks <- group_by(D_kc_avg_sacks, game_id) %>%
  summarise(kc_num_avg_sacks = mean(sack))

D_outcome <- left_join(D_outcome, D_kc_avg_sacks,
                       by=c("game_id"="game_id"))

### Predictor 8: Average yards gained by other team (yards allowed) ###
D_yards_allowed <- kc_def[, c("game_id", "yards_gained")]
D_yards_allowed <- na.omit(D_yards_allowed) 
kc_avg_yards_allowed <- aggregate(D_yards_allowed$yards_gained, 
                               by = list(D_yards_allowed$game_id), mean)$x

D_outcome <- left_join(D_outcome, data.frame(game_id = unique(kc_def$game_id), kc_avg_yards_allowed), by = "game_id")

### Predictor 9: Average number of interceptions
D_kc_avg_interceptions <- kc_def[ , c("game_id", "interception")]
D_kc_avg_interceptions <- na.omit(D_kc_avg_interceptions)

D_kc_avg_interceptions <- group_by(D_kc_avg_interceptions, game_id) %>%
  summarise(kc_num_avg_interceptions = mean(interception))

D_outcome <- left_join(D_outcome, D_kc_avg_interceptions,
                       by=c("game_id"="game_id"))

### Predictor 10: Opponent passing efficiency ###
D_opp_passing_efficiency <- kc_def[, c("game_id", "pass_attempt", "complete_pass")]
D_opp_passing_efficiency <- na.omit(D_opp_passing_efficiency)

D_opp_passing_efficiency <- group_by(D_opp_passing_efficiency, game_id) %>%
  summarise(opp_pass_efficiency = sum(complete_pass) / sum(pass_attempt))

D_outcome <- left_join(D_outcome, D_opp_passing_efficiency, by = c("game_id" = "game_id"))

### Predictor 11: Average QB_hits by kc ###
D_kc_avg_qb_hits <- kc_def[ , c("game_id", "qb_hit")]
D_kc_avg_qb_hits <- na.omit(D_kc_avg_qb_hits)

D_kc_avg_qb_hits <- group_by(D_kc_avg_qb_hits, game_id) %>%
  summarise(kc_avg_qb_hits = mean(qb_hit))

D_outcome <- left_join(D_outcome, D_kc_avg_qb_hits,
                       by=c("game_id"="game_id"))


##### Plots #####
# KC final scores over time
ggplot(D_outcome, aes(x = year, y = kc_score)) +
  geom_line() +
  labs(title = "KC Scores Over Time", x = "Year", y = "KC Score")

# correlation heatmap of offensive predictors
kc_offensive_predictors <- D_outcome %>%
  select(kc_avg_kick_dist, kc_pass_efficiency, kc_num_offensive_penalties, kc_avg_yards_gained, kc_avg_passing_yds, kc_avg_rushing_yds)

corr_matrix_offensive <- cor(kc_offensive_predictors)
corrplot(corr_matrix_offensive, method = "color")

# correlation heatmap of defensive predictors
kc_defensive_predictors <- D_outcome %>%
  select(kc_num_avg_sacks, kc_avg_qb_hits, opp_pass_efficiency, kc_num_avg_interceptions, kc_avg_yards_allowed)

corr_matrix_defensive <- cor(kc_defensive_predictors)
corrplot(corr_matrix_defensive, method = "color")

# correlation heatmap of all predictors
kc_all_predictors <- D_outcome %>%
  select(kc_avg_kick_dist, kc_pass_efficiency, kc_num_offensive_penalties, kc_avg_yards_gained, kc_avg_passing_yds, kc_avg_rushing_yds,
         kc_num_avg_sacks, kc_avg_qb_hits, opp_pass_efficiency, kc_num_avg_interceptions, kc_avg_yards_allowed)

corr_matrix_all <- cor(kc_all_predictors)
corrplot(corr_matrix_all, method = "color")

# Scatter plot of yards allowed vs opp passing efficiency
ggplot(D_outcome, aes(x = kc_avg_yards_allowed, y = opp_pass_efficiency)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Defensive Yards Allowed vs. Opponent's Passing Efficiency", x = "Average Yards Allowed", y = "Opponent Passing Efficiency")

# Scatter plot of yards gained vs passing efficiency
ggplot(D_outcome, aes(x = kc_avg_yards_gained, y = kc_pass_efficiency)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Offensive Yards Gained vs. KC's Passing Efficiency", x = "Average Yards Gained", y = "KC Passing Efficiency")

# KC play type distributions
ggplot(kc, aes(x = play_type, fill = play_type)) +
  geom_bar() +
  labs(title = "Distribution of KC Play Types",
       x = "Play Type",
       y = "Count",
       fill = "Play Type") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# KC win-loss analysis by season
ggplot(D_outcome, aes(x = factor(year), fill = kc_win)) +
  geom_bar(position = "stack", color = "white") +
  labs(title = "Win-Loss Analysis by Season",
       x = "Season",
       y = "Count",
       fill = "Win") +
  scale_fill_manual(values = c("yes" = "yellow", "no" = "red")) +  # Specify colors for wins and losses
  theme_minimal()

# Scatter plot of yards allowed vs average number of sacks
ggplot(D_outcome, aes(x = kc_avg_yards_allowed, y = kc_num_avg_sacks)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Defensive Yards Allowed vs. Average Number of Sacks", x = "Average Yards Allowed", y = "Average Number of Sacks")


##### Training/Holdout Split #####
D_Train <- subset(D_outcome, year<=2017)
D_Test <- subset(D_outcome, year>2017)

##### Models #####

### Regularized Linear Regression ###
set.seed(2023)
fitControl <- trainControl(method="cv", number=5)

glmnetGrid <- expand.grid(alpha = seq(0, 1, by=0.1),
                          lambda = 10^seq(-3,-0.5,by=.5))

GLMNET <- train(kc_score~kc_avg_kick_dist + kc_pass_efficiency + kc_num_offensive_penalties + kc_avg_yards_gained
                + kc_avg_passing_yds + kc_avg_rushing_yds + kc_num_avg_sacks + kc_avg_qb_hits + opp_pass_efficiency
                + kc_num_avg_interceptions + kc_avg_yards_allowed, data=D_Train,
                method="glmnet",
                tuneGrid=glmnetGrid,
                trControl=fitControl,
                preProc=c("center","scale"))

best_glmnet <- GLMNET$results[rownames(GLMNET$bestTune),]

### Random Forest ###
set.seed(2023)
fitControl <- trainControl(method="cv", number=5)

forestGrid <- expand.grid(mtry=c(2,4))

RF <- train(kc_score~kc_avg_kick_dist + kc_pass_efficiency + kc_num_offensive_penalties + kc_avg_yards_gained
            + kc_avg_passing_yds + kc_avg_rushing_yds + kc_num_avg_sacks + kc_avg_qb_hits + opp_pass_efficiency
            + kc_num_avg_interceptions + kc_avg_yards_allowed, data=D_Train, method="rf",
            tuneGrid=forestGrid,
            trControl=fitControl,
            preProc=c("center","scale"))

best_rf <- RF$results[rownames(RF$bestTune),]

### GBM ###
set.seed(2023)
fitControl <- trainControl(method="cv", number=5)

gbmGrid <- expand.grid(n.trees=c(1000),
                       interaction.depth=c(1,2),
                       shrinkage=c(0.005, 0.01),
                       n.minobsinnode=c(10))

GBMfit <-train(kc_score~kc_avg_kick_dist + kc_pass_efficiency + kc_num_offensive_penalties + kc_avg_yards_gained
               + kc_avg_passing_yds + kc_avg_rushing_yds + kc_num_avg_sacks + kc_avg_qb_hits + opp_pass_efficiency
               + kc_num_avg_interceptions + kc_avg_yards_allowed, data=D_Train, method="gbm",
               tuneGrid=gbmGrid,
               trControl=fitControl,
               preProc=c("center","scale"),
               verbose=FALSE)

best_gbm <- GBMfit$results[rownames(GBMfit$bestTune),]

### Model Comparison Table ###
model_names <- c("Regularized Linear Regression", "Random Forest", "GBM")
model_results <- bind_rows(
  data.frame(Model = "Regularized Linear Regression", RSquared = best_glmnet$Rsquared, RSquaredSD = best_glmnet$RsquaredSD,
             RMSE = best_glmnet$RMSE, RMSESD = best_glmnet$RMSESD),
  data.frame(Model = "Random Forest", RSquared = best_rf$Rsquared, RSquaredSD = best_rf$RsquaredSD, RMSE = best_rf$RMSE,
             RMSESD = best_rf$RMSESD),
  data.frame(Model = "GBM", RSquared = best_gbm$Rsquared, RSquaredSD = best_gbm$RsquaredSD, RMSE = best_gbm$RMSE,
             RMSESD = best_gbm$RMSESD)
)

ft <- flextable(model_results) %>%
  theme_zebra() %>%  
  color(j = "Model", color = "black") %>%  
  align(j = "RSquared", align = "center") %>%  
  align(j = "RMSE", align = "center") %>%  
  set_table_properties(width = .7, layout = "autofit") 
ft <- theme_zebra(ft, odd_header = "#CFCFCF", odd_body = "red", even_header = "transparent", even_body = "yellow")
ft

# 1 sd rule calculation for the top 2 models - 2 lowest RMSE's (GLMNET and GBM)
one_sd_calculation <- (best_gbm$RMSE - best_glmnet$RMSE)/ best_gbm$RMSESD
one_sd_calculation

# Neither is statistically better than the other (they are within 1 sd of each other), so we choose the simplest (GLMNET)

##### Holdout Predictions #####
# Predict using test data on GLM
D_Test$predict_kc_score <- predict(GLMNET, D_Test)

plot(D_Test$kc_score, D_Test$predict_kc_score,
     xlim=c(5,35), ylim=c(5,35), pch=15)
abline(a=0,b=1)

# Holdout RMSE
RMSE <- sqrt(mean((D_Test$kc_score - D_Test$predict_kc_score)^2))
RMSE
mean(D_Test$kc_score)
RMSE / mean(D_Test$kc_score)

# Residual scatter plot
residuals <- D_Test$kc_score - D_Test$predict_kc_score
plot(D_Test$predict_kc_score, residuals, pch=15, xlab="Predicted Scores", ylab="Residuals")
abline(h=0, col="red")

# Density plot of residuals
density_res <- density(residuals)
plot(density_res, main="Density Plot of Residuals", xlab="Residuals")

# QQplot of residuals
qqnorm(residuals)
qqline(residuals)

# Get coefficients
coefficients_glmnet_sparse <- coef(GLMNET$finalModel, s = best_glmnet$lambda)
coefficients_glmnet <- as.data.frame(as.matrix(coefficients_glmnet_sparse))

coefficients_glmnet$Predictor <- rownames(coefficients_glmnet)
coefficients_glmnet <- coefficients_glmnet[order(coefficients_glmnet$s1, decreasing = TRUE), ]

ft_coefficients_glmnet <- flextable(coefficients_glmnet) %>%
  set_table_properties(width = .7, layout = "autofit") %>%
  theme_zebra()

ft_coefficients_glmnet <- theme_zebra(ft_coefficients_glmnet, odd_header = "#CFCFCF", odd_body = "red", even_header = "transparent", even_body = "yellow")
ft_coefficients_glmnet

# Get feature importance scores
importance_glmnet <- varImp(GLMNET)

importance_df <- as.data.frame(importance_glmnet$importance)

importance_df$Variable <- rownames(importance_df)
importance_df <- importance_df[order(importance_df$Overall, decreasing = TRUE), ]

ft_importance_glmnet <- flextable(importance_df) %>%
  set_table_properties(width = .7, layout = "autofit") %>%
  theme_zebra()

ft_importance_glmnet <- theme_zebra(ft_importance_glmnet, odd_header = "#CFCFCF", odd_body = "red", even_header = "transparent", even_body = "yellow")
ft_importance_glmnet





