# ElectricTouch_Down

## Project Overview
This project utilizes the nflfastR package to analyze Kansas City Chiefs data from the past ten seasons and build three different predictive models.

## Data
This project utilizes the [nflfastr](https://www.nflfastr.com/) package.

The following plot shows the Chiefs win-loss statistics by season. Their number of wins has been trending upward, and with the exception of 2012, the Chiefs typically win far more games than they lose each season.

![Win Loss](plots/win_loss_by_season.png)

The next plot shows the distribution of the play types by the Chiefs over the past 10 seasons.

![Play Type](plots/play_type_hist.png)

## Predictors
Offensive and defensive measures wer calculated and used to build models to generate predictions of the Chiefs final score per game.

### Offensive Measures
* Average kick distance
* Passing efficiency
* Number of offensive penalties
* Average yards gained
* Average passing yards
* Average rushing yards

### Defensive Measures
* Average sacks
* Average yards allowed
* Average number of interceptions
* Opponent's passing efficiency
* Average QB hits
