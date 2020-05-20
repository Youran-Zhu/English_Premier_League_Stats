# English_Premier_League_Stats

## Data Source: 

   The dataset is collected from here: https://www.kaggle.com/idoyo92/epl-stats-20192020

   It is originally a merge of two English Premier League (EPL) dataset by the author from: 
   https://github.com/vaastav/Fantasy-Premier-League and https://datahub.io/sports-data/english-premier-league


## Response Variable and Predictors:

   There are 576 (n=576) rows in the dataset. Each row is a summary of an EPL game from one team's perspective. 

   There are 44 features in total. I used 37 numerical variables and 2 categorical variable **h_a** & **matchday**. With 
   one-hot encoding the categorical variable, there are p = 42 in my analysis. A list of the variables and the available 
   meaning is attached in the Appendix.

   I will use variable "**scored**" as response variable, which is goals scored in each match for one team. In other 
   words, we are working to predict how many goals each team would score in one match.  


## Data Preprocess:

   Luckily there is no missing data in this dataset.
   For numerical data, I standardized them to have a Mean of 0, and a standard deviation of 1 before the analysis.



## Appendix: 

| Variable Name | Meaning                                  |
|---------------|------------------------------------------|
| h_a           | home or away                             |
| xG            | expected goals                           |
| xGA           | xG index for opposite team               |
| npxG          | expected goals without penalties         |
| npxGA         | same as above for opposite team          |
| deep          |                                          |
| deep_allowed  |                                          |
| scored        | goals scored                             |
| missed        | goals conceded                           |
| xpts          | expected points                          |
| result        | win, draw or lose                        |
| date          | date of the match                        |
| wins          | binary for wins                          |
| draws         | binary for draws                         |
| loses         | binary for loses                         |
| pts           | points earnd                             |
| npxGD         |                                          |
| teamId        | team                                     |
| ppda_cal      | measure of pressing play.                |
| allowed_ppda  | same as above for opposite team          |
| matchtime     | the hour the match took place            |
| tot_points    | total point the team managed so far      |
| round         | Matchday number                          |
| tot_goal      | total goals team has scored so far       |
| tot_con       | same as above for conceded               |
| Referee.x     | ref name                                 |
| HS.x          | home team shots                          |
| HST.x         | home shots on target                     |
| HF.x          | home fouls                               |
| HC.x          | home corners                             |
| HY.x          | home yellow card                         |
| HR.x          | home red cards                           |
| AS.x          | away shots                               |
| AST.x         | away shots on target                     |
| AF.x          | away fouls                               |
| AC.x          | away corners                             |
| AY.x          | away yellow cards                        |
| AR.x          | away red cards                           |
| B365H.x       | B365 odd for home win                    |
| B365D.x       | B365 odd for draw                        |
| B365A.x       | B365 odd for away win                    |
| HtrgPerc      | shot on target/total shots - home        |
| AtrgPerc      | same as above for away                   |
| matchDay      | the day of the week the match took place |
